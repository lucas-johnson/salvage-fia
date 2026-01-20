clean_dir <- function(clean_dir = here::here("temp_dir")) {
    lapply(list.files(clean_dir, full.names = TRUE), unlink)
}


execute_query <- function(states, query_polygon, log_file) {
    log_connection <- file(log_file, 'w')
    sink(file = file(log_file, 'w'), type = c('message'), append = FALSE)
    db_file <- download_db(states)
    plot_ids <- get_affected_plot_ids(db_file, query_polygon)
    results <- get_affected_inventory(db_file, plot_ids)
    sink()
    close(log_connection)
    return(results)
}

download_db <- function(states) {
    
    file <- glue::glue("{paste0(states$STUSPS, collapse = '_')}.db")
    write_dir <- here::here("temp_dir")
    dir.create(write_dir, showWarnings = FALSE)
    
    filename <- file.path(write_dir, file)
    
    if (!file.exists(filename)) {
        FIESTA::DBgetPlots(
            states = states |> dplyr::pull(NAME),
            datsource = "datamart",
            eval = "FIA",
            eval_opts = list(Cur = TRUE, Type = "VOL"),
            savePOP = TRUE,
            returndata = FALSE,
            savedata = TRUE,
            savedata_opts = list(out_fmt = "sqlite",
                                 outfolder = write_dir,
                                 out_dsn = file))
    }
    return(filename)
}

get_affected_plot_ids <- function(db_file, query_polygon) {
    # TODO: FIESTA calls go here...
    coords <- tryCatch(
        {
            FIESTA::spGetXY(bnd = sf::st_as_sf(query_polygon),
                            xy_datsource = "sqlite",
                            xy_dsn = db_file,
                            eval = "FIA",
                            eval_opts = list(Cur = TRUE, Type = "VOL"))
        },
        error = function(cond) {
            # TODO: Do something here to display that the polygon is bad...
            message(conditionMessage(cond))
            NA
        }
    )
    return(coords$pltids)
}

get_affected_inventory <- function(db_file, plot_ids) {
    conn <- DBI::dbConnect(RSQLite::SQLite(), db_file)
    # unit_opts = FIESTA::unit_options(unitvar2 = "EVALID")
    pop_data <- FIESTA::modGBpop(
        popTabs = FIESTA::popTables(plt = "plot", cond = "cond", tree = "tree"),
        dsn = db_file,
        datsource = "sqlite",
        pltassgn = "pop_plot_stratum_assgn",
        stratalut = "pop_stratum",
        unitarea = "pop_estn_unit",
        defaultVars = FALSE,
        unitvar = "ESTN_UNIT",
        areavar = "AREA_USED",
        unit_opts = FIESTA::unit_options(unitvar2 = "EVALID"),
        strata_opts = FIESTAutils::strata_options(getwt = TRUE,
                                                  getwtvar = "P1POINTCNT")
    )
    
    volcfnet <- FIESTA::modGBtree(GBpopdat = pop_data, 
                                  landarea = "FOREST",
                                  sumunits = TRUE, # combines estimation units
                                  estvar = "VOLCFNET",
                                  estvar.filter = "STATUSCD == 1", # live trees
                                  returntitle = TRUE,
                                  rowvar = 'FORTYPGRPCD',
                                  colvar = 'STDSZCD', 
                                  table_opts = FIESTAutils::table_options(
                                      row.FIAname = TRUE, 
                                      col.FIAname = TRUE
                                  ), 
                                  pltids = plot_ids$PLT_CN)
    
    
    balive <- FIESTA::modGBtree(GBpopdat = pop_data, 
                                landarea = "FOREST",
                                sumunits = TRUE, # combines estimation units
                                estvar = "BA",
                                estvar.filter = "STATUSCD == 1", # live trees
                                returntitle = TRUE, 
                                rowvar = 'FORTYPGRPCD',
                                colvar = 'STDSZCD', 
                                table_opts = FIESTAutils::table_options(
                                    row.FIAname = TRUE, 
                                    col.FIAname = TRUE
                                ),
                                pltids = plot_ids$PLT_CN)
    
    DBI::dbDisconnect(conn)
    return(list(vol = volcfnet, ba = balive))   
}

pretty_results_table <- function(group_tab, total_tab, group_col) {
    
    dplyr::bind_rows(
        group_tab,
        total_tab |> 
            dplyr::select(-TOTAL) |>
            dplyr::mutate({{group_col}} := 'Total')
    ) |> 
        dplyr::select({{group_col}}, est, NBRPLT.gt0, est.se, pse) |> 
        dplyr::rename(Estimate = est, `n Plots` = NBRPLT.gt0,  SE = est.se, `SE (%)` = pse) |> 
        dplyr::mutate(
            dplyr::across(
                dplyr::where(is.numeric), 
                ~ (function(x) {
                    pretty_x <- prettyNum(round(.x, 2), big.mark = ",", scientific = FALSE)
                    return(
                        ifelse(pretty_x == "NA", "--", pretty_x)
                    )
                })(.x)
            )
        ) |>
        dplyr::mutate({{group_col}} := gsub("group ", "", {{group_col}}))
}

kablify_results_tables <- function(table) {
    table |> 
        kableExtra::kbl(booktabs = TRUE, align = c("l", rep("r", 4)),
        linesep = "\\addlinespace", format = 'html') |>
        kableExtra::kable_styling(position = 'center') |> 
        kableExtra::row_spec(
            row = nrow(table),
            bold = TRUE
        ) |> 
        kableExtra::row_spec(
            row = nrow(table) - 1,
            hline_after = TRUE 
        )
}

render_query_results <- function(results) {
    fortypgrp_tab <- pretty_results_table(results$raw$rowest, 
                                          results$raw$totest, 
                                          `Forest-type group`) |> 
        kablify_results_tables()
    stdszcd_tab <- pretty_results_table(results$raw$colest, 
                                        results$raw$totest, 
                                        `Stand-size class`) |> 
        kablify_results_tables()
    return(paste(fortypgrp_tab, stdszcd_tab, sep = "\n"))
}
