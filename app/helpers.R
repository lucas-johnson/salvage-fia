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
            istree = TRUE,
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
            FIESTA::spGetXY(bnd = sf::as_Spatial(query_polygon),
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
    return(coords)
}

get_affected_inventory <- function(db_file, plot_ids) {
    
    # TODO: FIESTA calls go here
    
    return(data.frame(
        FORTYP = c("Loblolly Pine", "Mixed upland hardwoods", "Swamp chestnut oak / cherrybark oak"),
        VOLCFNET = c(100, 200, 150),
        BA = c(10, 20, 15)
    ))
}

render_query_results <- function(results) {
    table <- results |> 
        dplyr::relocate(FORTYP, VOLCFNET, BA) |>
        dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2))) 
    names(table) <- c("Forest type", "Net Volume (Cubic Feet)", "Basal Area (ft^2)")
    table |> 
        kbl(booktabs = TRUE, align = c("l", rep("r", 2)),
            linesep = "\\addlinespace", format = 'html') |>
        kableExtra::kable_styling(position = 'center')
    
}