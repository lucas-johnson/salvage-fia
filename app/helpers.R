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

get_fiesta_est <- function(pop_data, estvar, plot_ids, mode, landarea = "FOREST", sumunits = TRUE, 
                           estvar.filter = "STATUSCD == 1", returntitle = TRUE, 
                           rowvar = NULL, table_opts = NULL) {
    if (is.null(table_opts)) {
        table_opts <- FIESTAutils::table_options(row.FIAname = TRUE)
    }
    
    args = list(GBpopdat = pop_data, 
                pltids = plot_ids$PLT_CN, 
                landarea = landarea, 
                sumunits = sumunits, 
                returntitle = returntitle, 
                rowvar = rowvar, 
                table_opts = table_opts)
    
    if (mode == 'area') {
        estvar <- "Area (acres)"
        result <- tryCatch(
            {
                do.call(FIESTA::modGBarea, args)
            }, error = function(cond) {
                message(conditionMessage(cond))
                return(list())
            }
        )
        
    } else if(mode == 'tree') {
        result <- tryCatch(
            {
                do.call(
                    FIESTA::modGBtree, 
                    c(list(estvar = estvar, estvar.filter = estvar.filter), args)
                )
            }, error = function(cond) {
                message(conditionMessage(cond))
                return(list())
            }
        )
    } else {
        stop("mode not supported! Use 'tree' or 'area'.")
    }
    
    return(c(result, list(estvar = estvar)))
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
    
    forest_area <- list(
        c(
            get_fiesta_est(pop_data, NULL, plot_ids, 'area', 
                           landarea = "ALL", rowvar = 'COND_STATUS_CD'),
            list(filter = 'None', grouping = 'COND_STATUS_CD')
        )
    )
    tree_numbers <- list(
        c(
            get_fiesta_est(pop_data, 'TPA_UNADJ', plot_ids, 'tree',
                           estvar.filter = 'STATUSCD == 1 & DIA >= 5', 
                           rowvar = 'SPGRPCD'),
            list(filter = "live; >= 5 in DBH", grouping = 'Species group')
        ),
        c(
            get_fiesta_est(pop_data, 'TPA_UNADJ', plot_ids, 'tree', 
                           estvar.filter = 'STATUSCD == 1', 
                           rowvar = 'SPGRPCD'),
            list(filter = 'live; >= 1 in DBH', grouping = 'Species group')
        ),
        c(
            get_fiesta_est(pop_data, 'TPA_UNADJ', plot_ids, 'tree', 
                           estvar.filter = 'STATUSCD == 0', 
                           rowvar = 'SPGRPCD'),
            list(filter = 'dead; >= 1 in DBH', grouping = 'Species group')
        ),
        c(
            get_fiesta_est(pop_data, 'TPA_UNADJ', plot_ids, 'tree',
                           estvar.filter = 'STATUSCD == 1 & DIA >= 5', 
                           rowvar = 'OWNGRPCD'),
            list(filter = "live; >= 5 in DBH", grouping = 'Owner group')
        ),
        c(
            get_fiesta_est(pop_data, 'TPA_UNADJ', plot_ids, 'tree', 
                           estvar.filter = 'STATUSCD == 1', 
                           rowvar = 'OWNGRPCD'),
            list(filter = 'live; >= 1 in DBH', grouping = 'Owner group')
        ),
        c(
            get_fiesta_est(pop_data, 'TPA_UNADJ', plot_ids, 'tree', 
                           estvar.filter = 'STATUSCD == 0', 
                           rowvar = 'OWNGRPCD'),
            list(filter = 'dead; >= 1 in DBH', grouping = 'Owner group')
        )
    )
    
    
    # Total stem sound volume wood and bark: VOLTSSND
    # Sound bole wood volume: VOLCFSND
    volume <- list(
        c(
            get_fiesta_est(pop_data, 'VOLTSSND', plot_ids, 'tree', 
                           estvar.filter = 'STATUSCD == 1 & DIA >= 5',
                           rowvar = 'SPCD'),
            list(filter = 'live; >= 5 in DBH', grouping = 'Species')
        ),
        c(
            get_fiesta_est(pop_data, 'VOLTSSND', plot_ids, 'tree', 
                           estvar.filter = 'STATUSCD == 0 & DIA >= 5',
                           rowvar = 'SPCD'),
            list(filter = 'dead; >= 5 in DBH', grouping = 'Species')
        ),
        c(
            get_fiesta_est(pop_data, 'VOLCFSND', plot_ids, 'tree', 
                           estvar.filter = 'STATUSCD == 1 & DIA >= 5',
                           rowvar = 'SPCD'),
            list(filter = 'live; >= 5 in DBH', grouping = 'Species')
        ),
        c(
            get_fiesta_est(pop_data, 'VOLCFSND', plot_ids, 'tree', 
                           estvar.filter = 'STATUSCD == 0 & DIA >= 5',
                           rowvar = 'SPCD'),
            list(filter = 'dead; >= 5 in DBH', grouping = 'Species')
        )
    )
    
    agbc <- list(
        c(
            get_fiesta_est(pop_data, 'DRYBIO_AG', plot_ids, 'tree', 
                           estvar.filter = 'STATUSCD == 1 & DIA >= 1'),
            list(filter = 'live; >= 1 in DBH', grouping = '')
        ),
        c(
            get_fiesta_est(pop_data, 'CARBON_AG', plot_ids, 'tree', 
                           estvar.filter = 'STATUSCD == 1 & DIA >= 1'),
            list(filter = 'live; >= 1 in DBH', grouping = '')
        ),
        c(
            get_fiesta_est(pop_data, 'CARBON_AG', plot_ids, 'tree', 
                           estvar.filter = 'STATUSCD == 0 & DIA >= 5'),
            list(filter = 'dead; >= 5 in DBH', grouping = '')
        )
    )
    DBI::dbDisconnect(conn)
    return(list(area = forest_area, tree_ct = tree_numbers, 
                volume = volume, agbc = agbc))   
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

render_query_results <- function(group_results) {
    group_names <- names(group_results)
    group_results <- lapply(
        group_results, 
        \(result_group) {
            lapply(result_group, \(result) {
                if (!"raw" %in% names(result)) {
                    return(result)
                } else {
                    if (result$grouping == '') {
                        group_col <- "Group"
                        result$display_est <- result$raw$totest |>
                            dplyr::select(-TOTAL) |> 
                            dplyr::mutate(Group = "Total")
                    
                    } else {
                        group_col <- names(result$raw$rowest) |> head(1)
                        result$display_est <- dplyr::bind_rows(
                            result$raw$rowest |> 
                                dplyr::arrange(-est), 
                            result$raw$totest |>
                                dplyr::select(-TOTAL) |>
                                dplyr::mutate({{group_col}} := 'Total')
                        )
                    }
                    result$display_est <- result$display_est |> 
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
                        )
                    return(result)
                }
  
            })
        }
    )
    
    names(group_results) <- group_names
    
    return(group_results)
}

map_estimation_group_to_result <- function(input, results) {
    result <- switch(
        input, 
        "Area" = results$area,
        "Tree count" = results$tree_ct,
        "Volume" = results$volume,
        "AGB / Carbon" = results$agbc
    )
    return(result)
}

build_html <- function(title, results) {
    display_tags <- lapply(results, \(r) {
        
        r_tags <- list(
            tags$span(glue::glue("Estimation variable: {r$estvar}")),
            tags$span(glue::glue("Filter: {r$filter}"))
        )
        if (!is.null(r$display_est)) {
            r_tags <- c(r_tags, list(shiny::HTML(kablify_results_tables(r$display_est))))
        } else {
            r_tags <- c(r_tags, list(tags$strong("No Estimate")))
        }
        return(r_tags)
        
    })
    
    display_tags <- c(
        list(tags$h4(title)),
        display_tags
    )
    return(tagList(display_tags))
}

download_csvs <- function(file, results) {
    
    temp_dir <- tempdir()
    
    results <- render_query_results(results)
    
    filenames <- lapply(
        results, 
        \(result_group) {
            lapply(result_group, \(table) {
                if (!is.null(table$display_est)) {
                    
                    group_string <- ifelse(table$grouping == '', '', paste0('by_', gsub(" ", "-", table$grouping)))
                    filter_string <- ifelse(table$filter == '', 'no-filter', gsub(" ", "", table$filter))
                    estvar <- ifelse(table$estvar == 'Area (acres)', 'AREA', table$estvar)
                    est_filename <- file.path(
                        temp_dir,
                        glue::glue("{estvar}_{group_string}_{filter_string}.csv")
                    )
                    write.csv(
                        table$display_est |> 
                            dplyr::rename(SE_pct = `SE (%)`), 
                        est_filename, 
                        row.names = FALSE
                    )
                    return(est_filename)
                }
            }) |> unlist()
                
        }
    ) |> unlist()
    
    zip(zipfile = file, files = filenames, flags = "-j")
    
}
