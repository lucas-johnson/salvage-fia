library(shiny)
library(bslib)
library(kableExtra)
library(shinyFeedback)
library(leaflet)
library(leaflet.extras)
source("helpers.R")
library(shinyjs)
library(FIESTA)
library(shinytail)
library(mirai)

mirai::daemons(2)

log_dir <- here::here("logs")

if (dir.exists(log_dir)) {
    clean_dir(log_dir)
} else {
    dir.create(log_dir)    
}

log_file_path <- file.path(log_dir, basename(tempfile(fileext = '.txt')))

confirm_modal <- function(download_details) {
    state_string <- paste0(download_details$states, collapse = ", ")
    
    modalDialog(
        title = "Confirm query",
        glue::glue("Query will download ~{download_details$size} GB of data for {state_string}. Are you sure you want to run this query?"), # Use the passed argument in the UI
        footer = tagList(
            actionButton("cancel_query", "Cancel"),
            actionButton("confirm_query", "Run", class = "btn btn-success")
        )
    )
}

ui <- page_fluid(
    # App title ----
    useShinyjs(),
    title = "Salvage FIA",
    navset_card_underline(
        title = "Salvage FIA",
        nav_panel("Application", 
                  card(
                      id = "inputs",
                      full_screen = TRUE,
                      card_header("Disaster area"),
                      layout_sidebar(
                          sidebar = sidebar(
                              "Disaster Area",
                              radioButtons(
                                  "polygon_type", 
                                  "Input type", 
                                  choices = c("Draw", "Upload"), 
                                  selected = "Draw"
                              ),
                              
                              position = 'right',
                              open = 'open'
                          ),
                          conditionalPanel(
                              condition = "input.polygon_type == 'Draw'",
                              leafletOutput("map")
                          ),
                          conditionalPanel(
                              condition = "input.polygon_type == 'Upload'",
                              fileInput(
                                  "upload_shape",
                                  "",
                                  multiple = FALSE,
                                  accept = c(".geojson", ".gpkg", ".kml"),
                                  width = NULL,
                                  buttonLabel = "Browse...",
                                  placeholder = "No file selected",
                                  capture = NULL
                              )
                          )
                      )
                  ),
                  card(
                      id = "query",
                      actionButton("run_query_btn", "Run Query"),
                      card(
                          card_header("Query Results"),
                          card(
                              radioButtons(
                                  inputId = "estimation_group",
                                  label = "Estimate group",
                                  choices = c("Area", "Tree count", "Volume", "AGB / Carbon"),
                                  selected = "Area"
                              ),
                          ),
                          card_body(htmlOutput("query_results")),
                          shinytail::shinyTail("logs"),
                          downloadButton("download_data_btn", "Download Results")
                      ),
                      
                  ),
        )
    )
    
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
    
    observe({
        shinyjs::disable("download_data_btn")
        shinyjs::disable("estimation_group")
    })
    
    draw_polygons <- reactiveVal(NULL)
    upload_files <- reactiveVal(NULL)
    query_polygon <- reactiveVal(NULL)
    query_states <- reactiveVal(NULL)
    
    
    get_query_results <- ExtendedTask$new(function(query_states, query_polygon, log_file) {
        mirai(
            {
                execute_query(query_states, query_polygon, log_file)
            },
            execute_query = execute_query,
            download_db = download_db,
            get_affected_plot_ids = get_affected_plot_ids,
            get_affected_inventory = get_affected_inventory,
            log_file = log_file,
            query_polygon = query_polygon,
            query_states = query_states,
            get_fiesta_est = get_fiesta_est
        )
    }) 
    
    log_data <- reactiveVal(value = NULL, label = 'log')
    log_tail_process <- shinytail::tailFile(log_file_path)
    observe({
        shinytail::readStream(log_data, log_tail_process)
    })
    
    observe({
        if ((input$polygon_type == 'Draw' && 
             (!is.null(draw_polygons()) && nrow(draw_polygons()) > 0)) ||
            (input$polygon_type == 'Upload' && 
             (!is.null(upload_files()) && nrow(upload_files()) > 0))) {
            shinyjs::enable("run_query_btn")
        } else {
            shinyjs::disable("run_query_btn")
            
        }
    })
    
    states <- sf::st_read(
        here::here("data/cb_2024_us_state_500k/cb_2024_us_state_500k.shp"),
        quiet = TRUE
    )
    
    output$map <- renderLeaflet({
        leaflet() %>% 
            setView(lng = -98.5795, lat = 39.8283, zoom = 4) |> # center the map in USA
            addTiles(layerId = 'map_click') |>
            addDrawToolbar(
                targetGroup = "disaster_extent",
                circleOptions = FALSE,
                polylineOptions = FALSE,
                rectangleOptions = FALSE,
                circleMarkerOptions = FALSE,
                markerOptions = FALSE,
                editOptions = editToolbarOptions(
                    remove = TRUE,
                    edit = FALSE
                )
            )
        
    })
    
    observeEvent(input$map_draw_deleted_features, {
        feature <- req(input$map_draw_deleted_features)
        
        delete_ids <- lapply(feature$features, \(ft){
            ft$properties$`_leaflet_id`
        }) |> unlist()
        
        draw_polygons(draw_polygons() |> 
                           dplyr::filter(!id %in% delete_ids))
    })
    
    
    observeEvent(input$map_draw_new_feature, {
        
        feature <- req(input$map_draw_new_feature)
        # Extract coordinates (longitude and latitude pairs)
        coords <- feature$geometry$coordinates[[1]]
        
        # Convert to a matrix for sf functions
        coords_matrix <- matrix(unlist(coords), ncol = 2, byrow = TRUE)
        
        # Create an sf polygon object
        # The st_polygon function expects a list of matrices
        new_poly <- sf::st_sf(
            id = feature$properties$`_leaflet_id`, 
            geometry = sf::st_sfc(sf::st_polygon(list(coords_matrix)), 
                                  crs = 'EPSG:4326')
        )
        
        if (is.null(draw_polygons())) {
            draw_polygons(new_poly)
        } else {
            draw_polygons(dplyr::bind_rows(draw_polygons(),new_poly))
        }
        
    })
    
    observeEvent(input$upload_shape, {
        upload_files(input$upload_shape)
    })
    
    observeEvent(input$run_query_btn, {
        
        if (input$polygon_type == 'Draw') {
            query_polygon(sf::st_union(draw_polygons()))
        } else {
            query_polygon(
                sf::st_read(
                    upload_files() |> tail(1) |> dplyr::pull(datapath)
                ) |> 
                    sf::st_union()
            )
        }
        
        query_states(
            states |> 
                sf::st_filter(
                    sf::st_transform(query_polygon(), sf::st_crs(states)),
                    .predicate = sf::st_intersects
                ) |>
                sf::st_drop_geometry() |> 
                dplyr::select(NAME, STUSPS)
        )
        exists_query_states <- nrow(query_states() > 0)
        
        if (!exists_query_states) {
            showNotification(
                "Disaster polygon must intersect with United States!", 
                type = 'error'
            )
            req(exists_query_states, cancelOutput = TRUE)        
        } else {

            showModal(
                confirm_modal(
                    list(
                        states = query_states()$NAME, 
                        size = round((nrow(query_states()) * 50) / 1000, 2)
                    )
                )
            )
            
            
        }
        
    })
    
    observeEvent(input$cancel_query, {
        showNotification("Query cancelled")
        removeModal()
    })
    
    observeEvent(input$confirm_query, {
        
        removeModal()
        
        shinyjs::disable("polygon_type")
        shinyjs::disable("run_query_btn")
        shinyjs::disable("download_data_btn")
        shinyjs::disable("estimation_group")
        
        get_query_results$invoke(query_states(), query_polygon(), log_file_path)
        output$logs <- renderText({
            paste(log_data(), collapse = "\n")
        })
        
        # TODO: Figure out how to do something on the changing of
        # get_query_result$result()
    })
    
    observeEvent(get_query_results$status(), {
        if (get_query_results$status() == 'success') {
            shinyjs::enable("polygon_type")
            shinyjs::enable("run_query_btn")
            shinyjs::enable("estimation_group")
            # shinyjs::enable("download_data_btn")
            shinyjs::enable("download_data_btn")
            output$download_data_btn <- downloadHandler(
                filename = function() {
                    paste('salvage-fia-results-', Sys.Date(), '.zip', sep='')
                },
                content = function(file) {
                    download_csvs(file, get_query_results$result())
                }
            )
            
            
            # Remove logs from the UI
            output$logs <- NULL
            
            # Clear out the logs from this download
            log_data(NULL)
            
        } else {
            shinyjs::disable("download_data_btn")
        }
    })
    
    
    output$query_results <- renderUI({
        results <- render_query_results(get_query_results$result())
        results <- map_estimation_group_to_result(input$estimation_group, results)
        build_html(input$estimation_group, results)
    })
    
    output$docs <- renderUI({
        
        return(list(
            h2("Salvage FIA"),
            strong("v1.0.0")
        ))
    })
    
}

onStop(function() {
    clean_dir(here::here("temp_dir"))
})

shinyApp(
    ui = ui, 
    server = server,
    onStop(function() {
        clean_dir(here::here("temp_dir"))
    })
)