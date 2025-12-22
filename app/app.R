library(shiny)
library(bslib)
library(kableExtra)
library(shinyFeedback)
library(leaflet)
library(leaflet.extras)
source("helpers.R")
library(shinyjs)
library(FIESTA)
library(shinylogs)
# 
# log_dir <- here::here("logs")
# dir.create(log_dir, showWarnings = FALSE)
# log_file <- file(file.path(log_dir, basename(tempfile(fileext = '.txt'))), "w")
# sink(log_file, append = FALSE, type = c("message"))

my_modal <- function(download_details) {
    state_string <- paste0(download_details$states, collapse = ", ")
    
    modalDialog(
        title = "Confirm query",
        glue::glue("Query will download {download_details$size} GB of data for {state_string}. Are you sure you want to run this query?"), # Use the passed argument in the UI
        footer = tagList(
            actionButton("cancel_query", "Cancel"),
            actionButton("confirm_query", "Run", class = "btn btn-success")
        )
    )
}

ui <- page_fluid(
    # App title ----
    useShinyjs(),
    waiter::use_waiter(),
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
                      actionButton("execute_query", "Run Query"),
                      card(
                          card_header("Query Results"), 
                          card_body(htmlOutput("query_results"))
                      )
                  ),
                  card_footer(htmlOutput("notice"))),
        nav_panel("Documentation", 
                  card(htmlOutput("docs"))),
    )
    
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
    
    draw_polygons <- reactiveVal(NULL)
    upload_files <- reactiveVal(NULL)
    query_polygon <- reactiveVal(NULL)
    query_states <- reactiveVal(NULL)
    
    observe({
        if ((input$polygon_type == 'Draw' && 
             (!is.null(draw_polygons()) && nrow(draw_polygons()) > 0)) ||
            (input$polygon_type == 'Upload' && 
             (!is.null(upload_files()) && nrow(upload_files()) > 0))) {
            shinyjs::enable("execute_query")
        } else {
            shinyjs::disable("execute_query")
            
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
    
    observeEvent(input$execute_query, {
        
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
                my_modal(
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
        
        # TODO: Can I pipe stdout from the FIESTA functions to the
        #       waiter display?
        
        waiter <- waiter::Waiter$new(
            id = "query",
            html = span("Downloading FIA DB data...")
        )
        waiter$show()
        shinyjs::disable("polygon_type")
        on.exit(waiter$hide())
        
        
        db_file <- download_db(query_states())
        waiter$update(html = "Extracting effected plot IDs...")
        plots <- get_affected_plot_ids(db_file, query_polygon())
        waiter$update(html = "Estimating affected resources...")
        results <- get_affected_inventory(db_file, plots)
        output$query_results <- renderText({
            render_query_results(results)
        })
        shinyjs::enable("polygon_type")
    })
    
    output$docs <- renderUI({
        
        return(list(
            h2("Salvage FIA"),
            strong("v1.0.0")
        ))
    })
    
}

onStop(function() {
    # clean_dir(here::here("temp_dir"))
    clean_dir(here::here("logs"))
})

shinyApp(
    ui = ui, 
    server = server,
    onStop(function() {
        # clean_dir(here::here("temp_dir"))
        clean_dir(here::here("logs"))
    })
)