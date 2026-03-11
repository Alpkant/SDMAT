# =================================================================
# Automated Species Distribution Mapping and Analysis Tool (SDMAT)
# Shiny Version
# =================================================================
# 
# Purpose:
#   This program automates the creation of species distribution maps and 
#   calculates key range metrics (AOO & EOO) following IUCN Red List standards.
#   It processes shapefiles with species distribution data and generates:
#   - Standardized distribution maps (similar to IUCN format)
#   - Area of Occupancy (AOO) calculations in km²
#   - Extent of Occurrence (EOO) calculations in km²
#   - Compiled results in Excel format
#
# Background:
#   The IUCN Red List requires standardized calculations of species ranges.
#   - AOO: Area of Occupancy - the area within EOO that is actually occupied 
#     by the taxon (calculated using 2×2 km grid cells)
#   - EOO: Extent of Occurrence - the minimum convex polygon containing all 
#     occurrences of a species
#
# =================================================================

# ---------- Package Installation and Loading ----------

# Install required packages if not already installed
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("shinyWidgets", quietly = TRUE)) install.packages("shinyWidgets")
if (!requireNamespace("shinydashboard", quietly = TRUE)) install.packages("shinydashboard")
if (!requireNamespace("shinyFiles", quietly = TRUE)) install.packages("shinyFiles")
if (!requireNamespace("shinyjs", quietly = TRUE)) install.packages("shinyjs")
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("raster", quietly = TRUE)) install.packages("raster")
if (!requireNamespace("sp", quietly = TRUE)) install.packages("sp")
if (!requireNamespace("redlistr", quietly = TRUE)) install.packages("redlistr")
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
if (!requireNamespace("tmap", quietly = TRUE)) install.packages("tmap")
if (!requireNamespace("terra", quietly = TRUE)) install.packages("terra")
if (!requireNamespace("leaflet", quietly = TRUE)) install.packages("leaflet")
if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")

# Load required packages
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyFiles)
library(shinyjs)
library(DT)
library(sf)
library(raster)
library(sp)
library(redlistr)
library(openxlsx)
library(tmap)
library(terra)
library(leaflet)
library(RColorBrewer)

# ---------- Helper Functions ----------

#' Calculate Area of Occupancy using the IUCN standard 2x2km grid
#' @param species_sf Species distribution as an sf object
#' @return AOO value in km²
calculate_aoo <- function(species_sf) {
  tryCatch({
    # Try redlistr's AOO calculation functions
    aoo_km2 <- redlistr::AOO.calc(species_sf, 
                                  cell_width = 2000)$AOO # 2000m = 2km
    return(aoo_km2)
  }, error = function(e) {
    # Manual calculation as fallback
    tryCatch({
      # Create a 2x2km grid
      bbox <- st_bbox(species_sf)
      width <- 2000 # 2km in meters
      height <- 2000
      
      # Create grid cells
      grid <- st_make_grid(species_sf, 
                           cellsize = c(width, height),
                           what = "polygons")
      grid_sf <- st_sf(geometry = grid)
      
      # Find which cells intersect with species distribution
      intersects <- st_intersects(grid_sf, species_sf)
      occupied_cells <- lengths(intersects) > 0
      
      # Calculate AOO (4 sq km per cell)
      aoo_km2 <- sum(occupied_cells) * 4
      
      return(aoo_km2)
    }, error = function(e2) {
      # Alternative approach using sf buffer and a different grid approach
      tryCatch({
        # Transform to equal-area projection if not already
        if(st_crs(species_sf)$proj4string != "+proj=cea") {
          species_sf_equal <- st_transform(species_sf, "+proj=cea +lat_ts=0 +lon_0=0")
        } else {
          species_sf_equal <- species_sf
        }
        
        # Create a buffer around points to ensure grid cells
        species_buffer <- st_buffer(species_sf_equal, dist = 1)
        
        # Create a 2km x 2km grid
        grid <- st_make_grid(species_buffer, cellsize = c(2000, 2000))
        grid_sf <- st_sf(id = 1:length(grid), geometry = grid)
        
        # Calculate intersection
        grid_intersect <- st_intersects(grid_sf, species_sf_equal)
        occupied <- lengths(grid_intersect) > 0
        
        # Count occupied cells and multiply by cell area
        aoo_km2 <- sum(occupied) * 4
        
        return(aoo_km2)
      }, error = function(e3) {
        warning(paste0("AOO calculation error: ", e$message, " | ", e2$message, " | ", e3$message))
        return(NA)
      })
    })
  })
}

#' Calculate Extent of Occurrence using minimum convex hull
#' @param species_sf Species distribution as an sf object
#' @return EOO value in km²
calculate_eoo <- function(species_sf) {
  tryCatch({
    # Use sf functions for EOO calculation
    species_union <- st_union(species_sf)
    hull <- st_convex_hull(species_union)
    
    # Calculate area in km²
    eoo_km2 <- as.numeric(st_area(hull)) / 1000000
    
    return(eoo_km2)
  }, error = function(e) {
    # Fallback to redlistr if sf approach fails
    tryCatch({
      eoo_km2 <- redlistr::EOO.calc(species_sf)$EOO
      return(eoo_km2)
    }, error = function(e2) {
      # Last resort: manually calculate convex hull using sf
      tryCatch({
        # Extract coordinates
        coords <- st_coordinates(species_sf)
        
        # Convert to spatial points
        points <- st_as_sf(data.frame(
          x = coords[, "X"],
          y = coords[, "Y"]
        ), coords = c("x", "y"), crs = st_crs(species_sf))
        
        # Calculate convex hull
        hull <- st_convex_hull(st_union(points))
        
        # Calculate area
        eoo_km2 <- as.numeric(st_area(hull)) / 1000000
        
        return(eoo_km2)
      }, error = function(e3) {
        warning(paste0("EOO calculation error: ", e$message, " | ", e2$message, " | ", e3$message))
        return(NA)
      })
    })
  })
}

#' Create a species distribution map with optional bathymetry
#' @param species_sf Species distribution as an sf object
#' @param species_name Name of the species for the title
#' @param output_path File path to save the map
#' @param bathymetry_raster Optional bathymetry raster data
create_distribution_map <- function(species_sf, species_name, output_path, 
                                    bathymetry_raster = NULL) {
  # Set the map theme to match IUCN style
  tmap_mode("plot")
  
  # Try to load rnaturalearth for world borders
  world <- NULL
  if(require("rnaturalearth", quietly = TRUE)) {
    world <- ne_countries(scale = "medium", returnclass = "sf")
  } else {
    # Fallback if rnaturalearth not available
    if(!requireNamespace("rnaturalearth", quietly = TRUE)) {
      install.packages("rnaturalearth")
      if(requireNamespace("rnaturalearthdata", quietly = TRUE)) {
        install.packages("rnaturalearthdata")
      }
      library(rnaturalearth)
      world <- ne_countries(scale = "medium", returnclass = "sf")
    }
  }
  
  # Start with basemap if we have bathymetry
  if(!is.null(bathymetry_raster)) {
    # Crop bathymetry to extent of species + buffer
    species_bbox <- st_bbox(species_sf)
    expanded_bbox <- species_bbox + c(-2, -2, 2, 2) # Add 2-degree buffer
    
    # Crop bathymetry to relevant area
    cropped_bath <- try(crop(bathymetry_raster, expanded_bbox), silent = TRUE)
    
    if(!inherits(cropped_bath, "try-error") && !is.null(cropped_bath)) {
      # Create map with bathymetry
      map <- tm_shape(cropped_bath) +
        tm_raster(palette = "Blues", title = "Depth (m)", 
                  style = "cont", alpha = 0.7) +
        tm_shape(species_sf) +
        tm_polygons(col = "firebrick", alpha = 0.7, border.col = "black", 
                    border.alpha = 0.8, lwd = 1.5) +
        tm_layout(title = paste0("Distribution of ", species_name),
                  title.size = 1.2,
                  legend.outside = TRUE,
                  legend.outside.position = "right")
    } else {
      # Fallback to standard map if bathymetry processing fails
      map <- create_standard_map(species_sf, species_name, world)
    }
  } else {
    # Create standard map without bathymetry
    map <- create_standard_map(species_sf, species_name, world)
  }
  
  # Save the map
  tmap_save(map, filename = output_path, width = 8, height = 6, dpi = 300)
  
  # Return the map object for potential display in Shiny
  return(map)
}

#' Create a standard species distribution map (no bathymetry)
#' @param species_sf Species distribution as an sf object
#' @param species_name Name of the species for the title
#' @param world Optional world borders data
#' @return A tmap object
create_standard_map <- function(species_sf, species_name, world = NULL) {
  # Create the map
  if(!is.null(world)) {
    tm_shape(world) +
      tm_borders(col = "darkgray", lwd = 0.5) +
      tm_shape(species_sf) +
      tm_polygons(col = "firebrick", alpha = 0.7, border.col = "black", 
                  border.alpha = 0.8, lwd = 1.5) +
      tm_layout(title = paste0("Distribution of ", species_name),
                title.size = 1.2,
                legend.outside = TRUE)
  } else {
    # Map without world borders if not available
    tm_shape(species_sf) +
      tm_polygons(col = "firebrick", alpha = 0.7, border.col = "black", 
                  border.alpha = 0.8, lwd = 1.5) +
      tm_layout(title = paste0("Distribution of ", species_name),
                title.size = 1.2,
                legend.outside = TRUE)
  }
}

#' Create an interactive leaflet map for Shiny
#' @param species_sf Species distribution as an sf object
#' @param species_name Name of the species for popup
#' @return A leaflet map object
create_interactive_map <- function(species_sf, species_name) {
  tryCatch({
    # Print debug information
    print(paste("Processing species:", species_name))
    print(paste("Geometry type:", st_geometry_type(species_sf)[1]))
    print(paste("CRS:", st_crs(species_sf)$proj4string))
    
    # Ensure proper CRS for leaflet
    if (!st_is_longlat(species_sf)) {
      species_sf <- st_transform(species_sf, 4326)
    }
    
    # Handle 3D coordinates by dropping Z dimension if present
    if (any(grepl("Z", st_geometry_type(species_sf)))) {
      species_sf <- st_zm(species_sf, drop = TRUE, what = "Z")
    }
    
    # Ensure valid geometries
    species_sf <- st_make_valid(species_sf)
    
    # Create popup content
    popup_content <- paste0("<strong>Species:</strong> ", species_name)
    
    # Create leaflet map
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap tiles
      addPolygons(
        data = species_sf,
        fillColor = "red",
        fillOpacity = 0.6,
        color = "black",
        weight = 2,
        popup = popup_content
      ) %>%
      addLegend(
        position = "bottomright",
        colors = "red",
        labels = "Species Distribution",
        opacity = 0.7
      )
  }, error = function(e) {
    print(paste("Error in create_interactive_map:", e$message))
    # Return a basic map with error message
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addControl(html = paste0("<h4>Error displaying map for ", species_name, "</h4>"), position = "center")
  })
}

#' Process a single species shapefile and return result row
#' @param shapefile Path to shapefile
#' @param plot_folder Path to output folder for maps
#' @param bathymetry_raster Optional bathymetry raster
#' @param equal_area_crs CRS for area calculations
#' @return Data frame row or NULL on error
process_single_species <- function(shapefile, plot_folder, bathymetry_raster, 
                                   equal_area_crs = 6933) {
  species_name <- tools::file_path_sans_ext(basename(shapefile))
  tryCatch({
    species_sf <- st_read(shapefile, quiet = TRUE)
    species_sf_equal_area <- st_transform(species_sf, crs = equal_area_crs)
    aoo_km2 <- calculate_aoo(species_sf_equal_area)
    eoo_km2 <- calculate_eoo(species_sf_equal_area)
    map_filename <- paste0(species_name, ".png")
    map_path <- file.path(plot_folder, map_filename)
    create_distribution_map(
      species_sf = st_transform(species_sf, 4326),
      species_name = species_name,
      output_path = map_path,
      bathymetry_raster = bathymetry_raster
    )
    data.frame(
      SpeciesName = species_name,
      AOO_km2 = aoo_km2,
      EOO_km2 = eoo_km2,
      MapFile = map_filename,
      ShapefilePath = shapefile,
      ProcessingDate = format(Sys.Date(), "%Y-%m-%d"),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    message(paste0("Error processing ", species_name, ": ", e$message))
    NULL
  })
}

#' Process species distribution shapefiles and calculate metrics
#' @param input_folder Path to folder containing species shapefiles
#' @param use_bathymetry Whether to include bathymetry in maps
#' @param bathymetry_file Optional path to bathymetry raster file
#' @param progress_callback Function to update progress in Shiny
process_species_data <- function(input_folder, use_bathymetry = FALSE, 
                                 bathymetry_file = NULL, 
                                 progress_callback = NULL) {
  
  start_time <- Sys.time()
  plot_folder <- file.path(input_folder, "Species_Plots")
  dir.create(plot_folder, showWarnings = FALSE, recursive = TRUE)
  output_file <- file.path(input_folder, "Species_Distribution_Results.xlsx")
  shapefiles <- list.files(path = input_folder, pattern = "\\.shp$", full.names = TRUE)
  
  if(length(shapefiles) == 0) {
    stop("No shapefiles found in the specified folder.")
  }
  
  message(paste0("Found ", length(shapefiles), " species shapefiles to process"))
  
  if(!is.null(progress_callback) && is.function(progress_callback)) {
    progress_callback(detail = paste0("Found ", length(shapefiles), " species shapefiles"), value = 0.1)
  }
  
  bathymetry_raster <- NULL
  if(use_bathymetry && !is.null(bathymetry_file) && file.exists(bathymetry_file)) {
    message("Loading bathymetry data...")
    if(!is.null(progress_callback) && is.function(progress_callback)) {
      progress_callback(detail = "Loading bathymetry data...", value = 0.15)
    }
    bathymetry_raster <- try(raster(bathymetry_file), silent = TRUE)
    if(inherits(bathymetry_raster, "try-error") || is.null(bathymetry_raster)) {
      warning("Failed to load bathymetry raster. Maps will be created without bathymetry.")
      bathymetry_raster <- NULL
    }
  }
  
  equal_area_crs <- 6933
  results <- data.frame(
    SpeciesName = character(),
    AOO_km2 = numeric(),
    EOO_km2 = numeric(),
    MapFile = character(),
    ShapefilePath = character(),
    ProcessingDate = character(),
    stringsAsFactors = FALSE
  )
  
  for(i in seq_along(shapefiles)) {
    shapefile <- shapefiles[i]
    species_name <- tools::file_path_sans_ext(basename(shapefile))
    message(paste0("\nProcessing ", i, " of ", length(shapefiles), ": ", species_name))
    progress_value <- 0.2 + (0.7 * (i-1) / length(shapefiles))
    if(!is.null(progress_callback) && is.function(progress_callback)) {
      progress_callback(
        detail = paste0("Processing ", i, " of ", length(shapefiles), ": ", species_name),
        value = progress_value
      )
    }
    row <- process_single_species(shapefile, plot_folder, bathymetry_raster, equal_area_crs)
    if(!is.null(row)) results <- rbind(results, row)
  }
  
  if(nrow(results) > 0) {
    message("\nExporting results to Excel...")
    if(!is.null(progress_callback) && is.function(progress_callback)) {
      progress_callback(detail = "Exporting results to Excel...", value = 0.9)
    }
    write.xlsx(results, file = output_file, rowNames = FALSE)
    message(paste0("Results saved to: ", output_file))
  } else {
    warning("No results to export.")
  }
  
  end_time <- Sys.time()
  proc_time <- difftime(end_time, start_time, units = "secs")
  message(paste0("\nProcessing completed in ", round(proc_time, 2), " seconds"))
  if(!is.null(progress_callback) && is.function(progress_callback)) {
    progress_callback(detail = "Processing complete!", value = 1.0)
  }
  return(list(
    results = results,
    plot_folder = plot_folder,
    output_file = output_file,
    processing_time = proc_time
  ))
}

# ---------- Shiny UI ----------

ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = "Species Distribution Mapping Tool"
  ),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Process Data", tabName = "process", icon = icon("gears")),
      menuItem("Results", tabName = "results", icon = icon("table")),
      menuItem("Maps", tabName = "maps", icon = icon("map")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  # Body
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f5f5f5;
        }
        .box {
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
        .info-box {
          min-height: 90px;
        }
        .info-box-icon {
          height: 90px;
          line-height: 90px;
        }
        .info-box-content {
          padding-top: 10px;
        }
      "))
    ),
    
    tabItems(
      # Process Data Tab
      tabItem(tabName = "process",
              fluidRow(
                box(
                  title = "Input Parameters", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(6,
                           # Input folder selection
                           shinyDirButton("dir_input", "Select Input Folder", "Select a folder containing species shapefiles"),
                           verbatimTextOutput("dir_selected"),
                           tags$hr(),
                           # Output folder selection
                           shinyDirButton("dir_output", "Select Output Folder", "Select folder for results and maps (optional, default to input folder)"),
                           verbatimTextOutput("dir_output_selected"),
                           tags$hr(),
                           # Bathymetry options
                           checkboxInput("use_bathymetry", "Include bathymetry in maps", FALSE),
                           conditionalPanel(
                             condition = "input.use_bathymetry == true",
                             shinyFilesButton("file_bathymetry", "Select Bathymetry File", 
                                              "Select a bathymetry raster file (.tif, .asc, .nc)", 
                                              multiple = FALSE),
                             verbatimTextOutput("bathymetry_selected")
                           )
                    ),
                    column(6,
                           # Processing options
                           h4("Processing Options"),
                           radioButtons("output_format", "Map Format:",
                                        choices = c("Static Maps (PNG)" = "static", 
                                                    "Interactive Maps + Static Maps" = "both"),
                                        selected = "static"),
                           helpText("Interactive maps are displayed in the app but not saved to disk."),
                           tags$hr(),
                           # Process button
                           actionButton("btn_process", "Process Species Data", 
                                        icon = icon("play"), 
                                        style = "color: #fff; background-color: #3c8dbc; border-color: #367fa9; padding: 10px;"),
                           tags$br(), tags$br(),
                           # Download button for Excel results
                           uiOutput("download_ui")
                    )
                  )
                )
              ),
              
              # Progress box
              fluidRow(
                box(
                  title = "Processing Status", status = "info", solidHeader = TRUE, width = 12,
                  uiOutput("processing_status"),
                  conditionalPanel(
                    condition = "input.btn_process > 0",
                    progressBar(id = "progress", value = 0, display_pct = TRUE),
                    verbatimTextOutput("process_log")
                  )
                )
              ),
              
              # Quick results summary
              uiOutput("summary_boxes")
      ),
      
      # Results Tab
      tabItem(tabName = "results",
              fluidRow(
                box(
                  title = "Analysis Results", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("results_table")
                )
              )
      ),
      
      # Maps Tab
      tabItem(tabName = "maps",
              fluidRow(
                box(
                  title = "Species Maps", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(4,
                           selectInput("map_select", "Select Species:", choices = NULL)
                    ),
                    column(8,
                           conditionalPanel(
                             condition = "input.map_select != null",
                             tabsetPanel(id = "map_tabs",
                                         tabPanel("Static Map", imageOutput("static_map", height = "500px")),
                                         tabPanel("Interactive Map", leafletOutput("interactive_map", height = "500px"))
                             )
                           )
                    )
                  )
                )
              )
      ),
      
      # About Tab
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "About This Tool", status = "primary", solidHeader = TRUE, width = 12,
                  h3("Species Distribution Mapping and Analysis Tool"),
                  p("This tool automates the creation of species distribution maps and calculates key range metrics 
               following IUCN Red List standards."),
                  
                  h4("Key Features:"),
                  tags$ul(
                    tags$li("Calculate Area of Occupancy (AOO) using IUCN standard 2×2 km grid"),
                    tags$li("Calculate Extent of Occurrence (EOO) using minimum convex hull method"),
                    tags$li("Generate standardized distribution maps with optional bathymetry"),
                    tags$li("Create interactive web maps for visualization"),
                    tags$li("Export results to Excel format")
                  ),
                  
                  h4("IUCN Red List Metrics:"),
                  tags$dl(
                    tags$dt("Area of Occupancy (AOO)"),
                    tags$dd("The area within EOO that is actually occupied by the taxon, calculated using 2×2 km grid cells."),
                    tags$dt("Extent of Occurrence (EOO)"),
                    tags$dd("The minimum convex polygon containing all occurrences of a species.")
                  ),
                  
                  h4("Input Requirements:"),
                  tags$ul(
                    tags$li("Folder containing one or more shapefiles (.shp) of species distributions"),
                    tags$li("Shapefiles should contain polygon data representing species range"),
                    tags$li("Optional bathymetry raster for depth-based visualization")
                  ),
                  
                  h4("Output Files:"),
                  tags$ul(
                    tags$li("Excel file with results will be saved as 'Species_Distribution_Results.xlsx'"),
                    tags$li("Maps will be saved in a subfolder named 'Species_Plots'"),
                    tags$li("By default, both are saved in the input folder. Use 'Select Output Folder' to save to a different location.")
                  ),
                  
                  hr(),
                  p("For questions or support, please open an issue on github.com/Alpkant/SDMAT"),
                  hr(),
                  p("Developed by Alperen Kantarci")
                )
              )
      )
    )
  )
)

# ---------- Shiny Server ----------

server <- function(input, output, session) {
  rv <- reactiveValues(
    input_dir = NULL,
    output_dir = NULL,
    bathymetry_file = NULL,
    results = NULL,
    plot_folder = NULL,
    output_file = NULL,
    processing_time = NULL,
    log_messages = NULL,
    is_processing = FALSE,
    shapefiles = NULL,
    current_index = 0L,
    processing_results = NULL,
    bathymetry_raster = NULL,
    progress_obj = NULL,
    start_time = NULL
  )
  
  volumes <- c("Working Directory"= getwd(), Home = "~" ,"R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, "dir_input", roots = volumes, session = session)
  shinyDirChoose(input, "dir_output", roots = volumes, session = session)
  
  # Setup shinyFileChoose
  shinyFileChoose(input, "file_bathymetry", roots = volumes, filetypes = c("tif", "asc", "nc"))
  
  output$dir_selected <- renderText({
    if(is.integer(input$dir_input)) return("No folder selected")
    rv$input_dir <- parseDirPath(volumes, input$dir_input)
    if(length(rv$input_dir) > 0) {
      return(paste0("Input: ", rv$input_dir))
    } else {
      return("No folder selected")
    }
  })
  
  output$dir_output_selected <- renderText({
    if(is.integer(input$dir_output)) {
      rv$output_dir <- NULL
      return("No output folder (using input folder)")
    }
    rv$output_dir <- parseDirPath(volumes, input$dir_output)
    if(length(rv$output_dir) > 0) {
      return(paste0("Output: ", rv$output_dir))
    } else {
      rv$output_dir <- NULL
      return("No output folder (using input folder)")
    }
  })
  
  # Display selected bathymetry file
  output$bathymetry_selected <- renderText({
    if(is.integer(input$file_bathymetry)) return("No bathymetry file selected")
    
    # Parse the file path and store it
    fileInfo <- parseFilePaths(volumes, input$file_bathymetry)
    if(nrow(fileInfo) > 0) {
      rv$bathymetry_file <- as.character(fileInfo$datapath)
      return(paste0("Selected file: ", basename(rv$bathymetry_file)))
    } else {
      rv$bathymetry_file <- NULL
      return("No bathymetry file selected")
    }
  })
  
  # Download button for Excel results
  output$download_ui <- renderUI({
    if(!is.null(rv$output_file) && file.exists(rv$output_file)) {
      downloadButton("download_excel", "Download Results (Excel)", 
                     style = "color: #fff; background-color: #00a65a; border-color: #008d4c")
    }
  })
  
  # Download handler for Excel results
  output$download_excel <- downloadHandler(
    filename = function() {
      "Species_Distribution_Results.xlsx"
    },
    content = function(file) {
      file.copy(rv$output_file, file)
    }
  )
  
  observeEvent(input$btn_process, {
    if(is.null(rv$input_dir) || !dir.exists(rv$input_dir)) {
      showNotification("Please select a valid input folder", type = "error")
      return()
    }
    if(input$use_bathymetry && (is.null(rv$bathymetry_file) || !file.exists(rv$bathymetry_file))) {
      showNotification("Please select a valid bathymetry file or disable bathymetry", type = "error")
      return()
    }
    
    rv$results <- NULL
    rv$plot_folder <- NULL
    rv$output_file <- NULL
    rv$processing_time <- NULL
    rv$log_messages <- "Starting processing...\n"
    rv$is_processing <- TRUE
    
    shapefiles <- list.files(path = rv$input_dir, pattern = "\\.shp$", full.names = TRUE)
    if(length(shapefiles) == 0) {
      showNotification("No shapefiles found in the specified folder.", type = "error")
      rv$is_processing <- FALSE
      return()
    }
    
    rv$shapefiles <- shapefiles
    rv$current_index <- 0L
    rv$processing_results <- data.frame(
      SpeciesName = character(), AOO_km2 = numeric(), EOO_km2 = numeric(),
      MapFile = character(), ShapefilePath = character(), ProcessingDate = character(),
      stringsAsFactors = FALSE
    )
    output_base <- if(!is.null(rv$output_dir) && dir.exists(rv$output_dir)) rv$output_dir else rv$input_dir
    rv$plot_folder <- file.path(output_base, "Species_Plots")
    dir.create(rv$plot_folder, showWarnings = FALSE, recursive = TRUE)
    rv$output_file <- file.path(output_base, "Species_Distribution_Results.xlsx")
    rv$start_time <- Sys.time()
    
    rv$bathymetry_raster <- NULL
    if(input$use_bathymetry && !is.null(rv$bathymetry_file) && file.exists(rv$bathymetry_file)) {
      rv$log_messages <- paste0(rv$log_messages, "Loading bathymetry data...\n")
      rv$bathymetry_raster <- try(raster(rv$bathymetry_file), silent = TRUE)
      if(inherits(rv$bathymetry_raster, "try-error")) rv$bathymetry_raster <- NULL
    }
    
    rv$progress_obj <- Progress$new(session, min = 0, max = 1)
    rv$progress_obj$set(value = 0.05, message = "Processing species data", detail = paste0("Found ", length(shapefiles), " shapefiles"))
    updateProgressBar(session, "progress", value = 5, status = "info")
  })
  
  observe({
    if(!rv$is_processing || is.null(rv$shapefiles) || length(rv$shapefiles) == 0) return()
    i <- isolate(rv$current_index) + 1L
    n_total <- length(rv$shapefiles)
    
    if(i > n_total) {
      tryCatch({
        if(nrow(rv$processing_results) > 0) {
          rv$log_messages <- paste0(rv$log_messages, "Exporting results to Excel...\n")
          if(!is.null(rv$progress_obj)) {
            rv$progress_obj$set(value = 0.95, detail = "Exporting results to Excel...")
          }
          updateProgressBar(session, "progress", value = 95, status = "info")
          write.xlsx(rv$processing_results, file = rv$output_file, rowNames = FALSE)
        }
        if(!is.null(rv$progress_obj)) {
          rv$progress_obj$set(value = 1, detail = "Processing complete!")
          rv$progress_obj$close()
        }
        updateProgressBar(session, "progress", value = 100, status = "success")
        rv$results <- rv$processing_results
        rv$processing_time <- difftime(Sys.time(), rv$start_time, units = "secs")
        rv$log_messages <- paste0(rv$log_messages, "Processing complete!\n")
        if(nrow(rv$results) > 0) {
          updateSelectInput(session, "map_select", choices = c("Select a species" = "", rv$results$SpeciesName))
          showNotification(paste0("Processing completed successfully. ", nrow(rv$results), " species processed."), type = "message", duration = 5)
        }
      }, error = function(e) {
        showNotification(paste0("Error during export: ", e$message), type = "error", duration = NULL)
        rv$log_messages <- paste0(rv$log_messages, "ERROR: ", e$message, "\n")
      }, finally = {
        rv$is_processing <- FALSE
      })
      return()
    }
    
    shapefile <- rv$shapefiles[i]
    species_name <- tools::file_path_sans_ext(basename(shapefile))
    progress_pct <- 0.1 + (0.85 * (i - 1) / n_total)
    
    tryCatch({
      if(!is.null(rv$progress_obj)) {
        rv$progress_obj$set(value = progress_pct, detail = paste0("Processing ", i, " of ", n_total, ": ", species_name))
      }
      updateProgressBar(session, "progress", value = round(progress_pct * 100), status = "info")
      rv$log_messages <- paste0(rv$log_messages, "Processing ", i, " of ", n_total, ": ", species_name, "\n")
      
      row <- process_single_species(shapefile, rv$plot_folder, rv$bathymetry_raster, 6933)
      if(!is.null(row)) {
        rv$processing_results <- rbind(rv$processing_results, row)
      }
    }, error = function(e) {
      rv$log_messages <- paste0(rv$log_messages, "Error: ", e$message, "\n")
    })
    
    rv$current_index <- i
    invalidateLater(0, session)
  })
  
  # Process log output
  output$process_log <- renderText({
    rv$log_messages
  })
  
  # Processing status UI
  output$processing_status <- renderUI({
    if(rv$is_processing) {
      div(
        h4("Processing in progress...", style = "color: #3c8dbc"),
        p("Please wait while your species data is being processed. This may take several minutes depending on the number of shapefiles and complexity of the data.")
      )
    } else if(!is.null(rv$results)) {
      div(
        h4("Processing complete!", style = "color: #00a65a"),
        p(paste0("Successfully processed ", nrow(rv$results), " species in ", 
                 round(as.numeric(rv$processing_time), 2), " seconds"))
      )
    } else {
      div(
        h4("Ready to process data", style = "color: #444"),
        p("Select an input folder containing shapefiles and click 'Process Species Data' to begin.")
      )
    }
  })
  
  # Summary boxes
  output$summary_boxes <- renderUI({
    if(!is.null(rv$results) && nrow(rv$results) > 0) {
      fluidRow(
        valueBox(
          nrow(rv$results),
          "Species Processed",
          icon = icon("leaf"),
          color = "green"
        ),
        valueBox(
          paste0(round(mean(rv$results$AOO_km2, na.rm = TRUE), 2), " km²"),
          "Average AOO",
          icon = icon("chart-area"),
          color = "blue"
        ),
        valueBox(
          paste0(round(mean(rv$results$EOO_km2, na.rm = TRUE), 2), " km²"),
          "Average EOO",
          icon = icon("chart-line"),
          color = "purple"
        )
      )
    }
  })
  
  # Results table
  output$results_table <- renderDT({
    req(rv$results)
    datatable(rv$results,
              options = list(
                pageLength = 10,
                autoWidth = TRUE,
                searchHighlight = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              rownames = FALSE,
              caption = "Species Distribution Metrics",
              extensions = 'Buttons') %>%
      formatRound(columns = c('AOO_km2', 'EOO_km2'), digits = 2)
  })
  
  # Static map display
  output$static_map <- renderImage({
    req(input$map_select)
    req(rv$plot_folder)
    
    # Get the selected species map file
    species_name <- input$map_select
    map_file <- rv$results[rv$results$SpeciesName == species_name, "MapFile"]
    
    if(length(map_file) > 0) {
      map_path <- file.path(rv$plot_folder, map_file)
      
      if(file.exists(map_path)) {
        # Return the image
        list(
          src = map_path,
          contentType = "image/png",
          width = "100%",
          alt = paste0("Distribution map of ", species_name)
        )
      } else {
        # Return placeholder if file doesn't exist
        list(
          src = "www/map_placeholder.png",
          contentType = "image/png",
          width = "100%",
          alt = "Map not available"
        )
      }
    }
  }, deleteFile = FALSE)
  
  # Interactive map display
  output$interactive_map <- renderLeaflet({
    req(input$map_select)
    req(rv$results)

    species_name <- input$map_select

    # Defensive: skip if no species selected or if it's the placeholder
    if (is.null(species_name) || species_name == "" || species_name == "Select a species") {
      return(
        leaflet() %>%
          addTiles() %>%
          setView(lng = 0, lat = 0, zoom = 2) %>%
          addControl(html = "<h4>Map data not available</h4>", position = "center")
      )
    }

    # Get the shapefile path from the results table
    shapefile_path <- rv$results$ShapefilePath[rv$results$SpeciesName == species_name]

    if (length(shapefile_path) > 0 && !is.na(shapefile_path[1]) && file.exists(shapefile_path[1])) {
      species_sf <- st_read(shapefile_path[1], quiet = TRUE)
      print(str(species_sf))
      create_interactive_map(species_sf, species_name)
    } else {
      leaflet() %>%
        addTiles() %>%
        setView(lng = 0, lat = 0, zoom = 2) %>%
        addControl(html = "<h4>Map data not available</h4>", position = "center")
    }
  })
  
  # Update map tabs visibility based on selected output format
  observe({
    if(input$output_format == "static") {
      hideTab("map_tabs", "Interactive Map")
    } else {
      showTab("map_tabs", "Interactive Map")
    }
  })
}

# ---------- Run the application ----------

shinyApp(ui = ui, server = server)