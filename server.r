# server.R
# Server logic for Fisheries and Climate Vulnerability Assessment Dashboard

# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)
library(htmltools)
library(readxl)
library(RSQLite)
library(readr)
library(stringr)
library(reticulate)
library(rmarkdown)
library(scales)
library(tidyr)
library(shinyjs)


# Server function
function(input, output, session) {
  
  # Helper functions ----------------------------------------------------------
  
  # Function to safely read CSV files
  safe_read_csv <- function(file_path, ...) {
    tryCatch({
      read_csv(file_path, ...)
    }, error = function(e) {
      warning(paste("Could not read file:", file_path, "Error:", e$message))
      return(NULL)
    })
  }
  
  # Function to safely read Excel files
  safe_read_excel <- function(file_path, ...) {
    tryCatch({
      read_excel(file_path, ...)
    }, error = function(e) {
      warning(paste("Could not read file:", file_path, "Error:", e$message))
      return(NULL)
    })
  }
  
  # Function to list files with specific extension in a directory
  list_files_with_ext <- function(dir_path, extension) {
    if (!dir.exists(dir_path)) {
      warning(paste("Directory not found:", dir_path))
      return(character(0))
    }
    files <- list.files(dir_path, pattern = paste0("\\.", extension, "$"), full.names = TRUE)
    return(files)
  }
  
  # Function to extract filenames without extension
  extract_filename <- function(file_path) {
    basename(tools::file_path_sans_ext(file_path))
  }
  
  # Function to load data from SQLite database
  load_db_data <- function(db_path, query) {
    if (!file.exists(db_path)) {
      warning(paste("Database not found at:", db_path))
      return(NULL)
    }
    
    tryCatch({
      conn <- dbConnect(SQLite(), db_path)
      on.exit(dbDisconnect(conn))
      result <- dbGetQuery(conn, query)
      return(result)
    }, error = function(e) {
      warning(paste("Error reading database:", e$message))
      return(NULL)
    })
  }
  
  # 1. Executive Summary ------------------------------------------------------
  
  # Load executive summary data
  exec_summary_data <- reactive({
    safe_read_csv("data/resumen/summary.csv")
  })
  
  # Render executive summary text
  output$executive_summary <- renderUI({
    data <- exec_summary_data()
    
    if (is.null(data)) {
      return(HTML("<div class='alert alert-warning'>Executive summary data not available.</div>"))
    }
    
    # Assuming the CSV has columns like "section" and "content"
    sections <- lapply(1:nrow(data), function(i) {
      section <- data$section[i]
      content <- data$content[i]
      
      tagList(
        h4(section),
        p(content)
      )
    })
    
    do.call(tagList, sections)
  })
  
  # 2. Environmental Variables ------------------------------------------------
  
  # List available entities (states)
  env_entities <- reactive({
    env_files <- list_files_with_ext("data/var_amb", "xlsx")
    entities <- sapply(env_files, extract_filename)
    return(entities)
  })
  
  # Update entity selector
  observe({
    entities <- env_entities()
    updateSelectInput(session, "env_entity", 
                      choices = entities,
                      selected = if(length(entities) > 0) entities[1] else NULL,
                      label = "Seleccionar Entidad:")
  })
  
  # Load environmental data for selected entity
  env_data <- reactive({
    req(input$env_entity)
    file_path <- file.path("data/var_amb", paste0(input$env_entity, ".xlsx"))
    safe_read_excel(file_path)
  })
  
  # Update variable selector based on selected entity
  observe({
    data <- env_data()
    if (!is.null(data)) {
      # Assuming first column is date, all others are variables
      var_names <- colnames(data)[-1]
      updateSelectInput(session, "env_variable", 
                      choices = var_names,
                      selected = if(length(var_names) > 0) var_names[1] else NULL,
                      label = "Seleccionar Variable:")
    }
  })
  
  # Plot time series for selected entity and variable
  output$env_time_series <- renderPlotly({
    req(input$env_entity, input$env_variable)
    data <- env_data()
    
    if (is.null(data)) {
      return(NULL)
    }
    
    # Assuming first column is date
    date_col <- colnames(data)[1]
    
    # Convert date to Year-Month format if it's not already
    # This handles the issue mentioned in the prompt for x-axis
    if (!inherits(data[[date_col]], "Date") && !inherits(data[[date_col]], "POSIXct")) {
      tryCatch({
        # Attempt to convert the date column to proper Date format
        data[[date_col]] <- as.Date(data[[date_col]])
      }, error = function(e) {
        # If that fails, try parsing it as character
        if (is.character(data[[date_col]])) {
          data[[date_col]] <- as.Date(data[[date_col]], format = "%Y-%m-%d")
        }
      })
    }
    
    # Format for year-month display
    if (inherits(data[[date_col]], "Date") || inherits(data[[date_col]], "POSIXct")) {
      data$year_month <- format(data[[date_col]], "%Y-%m")
      date_col <- "year_month"
      
      # If desired, create a proper time order
      data$year_month <- factor(data$year_month, levels = unique(data$year_month))
    }
    
    # Create time series plot
    p <- ggplot(data, aes_string(x = date_col, y = input$env_variable)) +
      geom_line(color = "#2874A6", group = 1) +  # group=1 ensures line connects all points
      geom_point(color = "#2874A6", size = 3) +
      labs(
        title = paste("Variable Ambiental:", input$env_variable),
        subtitle = paste("Entidad:", input$env_entity),
        x = "Fecha",
        y = input$env_variable
      ) +
      theme_minimal() +
      theme(
        text = element_text(family = "Inter"),
        plot.title = element_text(size = 16, color = "#1a365d", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(size = 12, color = "#2d3748"),
        axis.text = element_text(size = 10, color = "#4a5568"),
        axis.text.x = element_text(angle = 45, hjust = 1) # Angle the x labels for better readability
      )
    
    ggplotly(p) %>% 
      layout(margin = list(l = 50, r = 50, b = 80, t = 75)) # Increased bottom margin for angled labels
  })
  
  # 3. Species Risk -----------------------------------------------------------
  
  # List available species with risk images
  risk_species <- reactive({
    risk_files <- list_files_with_ext("data/riesgo_sp", "png")
    species <- sapply(risk_files, extract_filename)
    return(species)
  })
  
  # Update species selector for risk images
  observe({
    species <- risk_species()
    updateSelectInput(session, "risk_species", 
                      choices = species,
                      selected = if(length(species) > 0) species[1] else NULL,
                      label = "Seleccionar Especie:")
  })
  
  # Render species risk image
  output$risk_image <- renderUI({
    req(input$risk_species)
    
    img_path <- file.path("data/riesgo_sp", paste0(input$risk_species, ".png"))
    if (!file.exists(img_path)) {
      return(HTML("<div class='alert alert-warning'>Imagen no encontrada para la especie seleccionada.</div>"))
    }
    
    tags$div(
      style = "text-align: center;",
      tags$h4(paste("Evaluación de riesgo para", input$risk_species)),
      tags$img(src = img_path, width = "80%", class = "img-responsive")
    )
  })
  
  # 4. Potential Fishing Areas Maps -------------------------------------------
  
  # List available localities with maps
  fishing_localities <- reactive({
    map_files <- list_files_with_ext("data/mapas_areas_pesca", "jpg")
    localities <- sapply(map_files, extract_filename)
    return(localities)
  })
  
  # Update locality selector for fishing area maps
  observe({
    localities <- fishing_localities()
    updateSelectInput(session, "fishing_locality", 
                      choices = localities,
                      selected = if(length(localities) > 0) localities[1] else NULL,
                      label = "Seleccionar Localidad:")
  })
  
  # Render fishing area map
  output$fishing_map <- renderUI({
    req(input$fishing_locality)
    
    img_path <- file.path("data/mapas_areas_pesca", paste0(input$fishing_locality, ".jpg"))
    if (!file.exists(img_path)) {
      return(HTML("<div class='alert alert-warning'>Map not found for selected locality.</div>"))
    }
    
    tags$div(
      style = "text-align: center;",
      tags$h4(paste("Potential fishing areas for", input$fishing_locality)),
      tags$img(src = img_path, width = "80%", class = "img-responsive")
    )
  })
  
  # 5. Adaptation Actions -----------------------------------------------------
  
  # Load adaptation actions data
  adaptation_data <- reactive({
    safe_read_csv("data/acc_adap/adaptacion.csv")
  })
  
  # Get unique cooperatives for selector
  adaptation_cooperatives <- reactive({
    data <- adaptation_data()
    if (!is.null(data) && "cooperativa" %in% colnames(data)) {
      return(sort(unique(data$cooperativa)))
    }
    return(character(0))
  })
  
  # Update cooperative selector for adaptation actions
  observe({
    cooperatives <- adaptation_cooperatives()
    updateSelectInput(session, "adaptation_cooperative", 
                      choices = c("All" = "all", cooperatives),
                      selected = "all")
  })
  
  # Filter adaptation data based on selected cooperative
  filtered_adaptation_data <- reactive({
    data <- adaptation_data()
    if (is.null(data)) return(NULL)
    
    if (input$adaptation_cooperative != "all") {
      data <- data %>% filter(cooperativa == input$adaptation_cooperative)
    }
    
    return(data)
  })
  
  # Render adaptation actions table
  output$adaptation_table <- renderDT({
    data <- filtered_adaptation_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame(Message = "No adaptation actions data available"))
    }
    
    datatable(
      data,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      filter = 'top',
      class = 'cell-border stripe'
    )
  })
  
  # 6. Population Growth and Size ---------------------------------------------
  
  # Load population growth data
  population_growth_data <- reactive({
    safe_read_csv("data/crec_tam_pob/tabla9.csv")
  })
  
  # Load population size data
  population_size_data <- reactive({
    safe_read_csv("data/crec_tam_pob/tabla10.csv")
  })
  
  # Get unique species for selector
  population_species <- reactive({
    growth_data <- population_growth_data()
    size_data <- population_size_data()
    
    species <- character(0)
    
    if (!is.null(growth_data) && "species" %in% colnames(growth_data)) {
      species <- c(species, unique(growth_data$species))
    }
    
    if (!is.null(size_data) && "species" %in% colnames(size_data)) {
      species <- c(species, unique(size_data$species))
    }
    
    return(sort(unique(species)))
  })
  
  # Update species selector for population data
  observe({
    species <- population_species()
    updateSelectInput(session, "population_species", 
                      choices = c("All" = "all", species),
                      selected = "all")
  })
  
  # Combine and filter population data
  filtered_population_data <- reactive({
    growth_data <- population_growth_data()
    size_data <- population_size_data()
    
    if (is.null(growth_data) || is.null(size_data)) return(NULL)
    
    # Add data type indicator
    growth_data$data_type <- "Growth Rate"
    size_data$data_type <- "Population Size (Carrying Capacity)"
    
    # Combine data
    combined_data <- rbind(growth_data, size_data)
    
    # Filter by species if needed
    if (input$population_species != "all") {
      combined_data <- combined_data %>% filter(species == input$population_species)
    }
    
    return(combined_data)
  })
  
  # Render population data table
  output$population_table <- renderDT({
    data <- filtered_population_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame(Message = "No population data available"))
    }
    
    datatable(
      data,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      filter = 'top',
      class = 'cell-border stripe'
    )
  })
  
  # 7. Capture Determinants ---------------------------------------------------
  
  # Load capture determinants data
  capture_data <- reactive({
    safe_read_csv("data/det_cap/determinantes_captura.csv")
  })
  
  # Get unique species for selector
  capture_species <- reactive({
    data <- capture_data()
    if (!is.null(data) && "species" %in% colnames(data)) {
      return(sort(unique(data$species)))
    }
    return(character(0))
  })
  
  # Update species selector for capture determinants
  observe({
    species <- capture_species()
    updateSelectInput(session, "capture_species", 
                      choices = c("All" = "all", species),
                      selected = "all")
  })
  
  # Filter capture data based on selected species
  filtered_capture_data <- reactive({
    data <- capture_data()
    if (is.null(data)) return(NULL)
    
    if (input$capture_species != "all") {
      data <- data %>% filter(species == input$capture_species)
    }
    
    return(data)
  })
  
  # Render capture determinants table
  output$capture_table <- renderDT({
    data <- filtered_capture_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame(Message = "No capture determinants data available"))
    }
    
    datatable(
      data,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      filter = 'top',
      class = 'cell-border stripe'
    )
  })
  
  # 8. Regional Vulnerability -------------------------------------------------
  
  # Load vulnerability data from SQLite database
  vulnerability_data <- reactive({
    db_path <- "data/vuln_reg/vuln_reg.db"
    query <- "
      SELECT L.CVE_LOC, L.NOM_LOC, M.NOM_MUN, E.NOM_ENT, 
           L.deci_lat, L.deci_lon, L.POBTOT,
           R.id_scenario, S.scenario_name,
           R.Vulnerabilidad, R.Adaptabilidad, R.Exposicion, R.Sensibilidad
      FROM RESULTS R
      JOIN LOCALIDAD L ON R.CVE_LOC = L.CVE_LOC
      JOIN MUNICIPIO M ON L.CVE_MUN = M.CVE_MUN
      JOIN ENTIDAD E ON M.CVE_ENT = E.CVE_ENT
      JOIN SCENARIO S ON R.id_scenario = S.id_scenario
    "
    load_db_data(db_path, query)
  })
  
  # Get unique entities for selector
  vulnerability_entities <- reactive({
    data <- vulnerability_data()
    if (!is.null(data)) {
      return(sort(unique(data$NOM_ENT)))
    }
    return(character(0))
  })
  
  # Update entity selector for vulnerability map
  observe({
    entities <- vulnerability_entities()
    updateSelectInput(session, "vuln_entity", 
                      choices = c("All" = "all", entities),
                      selected = "all")
  })
  
  # Get municipalities for selected entity
  vulnerability_municipalities <- reactive({
    data <- vulnerability_data()
    if (is.null(data) || input$vuln_entity == "all") return(character(0))
    
    data %>% 
      filter(NOM_ENT == input$vuln_entity) %>%
      pull(NOM_MUN) %>%
      unique() %>%
      sort()
  })
  
  # Update municipality selector for vulnerability map
  observe({
    municipalities <- vulnerability_municipalities()
    updateSelectInput(session, "vuln_municipality", 
                      choices = c("All" = "all", municipalities),
                      selected = "all")
  })
  
  # Filter vulnerability data based on selections
  filtered_vulnerability_data <- reactive({
    data <- vulnerability_data()
    if (is.null(data)) return(NULL)
    
    # Filter by scenario
    data <- data %>% filter(scenario_name == input$vuln_scenario)
    
    # Filter by entity if selected
    if (input$vuln_entity != "all") {
      data <- data %>% filter(NOM_ENT == input$vuln_entity)
      
      # Filter by municipality if selected
      if (input$vuln_municipality != "all") {
        data <- data %>% filter(NOM_MUN == input$vuln_municipality)
      }
    }
    
    return(data)
  })
  
  # Render vulnerability map
  output$vulnerability_map <- renderLeaflet({
    data <- filtered_vulnerability_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(leaflet() %>% 
               addTiles() %>% 
               setView(lng = -102.5, lat = 23.6, zoom = 5) %>%
               addControl(
                 html = "<div class='alert alert-warning'>No vulnerability data available for selected filters</div>",
                 position = "topright"
               ))
    }
    
    # Calculate population range for circle sizing
    pop_range <- range(data$POBTOT, na.rm = TRUE)
    radius_scale <- function(x) {
      scales::rescale(x, to = c(5, 20), from = pop_range)
    }
    
    # Create color palette based on selected variable
    pal <- colorNumeric("RdYlGn", domain = range(data[[input$vuln_variable]], na.rm = TRUE), reverse = TRUE)
    
    # Variable names mapping for popup
    var_names <- c(
      "Vulnerabilidad" = "Vulnerability",
      "Adaptabilidad" = "Adaptability",
      "Exposicion" = "Exposure",
      "Sensibilidad" = "Sensitivity"
    )
    
    # Create the map
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~deci_lon,
        lat = ~deci_lat,
        radius = ~radius_scale(POBTOT),
        popup = ~paste(
          "<strong>Entity:</strong>", NOM_ENT, "<br>",
          "<strong>Municipality:</strong>", NOM_MUN, "<br>",
          "<strong>Locality:</strong>", NOM_LOC, "<br>",
          "<strong>Population:</strong>", format(POBTOT, big.mark = ","), "<br>",
          "<strong>", var_names[input$vuln_variable], ":</strong> ", 
          round(get(input$vuln_variable), 3)
        ),
        color = ~pal(get(input$vuln_variable)),
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 1
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~get(input$vuln_variable),
        title = paste(var_names[input$vuln_variable], "-", input$vuln_scenario),
        opacity = 0.7
      )
  })
  
  # Render vulnerability scatter plot
  output$vulnerability_scatter <- renderPlotly({
    data <- filtered_vulnerability_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    
    # Create the scatter plot
    p <- ggplot(data,
                aes(x = Adaptabilidad,
                    y = Sensibilidad,
                    size = POBTOT,
                    color = Exposicion,
                    text = paste0(
                      "Locality: ", NOM_LOC, "<br>",
                      "Population: ", format(POBTOT, big.mark = ","), "<br>",
                      "Adaptability: ", round(Adaptabilidad, 3), "<br>",
                      "Vulnerability: ", round(Vulnerabilidad, 3), "<br>",
                      "Sensitivity: ", round(Sensibilidad, 3), "<br>",
                      "Exposure: ", round(Exposicion, 3)
                    ))) +
      geom_point(alpha = 0.7) +
      scale_size_continuous(name = "Total Population", 
                           range = c(2, 12),
                           breaks = c(5000, 25000, 100000, 200000)) +
      scale_color_viridis_c(name = "Exposure", 
                           option = "magma", 
                           direction = -1) +
      labs(
        title = "Vulnerability Components Analysis",
        subtitle = paste("Scenario:", input$vuln_scenario),
        x = "Adaptability",
        y = "Sensitivity"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
    
    # Add facet_wrap only if multiple entities are present
    if (length(unique(data$NOM_ENT)) > 1) {
      p <- p + facet_wrap(~ NOM_ENT, ncol = 2)
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(l = 50, r = 50, b = 50, t = 75))
  })
  
  # 9. Distribution Changes ---------------------------------------------------
  
  # List available species with distribution HTML maps
  dist_html_species <- reactive({
    map_files <- list.files("data/cam_dist/mod_dist", pattern = "\\.html$", full.names = FALSE)
    species <- tools::file_path_sans_ext(map_files)
    return(species)
  })
  
  # Update species selector for HTML distribution maps
  observe({
    species <- dist_html_species()
    updateSelectInput(session, "dist_html_species", 
                      choices = species,
                      selected = if(length(species) > 0) species[1] else NULL)
  })
  
  # Render HTML distribution map
  output$dist_html_map <- renderUI({
    req(input$dist_html_species)
    
    html_path <- file.path("data/cam_dist/mod_dist", paste0(input$dist_html_species, ".html"))
    if (!file.exists(html_path)) {
      return(HTML("<div class='alert alert-warning'>Distribution map not found for selected species.</div>"))
    }
    
    # Embed HTML map in an iframe
    tags$iframe(
      src = html_path,
      width = "100%",
      height = "600px",
      style = "border: none;"
    )
  })
  
  # List available species with CSV distribution data
  dist_csv_species <- reactive({
    csv_files <- list_files_with_ext("data/cam_dist/ocurr_dist", "csv")
    species <- sapply(csv_files, extract_filename)
    return(species)
  })
  
  # Update species selector for CSV distribution maps
  observe({
    species <- dist_csv_species()
    updateSelectInput(session, "dist_csv_species", 
                      choices = species,
                      selected = if(length(species) > 0) species[1] else NULL)
  })
  
  # Generate  map from CSV data
  output$dist_csv_map <- renderLeaflet({
    req(input$dist_csv_species)
    
    csv_path <- file.path("data/cam_dist/ocurr_dist", paste0(input$dist_csv_species, ".csv"))
    if (!file.exists(csv_path)) {
      return(leaflet() %>% 
                addTiles() %>% 
                setView(lng = -102.5, lat = 23.6, zoom = 5) %>%
                addControl(
                  html = "<div class='alert alert-warning'>Datos de distribución no encontrados para la especie seleccionada.</div>",
                  position = "topright"
                ))
    }
    
    # Leer el CSV directamente con R
    data <- read.csv(csv_path)
    
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = mean(data$longitude), lat = mean(data$latitude), zoom = 5) %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~paste0(input$dist_csv_species, ": ", location)
      )
  })
  
  # 10. Workshops -------------------------------------------------------------
  
  # Load workshops data
  workshops_data <- reactive({
    safe_read_csv("data/talleres/talleres.csv")
  })
  
  # Get unique cooperatives for selector
  workshops_cooperatives <- reactive({
    data <- workshops_data()
    if (!is.null(data) && "cooperativa" %in% colnames(data)) {
      return(sort(unique(data$cooperativa)))
    }
    return(character(0))
  })
  
  # Update cooperative selector for workshops
  observe({
    cooperatives <- workshops_cooperatives()
    updateSelectInput(session, "workshops_cooperative", 
                      choices = c("All" = "all", cooperatives),
                      selected = "all")
  })
  
  # Filter workshops data based on selected cooperative
  filtered_workshops_data <- reactive({
    data <- workshops_data()
    if (is.null(data)) return(NULL)
    
    if (input$workshops_cooperative != "all") {
      data <- data %>% filter(cooperativa == input$workshops_cooperative)
    }
    
    return(data)
  })
  
  # Render workshops table
  output$workshops_table <- renderDT({
    data <- filtered_workshops_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame(Message = "No workshops data available"))
    }
    
    datatable(
      data,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      filter = 'top',
      class = 'cell-border stripe'
    )
  })
  
  # Download handlers ---------------------------------------------------------
  
  # Download data from current view
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("fisheries_data_", 
             gsub(" ", "_", tolower(input$mainTabs)), "_",
             format(Sys.time(), "%Y%m%d-%H%M"), 
             ".csv")
    },
    content = function(file) {
      # Get data based on current tab
      data <- switch(input$mainTabs,
                     "Executive Summary" = exec_summary_data(),
                     "Environmental Variables" = env_data(),
                     "Adaptation Actions" = filtered_adaptation_data(),
                     "Population Growth and Size" = filtered_population_data(),
                     "Capture Determinants" = filtered_capture_data(),
                     "Regional Vulnerability" = filtered_vulnerability_data(),
                     "Workshops" = filtered_workshops_data(),
                     NULL)
      
      if (!is.null(data)) {
        write.csv(data, file, row.names = FALSE)
      } else {
        write.csv(data.frame(message = "No data available to download"), file, row.names = FALSE)
      }
    }
  )
  
  # Download report in HTML format
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("fisheries_report_", 
             gsub(" ", "_", tolower(input$mainTabs)), "_",
             format(Sys.time(), "%Y%m%d-%H%M"), 
             ".html")
    },
    content = function(file) {
      # Create a temporary file for the report
      temp_report <- file.path(tempdir(), "report_template.Rmd")
      file.copy("report_template.Rmd", temp_report, overwrite = TRUE)
      
      # Parameters for the report based on current tab
      params <- list(
        report_title = paste("Fisheries and Climate Vulnerability Report -", input$mainTabs),
        tab = input$mainTabs,
        data = switch(input$mainTabs,
                     "Executive Summary" = exec_summary_data(),
                     "Environmental Variables" = env_data(),
                     "Adaptation Actions" = filtered_adaptation_data(),
                     "Population Growth and Size" = filtered_population_data(),
                     "Capture Determinants" = filtered_capture_data(),
                     "Regional Vulnerability" = filtered_vulnerability_data(),
                     "Workshops" = filtered_workshops_data(),
                     NULL),
        date = format(Sys.time(), "%B %d, %Y")
      )
      
      # Render the report
      withProgress(message = 'Generating report...', {
        rmarkdown::render(temp_report, 
                          output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv()))
      })
    }
  )
  
  # Element counters ----------------------------------------------------------
  
  # Count species for risk visualization
  output$risk_species_count <- renderText({
    species <- risk_species()
    length(species)
  })
  
  # Count localities for fishing areas
  output$fishing_localities_count <- renderText({
    localities <- fishing_localities()
    length(localities)
  })
  
  # Count cooperatives for adaptation actions
  output$adaptation_cooperatives_count <- renderText({
    cooperatives <- adaptation_cooperatives()
    length(cooperatives)
  })
  
  # Count records in vulnerability data
  output$vulnerability_localities_count <- renderText({
    data <- filtered_vulnerability_data()
    if (is.null(data)) return("0")
    length(unique(data$CVE_LOC))
  })
  
  # Count workshops
  output$workshops_count <- renderText({
    data <- workshops_data()
    if (is.null(data)) return("0")
    nrow(data)
  })
  
  # Information modal handlers ------------------------------------------------
  
  # Show methodology information
  observeEvent(input$show_methodology, {
    showModal(modalDialog(
      title = "Información Metodológica",
      HTML("
        <h4>Fuentes de datos</h4>
        <p>Los datos presentados en este panel provienen de diversas fuentes, incluyendo encuestas de campo, bases de datos gubernamentales y estudios científicos realizados entre 2020 y 2023.</p>
        
        <h4>Evaluación de vulnerabilidad</h4>
        <p>La evaluación de vulnerabilidad sigue el marco establecido por el Panel Intergubernamental sobre el Cambio Climático (IPCC), que define la vulnerabilidad como la medida en que una entidad social puede hacer frente de manera efectiva a los impactos adversos de una perturbación.</p>
        
        <p>La vulnerabilidad se mide mediante la evaluación simultánea de la presión ambiental (exposición), la susceptibilidad al daño y la capacidad de adaptación (la capacidad de los individuos para anticipar, responder y recuperarse de los impactos).</p>
        
        <h4>Escenarios climáticos</h4>
        <p>Utilizamos proyecciones futuras del cambio climático desarrolladas como parte de CMIP6 (Coupled Model Intercomparison Project 6), dentro del Proyecto de Intercomparación de Escenarios (ScenarioMIP); estos escenarios sirven de base para el 6º informe del IPCC.</p>
      "),
      easyClose = TRUE,
      footer = modalButton("Cerrar")
    ))
  })
  
  # Session cleanup ----------------------------------------------------------
  
  # Close database connections when app stops
  onSessionEnded(function() {
    # Cleanup code here if needed
  })
}