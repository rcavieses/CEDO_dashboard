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
library(shinyBS)


# Server function
function(input, output, session) {
  
  # Helper functions ----------------------------------------------------------
  
  # Función para determinar y obtener rutas de archivos
  get_file_paths <- function(base_dir, filename = NULL) {
    # Rutas posibles
    standard_path <- if (is.null(filename)) base_dir else file.path(base_dir, filename)
    www_path <- if (is.null(filename)) file.path("www", base_dir) else file.path("www", base_dir, filename)
    
    # Determinar cuál estructura está siendo usada
    if(dir.exists(file.path("www", base_dir))) {
      # Para verificar existencia de archivos en el sistema
      file_path <- www_path
      # Para URL en el navegador (relativa a www)
      url_path <- if (is.null(filename)) base_dir else file.path(base_dir, filename)
      using_www <- TRUE
    } else {
      file_path <- standard_path
      # En la estructura antigua, usaríamos una ruta especial
      url_path <- standard_path
      using_www <- FALSE
    }
    
    return(list(
      file_path = file_path,      # Ruta del sistema para verificar existencia
      url_path = url_path,        # Ruta URL para navegador
      using_www = using_www       # Indicador de estructura
    ))
  }
  
  # Function to safely read CSV files
  safe_read_csv <- function(file_path, ...) {
    tryCatch({
      read_csv(file_path, ...)
    }, error = function(e) {
      warning(paste("Could not read file:", file_path, "Error:", e$message))
      return(NULL)
    })
  }
  
  # Function to safely read text files
  safe_read_text <- function(file_path) {
    tryCatch({
      readLines(file_path, warn = FALSE) %>% paste(collapse = "\n")
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
  
  # Load executive summary text directly from the summary.txt file
  exec_summary_text <- reactive({
    paths <- get_file_paths("data/resumen/summary.md")
    safe_read_text(paths$file_path)
  })
  
  # Render executive summary text
  output$executive_summary <- renderUI({
    text <- exec_summary_text()
    
    if (is.null(text)) {
      # Try loading from CSV as fallback if text file is not available
      paths <- get_file_paths("data/resumen/summary.csv")
      data <- safe_read_csv(paths$file_path)
      
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
    } else {
      # Process the text from summary.txt
      # Split by lines that look like headers (e.g., "## Section Title")
      sections <- strsplit(text, "(?=^#{2,3} )", perl = TRUE)[[1]]
      
      processed_sections <- lapply(sections, function(section) {
        # Extract the header if it exists
        header_match <- regmatches(section, regexec("^(#{2,3}) (.+)$", section, perl = TRUE))[[1]]
        
        if (length(header_match) > 0) {
          header_text <- header_match[3]
          # Remove the header from the content
          content <- sub("^#{2,3} .+\n", "", section)
          
          # Determine heading level
          heading_level <- nchar(header_match[2])
          heading_tag <- paste0("h", heading_level)
          
          # Return the formatted section
          return(tagList(
            tags$div(class = "summary-section",
                    tags[[heading_tag]](header_text, class = "summary-heading"),
                    HTML(markdown::markdownToHTML(text = content, fragment.only = TRUE))
            )
          ))
        } else {
          # No header, just content
          return(tagList(
            tags$div(class = "summary-section",
                    HTML(markdown::markdownToHTML(text = section, fragment.only = TRUE))
            )
          ))
        }
      })
      
      do.call(tagList, processed_sections)
    }
  })
  
  # 2. Environmental Variables ------------------------------------------------
  
  # List available entities (states) from CSV files
  env_entities <- reactive({
    paths <- get_file_paths("data/var_amb")
    
    if(paths$using_www) {
      # Si estamos usando estructura www
      env_files <- list.files(path = paths$file_path, pattern = "\\.csv$", full.names = FALSE)
      # Extraer solo el nombre del archivo sin extensión
      entities <- tools::file_path_sans_ext(env_files)
    } else {
      # Estructura original
      env_files <- list_files_with_ext("data/var_amb", "csv")
      # Extraer solo el nombre del archivo sin ruta ni extensión
      entities <- sapply(env_files, function(path) {
        basename(tools::file_path_sans_ext(path))
      })
    }
    
    return(entities)
  })
  
  # Update entity selector
  observe({
    entities <- env_entities()
    updateSelectInput(session, "env_entity", 
                      choices = entities,
                      selected = if(length(entities) > 0) entities[1] else NULL,
                      label = "Entidad:")
  })
  
  # Load environmental data for selected entity
  env_data <- reactive({
    req(input$env_entity)
    
    # Create reverse mapping to get filename from display name
    display_to_filename <- c(
      "BCS (Costa este)" = "BCS_este",
      "Nayarit" = "Nayarit",
      "Quintana Roo" = "Quintana_Roo",
      "Yucatan" = "Yucatan"
    )
    
    # Get the actual filename
    filename <- display_to_filename[input$env_entity]
    if(is.na(filename)) filename <- input$env_entity  # Fallback if not found
    
    # Get the correct path with our helper function
    paths <- get_file_paths("data/var_amb", paste0(filename, ".csv"))
    
    data <- safe_read_csv(paths$file_path, locale = locale(encoding = "utf-8"))
    
    if (!is.null(data)) {
      # Create Date column from Año and Mes
      if ("Año" %in% colnames(data) && "Mes" %in% colnames(data)) {
        # Convert Mes to numeric if it's a month name
        if (is.character(data$Mes)) {
          month_names <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                          "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
          month_abbr <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                         "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
          
          # Check if month names are in Spanish
          if (any(data$Mes %in% month_names)) {
            data$Mes_num <- match(data$Mes, month_names)
          } else if (any(data$Mes %in% month_abbr)) {
            data$Mes_num <- match(data$Mes, month_abbr)
          } else {
            # Try to convert directly if it's a number in string format
            data$Mes_num <- as.numeric(data$Mes)
          }
        } else {
          data$Mes_num <- data$Mes
        }
        
        # Create Date as the 15th of each month
        data$Date <- as.Date(paste(data$Año, data$Mes_num, "15", sep="-"), format="%Y-%m-%d")
        
        # Make sure it's sorted chronologically
        data <- data[order(data$Date), ]
      }
    }
    
    return(data)
  })
  
  # Update entity selector with custom display names
  observe({
    entities <- env_entities()
    
    # Create a named vector for display names
    display_names <- c(
      "BCS_este" = "BCS (Costa este)",
      "Nayarit" = "Nayarit",
      "Quintana_Roo" = "Quintana Roo",
      "Yucatan" = "Yucatan"
    )
    
    # Filter out any entities not in our mapping
    valid_entities <- entities[entities %in% names(display_names)]
    
    # Create choices with display names
    choices <- display_names[valid_entities]
    
    updateSelectInput(session, "env_entity", 
                      choices = choices,
                      selected = if(length(choices) > 0) choices[1] else NULL,
                      label = "Entidad:")
  })
  
  # Plot time series for selected entity and variable
  output$env_time_series <- renderPlotly({
    req(input$env_entity, input$env_variables)
    data <- env_data()
    
    if (is.null(data) || length(input$env_variables) == 0) {
      return(NULL)
    }
    
    # Use created Date column for x-axis
    date_col <- "Date"
    
    # Create a plot with multiple variables
    p <- ggplot(data, aes(x = !!sym(date_col))) +
      labs(
        title = paste("Variables Ambientales para", input$env_entity),
        x = "Fecha",
        y = "Valor"
      ) +
      theme_minimal() +
      theme(
        text = element_text(family = "Inter"),
        plot.title = element_text(size = 16, color = "#1a365d", hjust = 0.5),
        axis.title = element_text(size = 12, color = "#2d3748"),
        axis.text = element_text(size = 10, color = "#4a5568"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.title = element_blank()
      ) +
      scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m")
    
    # Colors for different variables
    var_colors <- c(
      "Temperatura (C)" = "#2874A6",
      "Clorofila (mg/m3)" = "#27AE60",
      "PDO" = "#8E44AD",
      "MEI" = "#D35400"
    )
    
    # Add each selected variable to the plot
    for (var in input$env_variables) {
      # Skip if the variable doesn't exist in the data
      if (!var %in% colnames(data)) next

      
      # Add the line for this variable
      p <- p + 
        geom_line(aes(y = !!sym(var), color = var), size = 1) 
    }
    
    # Add the color scale
    p <- p + scale_color_manual(values = var_colors)
    
    # Convert to plotly
    ggplotly(p) %>% 
      layout(margin = list(l = 55, r = 55, b = 78, t = 73))
  })
  
  # 3. Species Risk -----------------------------------------------------------
  
  # # List available species with risk images
  # risk_species <- reactive({
  #   paths <- get_file_paths("data/riesgo_sp")
    
  #   if(paths$using_www) {
  #     # Si estamos usando estructura www
  #     risk_files <- list.files(path = paths$file_path, pattern = "\\.png$", full.names = FALSE)
  #     species <- tools::file_path_sans_ext(risk_files)
  #   } else {
  #     # Estructura original
  #     risk_files <- list_files_with_ext("data/riesgo_sp", "png")
  #     species <- sapply(risk_files, extract_filename)
  #   }
    
  #   return(species)
  # })
  
  # # Update species selector for risk images
  # observe({
  #   species <- risk_species()
  #   updateSelectInput(session, "risk_species", 
  #                     choices = species,
  #                     selected = if(length(species) > 0) species[1] else NULL,
  #                     label = "Seleccionar Especie:")
  # })
  
  # # Render species risk image
  # output$risk_image <- renderUI({
  #   req(input$risk_species)
    
  #   paths <- get_file_paths("data/riesgo_sp", paste0(input$risk_species, ".png"))
    
  #   if (!file.exists(paths$file_path)) {
  #     return(HTML("<div class='alert alert-warning'>Imagen no encontrada para la especie seleccionada.</div>"))
  #   }
    
  #   # Si estamos usando estructura www, la URL es relativa a www
  #   if(paths$using_www) {
  #     img_src <- paths$url_path
  #   } else {
  #     img_src <- paths$file_path
  #   }
    
  #   tags$div(
  #     style = "text-align: center;",
  #     tags$h4(paste("Evaluación de riesgo para", gsub("_", " ", input$risk_species))),
  #     tags$img(src = img_src, width = "80%", class = "img-responsive")
  #   )
  # })
  
  # 4. Potential Fishing Areas Maps -------------------------------------------
  
  # List available localities with maps
  fishing_localities <- reactive({
    paths <- get_file_paths("data/mapas_areas_pesca")
    
    if(paths$using_www) {
      # Si estamos usando estructura www
      map_files <- list.files(path = paths$file_path, pattern = "\\.jpg$", full.names = FALSE)
      # Extraer solo el nombre del archivo sin extensión
      localities <- tools::file_path_sans_ext(map_files)
    } else {
      # Estructura original
      map_files <- list_files_with_ext("data/mapas_areas_pesca", "jpg")
      localities <- sapply(map_files, extract_filename)
    }
    
    # Limpiar nombres: reemplazar guiones bajos por espacios para mejor visualización
    display_names <- gsub("_", " ", localities)
    display_names <- tools::toTitleCase(display_names)
    
    # Crear un vector nombrado donde los nombres son los que se muestran
    # y los valores son los nombres de archivo
    localities_named <- localities
    names(localities_named) <- display_names
    
    return(localities_named)
  })
  
  # Update locality selector for fishing area maps
  observe({
    localities <- fishing_localities()
    
    if(length(localities) > 0) {
      updateSelectInput(session, "fishing_locality", 
                      choices = localities,
                      selected = localities[1],
                      label = "Seleccionar Cooperativa:")
    } else {
      updateSelectInput(session, "fishing_locality", 
                      choices = character(0),
                      label = "Seleccionar Cooperativa: (no se encontraron mapas)")
    }
  })
  
  # Render fishing area map
  output$fishing_map <- renderUI({
    req(input$fishing_locality)
    
    paths <- get_file_paths("data/mapas_areas_pesca", paste0(input$fishing_locality, ".jpg"))
    
    if (!file.exists(paths$file_path)) {
      return(HTML("<div class='alert alert-warning'>Mapa no encontrado para la cooperativa seleccionada.</div>"))
    }
    
    # Si estamos usando estructura www, la URL es relativa a www
    if(paths$using_www) {
      img_src <- paths$url_path
    } else {
      img_src <- paths$file_path
    }
    
    # Obtener el nombre de visualización
    display_name <- names(which(fishing_localities() == input$fishing_locality))
    if(length(display_name) == 0) {
      display_name <- gsub("_", " ", input$fishing_locality)
      display_name <- tools::toTitleCase(display_name)
    }
    
    tags$div(
      style = "text-align: center;",
      tags$h4(paste("Áreas potenciales de pesca para", display_name)),
      tags$img(src = img_src, width = "80%", class = "img-responsive")
    )
  })
  
  # 5. Adaptation Actions -----------------------------------------------------
  # Load adaptation actions data
  adaptation_data <- reactive({
    paths <- get_file_paths("data/acc_adap/adaptacion.csv")
    data <- safe_read_csv(paths$file_path, show_col_types = FALSE)
    
    # Verificar que los datos se cargaron correctamente
    if (is.null(data) || ncol(data) == 0) {
      warning("No se pudo cargar el archivo CSV o está vacío")
      return(NULL)
    }
    
    return(data)
  })

  # Get unique strategy types for radio buttons
  adaptation_types <- reactive({
    data <- adaptation_data()
    if (!is.null(data) && "Tipo de estrategia" %in% colnames(data)) {
      return(sort(unique(data$`Tipo de estrategia`)))
    }
    return(character(0))
  })

  # Get unique timeframes for selector
  adaptation_timeframes <- reactive({
    data <- adaptation_data()
    if (!is.null(data) && "Plazo de implementación" %in% colnames(data)) {
      return(sort(unique(data$`Plazo de implementación`)))
    }
    return(character(0))
  })

  # Update strategy types radio buttons
  observe({
    types <- adaptation_types()
    if(length(types) > 0) {
      choices <- c("Todas" = "all", setNames(types, types))
      updateRadioButtons(session, "adaptation_type", choices = choices, selected = "all")
    }
  })

  # Update timeframe selector
  observe({
    timeframes <- adaptation_timeframes()
    updateSelectInput(session, "adaptation_timeframe", 
                    choices = c("Todos" = "all", timeframes),
                    selected = "all")
  })

  # Filter adaptation data based on selected filters
  filtered_adaptation_data <- reactive({
    data <- adaptation_data()
    if (is.null(data)) return(NULL)
    
    # Filtrar por tipo de estrategia
    if (input$adaptation_type != "all" && "Tipo de estrategia" %in% colnames(data)) {
      data <- data %>% filter(`Tipo de estrategia` == input$adaptation_type)
    }
    
    # Filtrar por plazo de implementación
    if (input$adaptation_timeframe != "all" && "Plazo de implementación" %in% colnames(data)) {
      data <- data %>% filter(`Plazo de implementación` == input$adaptation_timeframe)
    }
    
    return(data)
  })

  # Render adaptation actions table
  output$adaptation_table <- renderDT({
    data <- filtered_adaptation_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame(Mensaje = "No hay datos de adaptación disponibles para los filtros seleccionados"))
    }
    
    # Ordenar por tipo de estrategia
    if ("Tipo de estrategia" %in% colnames(data)) {
      data <- data %>% arrange(`Tipo de estrategia`)
    }
    
    # Limitar registros si no se marca "mostrar todos"
    if (!input$show_all_records && nrow(data) > 10) {
      data <- head(data, 10)
    }
    
    # Seleccionar columnas excluyendo "Tipo de estrategia" (ya filtrado)
    if ("Tipo de estrategia" %in% colnames(data)) {
      data_display <- data %>% 
        select(-`Tipo de estrategia`)
    } else {
      data_display <- data
    }
    
    # Crear tabla con formato similar a la imagen
    dt <- datatable(
      data_display,
      options = list(
        pageLength = if(input$show_all_records) -1 else 10, # -1 muestra todos los registros
        autoWidth = FALSE,            # No ajustar anchos automáticamente
        scrollX = TRUE,               # Scroll horizontal si es necesario
        dom = 'ft',                   # Solo tabla y filtro de búsqueda
        ordering = FALSE,             # Desactivar ordenamiento
        columnDefs = list(
          # Ajustar anchos específicos para cada columna
          list(width = '40%', targets = 0),  # Estrategia (primera columna ahora)
          list(width = '15%', targets = 1),  # Plazo de implementación
          list(width = '15%', targets = 2),  # Vinculacion al PLECCA
          list(width = '30%', targets = 3)   # Actores
        )
      ),
      rownames = FALSE,                # No mostrar números de fila
      filter = 'top',                  # Buscador en la parte superior
      class = 'cell-border stripe',    # Clases CSS para estilo
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left; font-weight: bold; font-size: 1.2em; margin-bottom: 10px;',
        'Acciones de Adaptación'
      )
    )
    
    # Aplicar estilos a las celdas
    dt <- dt %>% formatStyle(
      columns = 0:(ncol(data_display)-1),  # Todas las columnas
      border = '1px solid #ddd',
      borderCollapse = 'collapse'
    )
    
    # Aplicar estilo específico a los plazos si existe la columna
    if ("Plazo de implementación" %in% colnames(data_display)) {
      dt <- dt %>% formatStyle(
        'Plazo de implementación',
        backgroundColor = styleEqual(
          c("Corto", "Mediano", "Largo"),
          c('#f0f8ff', '#f0fff0', '#fff0f0')
        ),
        fontWeight = 'bold'
      )
    }
    
    dt
  })

  # Update help text
  output$adaptation_help_text <- renderUI({
    data <- filtered_adaptation_data()
    count <- if(is.null(data)) 0 else nrow(data)
    
    # Determinar el tipo seleccionado para mostrar
    selected_type <- input$adaptation_type
    if (selected_type == "all") {
      tipo_texto <- "Todas"
    } else {
      tipo_texto <- selected_type
    }
    
    # Determinar el plazo seleccionado para mostrar
    selected_plazo <- input$adaptation_timeframe
    if (selected_plazo == "all") {
      plazo_texto <- "Todos"
    } else {
      plazo_texto <- selected_plazo
    }
    
    div(class = "help-text", 
        "Mostrando ", 
        if(input$show_all_records || count <= 20) {
          paste0(count, " estrategias")
        } else {
          "primeras 20 estrategias (marque 'Mostrar todos los registros' para ver el total)"
        },
        br(),
        "Filtros aplicados: ",
        "Tipo de estrategia: ", strong(tipo_texto),
        " | Plazo: ", strong(plazo_texto)
    )
  })

  # Count for total strategies
  output$adaptation_cooperatives_count <- renderText({
    data <- filtered_adaptation_data()
    if (is.null(data)) return("0")
    nrow(data)
  })
  
  # 6. Population Growth and Size ---------------------------------------------
    
  # Load tabla9 data (environmental sensitivity)
      tabla9_data <- reactive({
        paths <- get_file_paths("data/crec_tam_pob/tabla9.csv")
        safe_read_csv(paths$file_path)
      })
      
      # Load tabla10 data (migration capability)
      tabla10_data <- reactive({
        paths <- get_file_paths("data/crec_tam_pob/tabla10.csv")
        safe_read_csv(paths$file_path)
      })
      
      # Get unique species for selector
      all_species <- reactive({
        tabla9 <- tabla9_data()
        tabla10 <- tabla10_data()
        
        species <- character(0)
        
        if (!is.null(tabla9) && "Especie" %in% colnames(tabla9)) {
          species <- c(species, unique(tabla9$Especie))
        }
        
        if (!is.null(tabla10) && "Especie" %in% colnames(tabla10)) {
          species <- c(species, unique(tabla10$Especie))
        }
        
        return(sort(unique(species)))
      })
      
      # Update species selector - sin opción "Todas" y seleccionando la primera especie por defecto
      observe({
        species <- all_species()
        
        # Seleccionar la primera especie por defecto si hay especies disponibles
        default_selection <- if(length(species) > 0) species[1] else NULL
        
        updateSelectInput(session, "population_species", 
                          choices = species,
                          selected = default_selection)
      })
      
      # Filter and prepare tabla9 data
      filtered_tabla9_data <- reactive({
        data <- tabla9_data()
        
        if (is.null(data) || is.null(input$population_species)) return(NULL)
        
        # Filtrar por especie seleccionada
        data %>% filter(Especie == input$population_species)
      })
      
      # Filter and prepare tabla10 data
      filtered_tabla10_data <- reactive({
        data <- tabla10_data()
        
        if (is.null(data) || is.null(input$population_species)) return(NULL)
        
        # Filtrar por especie seleccionada
        data %>% filter(Especie == input$population_species)
      })
      
      # Función auxiliar para formatear valores y determinar colores
      format_percentage_value <- function(value) {
        # Convertir a numérico por si acaso
        value_num <- as.numeric(value)
        
        # Multiplicar por 100 para mostrar como porcentaje
        value_percent <- value_num * 100
        
        # Determinar color basado en si es positivo o negativo
        color <- ifelse(value_num >= 0, "#3498DB", "#D35400")  # Azul para positivo, naranja para negativo
        
        # Crear HTML con formato
        formatted_value <- paste0(
          "<span style='color: ", color, "; font-size: 28px; font-weight: bold;'>",
          sprintf("%.1f%%", value_percent),  # Formatear a 1 decimal con símbolo de porcentaje
          "</span>"
        )
        
        return(formatted_value)
      }
      
      #  Render combined cards for both tables
      output$combined_cards <- renderUI({
        data9 <- filtered_tabla9_data()
        data10 <- filtered_tabla10_data()
        
        if ((is.null(data9) || nrow(data9) == 0) && (is.null(data10) || nrow(data10) == 0)) {
          return(h4("No hay datos disponibles", style = "color: gray; text-align: center;"))
        }
        
        # Get species name
        species_name <- ""
        if (!is.null(data9) && nrow(data9) > 0) {
          species_name <- data9[1, "Especie"]
        } else if (!is.null(data10) && nrow(data10) > 0) {
          species_name <- data10[1, "Especie"]
        }
        
        # Main container
        tagList(
          # Título con nombre de especie
          div(style = "margin-bottom: 20px; background-color: #1a365d; padding: 15px; border-radius: 10px;",
              h3(species_name, style = "color: white; margin: 0; text-align: center;")
          ),
          
          # Texto explicativo sobre los datos
          div(
            style = "background-color: #f8f9fa; border-left: 4px solid #2874A6; padding: 15px; margin-bottom: 20px; border-radius: 4px;",
            h4("Datos para el horizonte 2050", style = "margin-top: 0; color: #2874A6;"),
            p("Los valores muestran el cambio proyectado para el año 2050 en porcentaje. Se indican en ", 
              tags$span("azul los cambios positivos", style = "color: #3498DB; font-weight: bold;"), 
              " y en ",
              tags$span("naranja los cambios negativos", style = "color: #D35400; font-weight: bold;"), 
              ".")
          ),
          
          # Sensibilidad a Factores Ambientales (Tabla 9)
          div(
            h4("Sensibilidad a Factores Ambientales", style = "margin-bottom: 20px; color: #1a365d;"),
            
            if (is.null(data9) || nrow(data9) == 0) {
              div(h5("No hay datos disponibles para esta especie", style = "color: gray; text-align: center;"))
            } else {
              row <- data9[1, ]
              fluidRow(
                column(4,
                  div(style = "background-color: #edf2f7; border-radius: 8px; padding: 15px; margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                    h4("Temperatura", style = "margin-top: 0; color: #2d3748;"),
                    HTML(format_percentage_value(row$Temperatura))
                  )
                ),
                column(4,
                  div(style = "background-color: #edf2f7; border-radius: 8px; padding: 15px; margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                    h4("Nivel del Mar", style = "margin-top: 0; color: #2d3748;"),
                    HTML(format_percentage_value(row$Nivel_del_mar))
                  )
                ),
                column(4,
                  div(style = "background-color: #edf2f7; border-radius: 8px; padding: 15px; margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                    h4("Acidificación", style = "margin-top: 0; color: #2d3748;"),
                    HTML(format_percentage_value(row$Acidificacion))
                  )
                ),
                column(6,
                  div(style = "background-color: #edf2f7; border-radius: 8px; padding: 15px; margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                    h4("Enfermedades", style = "margin-top: 0; color: #2d3748;"),
                    HTML(format_percentage_value(row$Enfermedades))
                  )
                ),
                column(6,
                  div(style = "background-color: #edf2f7; border-radius: 8px; padding: 15px; margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                    h4("Flujo de Agua Dulce", style = "margin-top: 0; color: #2d3748;"),
                    HTML(format_percentage_value(row$Flujo_de_agua_dulce))
                  )
                )
              )
            }
          ),
          
          # Separador
          tags$hr(style = "margin: 30px 0; border-top: 1px dashed #ccc;"),
          
          # Capacidad de Migración y Otros Factores (Tabla 10)
          div(
            h4("Capacidad de Migración y Otros Factores", style = "margin-bottom: 20px; color: #1a365d;"),
            
            if (is.null(data10) || nrow(data10) == 0) {
              div(h5("No hay datos disponibles para esta especie", style = "color: gray; text-align: center;"))
            } else {
              row <- data10[1, ]
              fluidRow(
                column(4,
                  div(style = "background-color: #edf2f7; border-radius: 8px; padding: 15px; margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                    h4("Nivel del Mar", style = "margin-top: 0; color: #2d3748;"),
                    HTML(format_percentage_value(row$Nivel_del_mar))
                  )
                ),
                column(4,
                  div(style = "background-color: #edf2f7; border-radius: 8px; padding: 15px; margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                    h4("Capacidad de Migración", style = "margin-top: 0; color: #2d3748;"),
                    HTML(format_percentage_value(row$Capacidad_de_migracion))
                  )
                ),
                column(4,
                  div(style = "background-color: #edf2f7; border-radius: 8px; padding: 15px; margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                    h4("Flujo de Agua Dulce", style = "margin-top: 0; color: #2d3748;"),
                    HTML(format_percentage_value(row$Flujo_de_agua_dulce))
                  )
                ),
                column(6,
                  div(style = "background-color: #edf2f7; border-radius: 8px; padding: 15px; margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                    h4("Capturabilidad", style = "margin-top: 0; color: #2d3748;"),
                    HTML(format_percentage_value(row$Capturabilidad))
                  )
                ),
                column(6,
                  div(style = "background-color: #edf2f7; border-radius: 8px; padding: 15px; margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                    h4("Gobernanza", style = "margin-top: 0; color: #2d3748;"),
                    HTML(format_percentage_value(row$Gobernanza))
                  )
                )
              )
            }
          )
        )
      })

  # 7. Capture Determinants ---------------------------------------------------
  
 # Load capture determinants data
  capture_data <- reactive({
    paths <- get_file_paths("data/det_cap", "determinantes_captura.csv")
    safe_read_csv(paths$file_path)
  })
  
  # Get unique species for selector
  capture_species <- reactive({
    data <- capture_data()
    if (!is.null(data) && "Especie" %in% colnames(data)) {
      return(sort(unique(data$Especie)))
    }
    return(character(0))
  })
  
  # Update species selector for capture determinants
  # No mostrar "All" y seleccionar la primera especie por defecto
  observe({
    species <- capture_species()
    
    # Seleccionar la primera especie por defecto si hay especies disponibles
    default_selection <- if(length(species) > 0) species[1] else NULL
    
    updateSelectInput(session, "capture_species", 
                      choices = species,
                      selected = default_selection,
                      label = "Seleccionar Especie:")
  })
  
  # Filter capture data based on selected species
  filtered_capture_data <- reactive({
    data <- capture_data()
    if (is.null(data)) return(NULL)
    
    req(input$capture_species)
    data %>% filter(Especie == input$capture_species)
  })
  
  # Añadir texto explicativo sobre la tabla de determinantes
  output$capture_explanation <- renderUI({
    tags$div(
      class = "explanation-box",
      style = "background-color: #f8f9fa; border-left: 4px solid #2874A6; padding: 15px; margin-bottom: 20px; border-radius: 4px;",
      
      tags$h4("Interpretación de los datos:", style = "margin-top: 0; color: #2874A6;"),
      
      tags$p(
        "Esta tabla muestra los determinantes de captura para la especie seleccionada, destacando:", 
        style = "margin-bottom: 10px;"
      ),
      
      tags$ul(
        tags$li(
          tags$strong("Tendencia temporal:"), 
          "Se muestra la tasa de cambio anual (kg/año) con ", 
          tags$span("valores positivos en verde", style = "color: green; font-weight: bold"), 
          " (aumento de captura) y ",
          tags$span("valores negativos en naranja", style = "color: orange; font-weight: bold"),
          " (disminución)."
        ),
        
        tags$li(
          tags$strong("Factores influyentes:"), 
          "Se divide la varianza explicada entre el esfuerzo pesquero (Días efectivos), condiciones locales (Temperatura y Clorofila) y condiciones regionales (MEI y PDO/ADO)."
        ),
        
        tags$li(
          tags$strong("CPUE:"), 
          "Captura por unidad de esfuerzo (kg/día), otro indicador importante de la productividad pesquera."
        )
      )
    )
  })
  
  # Render capture determinants table with color coding
  output$capture_table <- renderDT({
    data <- filtered_capture_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame(Message = "No hay datos disponibles para esta especie"))
    }
    
    # Renombrar columnas para eliminar guiones bajos
    colnames(data) <- gsub("_", " ", colnames(data))
    
    # Crear la tabla
    dt <- datatable(
      data,
      options = list(
        pageLength = 15,          # Mostrar más filas por página
        autoWidth = TRUE,
        scrollX = TRUE,
        dom = 't',                # Solo mostrar la tabla ('t'), sin filtros ni paginación
        ordering = FALSE,         # Desactivar ordenamiento
        searching = FALSE,        # Desactivar búsqueda
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")  # Centrar todo el texto
        )
      ),
      rownames = FALSE,
      filter = 'none',            # Quitar filtros
      class = 'cell-border stripe',
      selection = 'none',         # Desactivar selección de filas
      caption = htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: left;',
        htmltools::HTML("Se señala en color verde las tendencias positivas (aumento en la captura) y en naranja las negativas basadas en modelos de regresión para cada especie, tendencia de la captura (2018-2023) y división de la varianza explicada entre el esfuerzo pesquero, las condiciones locales (temperatura y concentración de clorofila) y las regionales (MEI y PDO/ADO)")
      )
    )
    
    # Aplicar formato condicional para la columna de tasa de cambio anual
    dt <- dt %>% 
      formatStyle(
        'Tasa de cambio anual kg año',  # Nombre de columna sin guiones bajos
        backgroundColor = styleInterval(
          cuts = 0,  # Valor de corte
          values = c('#FFA07A', '#90EE90')  # Naranja claro para negativo, verde claro para positivo
        ),
        fontWeight = 'bold'
      )
    
    # Aplicar formato condicional para la columna CPUE
    dt <- dt %>% 
      formatStyle(
        'CPUE kg dia efectivo',  # Nombre de columna sin guiones bajos
        backgroundColor = styleInterval(
          cuts = 0,  # Valor de corte
          values = c('#FFA07A', '#90EE90')  # Naranja claro para negativo, verde claro para positivo
        ),
        fontWeight = 'bold'
      )
    
    # Aplicar formato condicional para otras columnas con valores numéricos que representan impacto
    # Para Dias efectivos, Temperatura, Clorofila, MEI y ADO o PDO
    value_columns <- c('Dias efectivos', 'Temperatura', 'Clorofila', 'MEI', 'ADO o PDO')
    
    for (col in value_columns) {
      if (col %in% colnames(data)) {
        dt <- dt %>% 
          formatStyle(
            col,
            background = styleColorBar(
              range(data[[col]], na.rm = TRUE), 
              'lightblue'
            ),
            backgroundSize = '98% 88%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          )
      }
    }
    
    dt
  })
    
  # 8. Regional Vulnerability -------------------------------------------------

  # Load vulnerability data from SQLite database
  vulnerability_data <- reactive({
    paths <- get_file_paths("data/vuln_reg/vuln_reg.db")
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
      WHERE E.NOM_ENT IN ('Baja California Sur', 'Baja California', 'Quintana Roo', 'Yucatán')
    "
    load_db_data(paths$file_path, query)
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
               setView(lng = -102.5, lat = 23.0, zoom = 6) %>%
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
      "Vulnerabilidad" = "Vulnerabilidad",
      "Adaptabilidad" = "Adaptabilidad",
      "Exposicion" = "Exposicion",
      "Sensibilidad" = "Sensibilidad"
    )
    
    # Create the map
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~deci_lon,
        lat = ~deci_lat,
        radius = ~radius_scale(POBTOT),
        popup = ~paste(
          "<strong>Estado:</strong>", NOM_ENT, "<br>",
          "<strong>Municipalidad:</strong>", NOM_MUN, "<br>",
          "<strong>Localidad:</strong>", NOM_LOC, "<br>",
          "<strong>Población:</strong>", format(POBTOT, big.mark = ","), "<br>",
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
                      "Localidad: ", NOM_LOC, "<br>",
                      "Población: ", format(POBTOT, big.mark = ","), "<br>",
                      "Adaptabilidad: ", round(Adaptabilidad, 3), "<br>",
                      "Vulnerabilidad: ", round(Vulnerabilidad, 3), "<br>",
                      "Sensibilidad: ", round(Sensibilidad, 3), "<br>",
                      "Exposición: ", round(Exposicion, 3)
                    ))) +
      geom_point(alpha = 0.7) +
      scale_size_continuous(name = "Población total", 
                           range = c(2, 12),
                           breaks = c(5000, 25000, 100000, 200000)) +
      scale_color_viridis_c(name = "Exposición", 
                           option = "magma", 
                           direction = -1) +
      labs(
        
        subtitle = paste("Escenario:", input$vuln_scenario),
        x = "Adaptabilidad",
        y = "Sensibilidad"
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

  # (a) Leemos la tabla7.csv para usarla en la info filtrada
  tabla7_data <- reactive({
    csv_path <- "data/cam_dist/tabla7.csv"  # Ajusta según tu carpeta real
    
    # Checar si existe
    if (!file.exists(csv_path)) {
      warning("No se encontró el archivo: ", csv_path)
      return(NULL)
    }
    
    # Leer de manera segura
    df <- safe_read_csv(csv_path)
    df
  })

  # (b) Generamos la lista unificada de especies:
  dist_species <- reactive({
    # 1. Archivos HTML (en carpeta www)
    html_files <- list.files(
      "www/data/cam_dist/mod_dist",
      pattern = "\\.html$",
      full.names = FALSE
    )
    sp_html_raw <- tools::file_path_sans_ext(html_files)
    sp_html <- gsub("_", " ", sp_html_raw)
    
    # 2. Archivos CSV (fuera de www)
    csv_files <- list.files(
      "data/cam_dist/ocurr_dist",
      pattern = "\\.csv$",
      full.names = FALSE
    )
    sp_csv_raw <- tools::file_path_sans_ext(csv_files)
    sp_csv <- gsub("_", " ", sp_csv_raw)
    
    # 3. Unimos y ordenamos
    especies <- union(sp_html, sp_csv)
    sort(especies)
  })

  # (c) Filtramos tabla7 para la especie seleccionada
  filtered_tabla7 <- reactive({
    req(input$dist_species)
    df <- tabla7_data()
    if (is.null(df)) return(NULL)
    
    # Filtrar filas de la especie (en la columna "Especie", que tiene espacios)
    df_filtrado <- df[df$Especie == input$dist_species, ]
    cols_requeridas <- c("Porcentaje_cambio_SSP245", "Porcentaje_cambio_SSP585")
    cols_encontradas <- intersect(cols_requeridas, names(df_filtrado))
    if (length(cols_encontradas) == 0) {
      return(NULL)
    } else {
      df_filtrado <- df_filtrado[, cols_encontradas, drop = FALSE]
    }
    
    df_filtrado
  })

  # (d) Renderizamos dos 'cajas' que muestran Porcentaje_cambio_SSP245 y Porcentaje_cambio_SSP585
  output$tabla7_boxes <- renderUI({
    data <- filtered_tabla7()
    
    # Si no hay registros o no existen las columnas, mostramos advertencia
    if (is.null(data) || nrow(data) == 0) {
      return(HTML("<div class='alert alert-warning'>No hay registros en tabla7 para esta especie o faltan columnas requeridas.</div>"))
    }
    
    # Tomamos la primera fila (por si hubiera más de una)
    row <- data[1, , drop = FALSE]
    
    val245 <- if ("Porcentaje_cambio_SSP245" %in% names(row)) row[["Porcentaje_cambio_SSP245"]] else NA
    val585 <- if ("Porcentaje_cambio_SSP585" %in% names(row)) row[["Porcentaje_cambio_SSP585"]] else NA
    
    fluidRow(
      column(
        width = 6,
        div(
          style = "background-color: #edf2f7; border-left: 5px solid #2874A6; 
                  border-radius: 8px; padding: 15px; margin-bottom: 20px;",
          h4("Porcentaje cambio SSP245", style = "margin-top: 0;"),
          div(
            style = "font-size: 28px; font-weight: bold; color: #2874A6;",
            paste0(val245, if (!is.na(val245) && is.numeric(val245)) "%" else "")
          )
        )
      ),
      column(
        width = 6,
        div(
          style = "background-color: #edf2f7; border-left: 5px solid #D35400; 
                  border-radius: 8px; padding: 15px; margin-bottom: 20px;",
          h4("Porcentaje cambio SSP585", style = "margin-top: 0;"),
          div(
            style = "font-size: 28px; font-weight: bold; color: #D35400;",
            paste0(val585, if (!is.na(val585) && is.numeric(val585)) "%" else "")
          )
        )
      )
    )
  })

  # (e) Observador para actualizar el selectInput con las especies en 'modo espacios'
  observe({
    species_list <- dist_species()
    updateSelectInput(
      session,
      "dist_species",
      choices = species_list,
      selected = if (length(species_list) > 0) species_list[1] else NULL
    )
  })

  # (f) Mapa HTML embebido
  #     Convertimos espacios a '_' para encontrar el .html en la carpeta www
  output$dist_html_map <- renderUI({
    req(input$dist_species)
    
    sp_file <- gsub(" ", "_", input$dist_species)
    html_path <- file.path("www/data/cam_dist/mod_dist", paste0(sp_file, ".html"))
    
    if (!file.exists(html_path)) {
      return(HTML("<div class='alert alert-warning'>No se encontró el archivo HTML para esta especie.</div>"))
    }
    
    # Para el src, removemos el 'www/' para que sea relativo a la raíz de la app
    relative_url <- sub("^www/", "", html_path)
    
    tags$iframe(
      src = relative_url,
      width = "100%",
      height = "600px",
      style = "border: none;"
    )
  })

  # (g) Mapa CSV (Leaflet)
  #     Convertimos espacios a '_' para el .csv
  output$dist_csv_map <- renderLeaflet({
    req(input$dist_species)
    
    sp_file <- gsub(" ", "_", input$dist_species)
    csv_path <- file.path("data/cam_dist/ocurr_dist", paste0(sp_file, ".csv"))
    
    if (!file.exists(csv_path)) {
      return(
        leaflet() %>%
          addTiles() %>%
          setView(lng = -102.5, lat = 23.6, zoom = 5) %>%
          addControl(
            html = "<div class='alert alert-warning'>No se encontró el CSV de ocurrencia para esta especie.</div>",
            position = "topright"
          )
      )
    }
    
    data <- read.csv(csv_path)
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(
        lng = mean(data$Longitud, na.rm = TRUE),
        lat = mean(data$Latitud, na.rm = TRUE),
        zoom = 5
      ) %>%
      addCircleMarkers(
        lng = ~Longitud,
        lat = ~Latitud,
        popup = paste("Especie:", input$dist_species),
        radius = 5,
        fillOpacity = 0.8,
        color = "#3D5A80",
        weight = 1
      )
  })

  # (h) UI que muestra ambos mapas en columnas
  output$distribution_maps <- renderUI({
    req(input$dist_species)
    
    fluidRow(
      column(
        width = 6,
        uiOutput("dist_html_map")
      ),
      column(
        width = 6,
        leafletOutput("dist_csv_map", height = "600px")
      )
    )
  })
  # 10. Workshops -------------------------------------------------------------
  # Load workshop data
  workshops_data <- reactive({
    paths <- get_file_paths("data/talleres", "talleres.csv") 
    df <- safe_read_csv(paths$file_path)
    df
  })

  # Get unique cooperatives list - limpia y normalizada para el selector
  workshops_cooperatives <- reactive({
    data <- workshops_data()
    if (is.null(data)) return(character(0))
    
    if (!"Cooperativa" %in% colnames(data)) {
      warning("Column 'Cooperativa' not found. Check CSV structure.")
      return(character(0))
    }
    
    # Normalizar nombres para comparación
    cleaned_names <- trimws(data$Cooperativa)  # Eliminar espacios en blanco al inicio y final
    
    # Obtener valores únicos (conservando el primer ejemplo de cada nombre)
    unique_coops <- cleaned_names[!duplicated(cleaned_names)]
    
    # Ordenar alfabéticamente para el selector
    sort(unique_coops)
  })

  # Update cooperative selector con la lista limpia
  observe({
    cooperatives <- workshops_cooperatives()
    updateSelectInput(
      session,
      "workshops_cooperative",
      choices = cooperatives,
      selected = if (length(cooperatives) > 0) cooperatives[1] else NULL
    )
  })

  # Filter data by selected cooperative - modificado para buscar coincidencias
  filtered_workshops_data <- reactive({
    data <- workshops_data()
    if (is.null(data)) return(NULL)
    
    req(input$workshops_cooperative)
    
    # Normalizar el nombre seleccionado y los nombres en el dataset
    selected_coop_clean <- trimws(input$workshops_cooperative)
    data_coops_clean <- trimws(data$Cooperativa)
    
    # Filtrar registros donde el nombre limpio coincida con el seleccionado
    data[data_coops_clean == selected_coop_clean, , drop = FALSE]
  })
  # Define column groupings
  info_general_cols <- c("Grupo de acción", "Tipo de acción", "Origen de la acción")
  acciones_cols     <- c("Acción recomendada", "Plazo")
  impacto_cols      <- c("Vinculación al PLECCA", "Impacto esperado")

  # Render main workshop panel
  output$workshops_panel <- renderUI({
    data <- filtered_workshops_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(HTML("<div class='alert alert-warning'>No hay datos para la cooperativa seleccionada.</div>"))
    }
    
    # Cooperative title
    coop_title <- h2(paste("Cooperativa:", input$workshops_cooperative))
    
    # Get unique action groups
    grupos_accion <- NULL
    if ("Grupo de acción" %in% colnames(data)) {
      grupos_accion <- unique(data[["Grupo de acción"]])
    } else {
      return(HTML("<div class='alert alert-warning'>Column 'Grupo de acción' not found in data.</div>"))
    }
    
    # Create a block for each action group
    bloques_grupos <- lapply(grupos_accion, function(grupo) {
      # Filter rows for this group
      filas_grupo <- data[data[["Grupo de acción"]] == grupo, , drop = FALSE]
      
      # Create container div for this group
      div(
        style = "background-color: #f5f7fa; border-radius: 8px; padding: 20px; margin-bottom: 25px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
        h3(grupo, style = "color: var(--primary-blue); border-bottom: 2px solid var(--border-color); padding-bottom: 10px;"),
        
        # For each row in this group, create an action panel
        lapply(1:nrow(filas_grupo), function(i) {
          row_data <- filas_grupo[i, , drop = FALSE]
          
          div(
            style = "background-color: white; border-radius: 6px; padding: 15px; margin-bottom: 15px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
            
            # General information section
            div(
              style = "margin-bottom: 15px;",
              h4("Información general", style = "color: #2874A6; margin-top: 0; font-size: 16px;"),
              fluidRow(
                lapply(info_general_cols, function(col) {
                  if (col != "Grupo de acción" && col %in% colnames(row_data)) {
                    column(
                      width = 6,
                      div(
                        style = "border-left: 4px solid #2874A6; padding-left: 10px; margin-bottom: 10px;",
                        h5(col, style = "margin-top: 0; margin-bottom: 5px; font-size: 14px;"),
                        div(style = "font-weight: bold;", as.character(row_data[[col]]))
                      )
                    )
                  }
                })
              )
            ),
            
            # Recommended action section
            div(
              style = "margin-bottom: 15px;",
              h4("Acción recomendada", style = "color: #27AE60; margin-top: 0; font-size: 16px;"),
              fluidRow(
                column(
                  width = 8,
                  div(
                    style = "border-left: 4px solid #27AE60; padding-left: 10px; margin-bottom: 10px;",
                    h5("Acción", style = "margin-top: 0; margin-bottom: 5px; font-size: 14px;"),
                    div(style = "font-weight: bold;", if ("Acción recomendada" %in% colnames(row_data)) as.character(row_data[["Acción recomendada"]]) else "N/A")
                  )
                ),
                column(
                  width = 4,
                  div(
                    style = "border-left: 4px solid #27AE60; padding-left: 10px; margin-bottom: 10px;",
                    h5("Plazo", style = "margin-top: 0; margin-bottom: 5px; font-size: 14px;"),
                    div(style = "font-weight: bold;", if ("Plazo" %in% colnames(row_data)) as.character(row_data[["Plazo"]]) else "N/A")
                  )
                )
              )
            ),
            
            # Impact and linkage section
            div(
              style = "margin-bottom: 10px;",
              h4("Impacto y vinculación", style = "color: #D35400; margin-top: 0; font-size: 16px;"),
              fluidRow(
                lapply(impacto_cols, function(col) {
                  if (col %in% colnames(row_data)) {
                    column(
                      width = 6,
                      div(
                        style = "border-left: 4px solid #D35400; padding-left: 10px; margin-bottom: 10px;",
                        h5(col, style = "margin-top: 0; margin-bottom: 5px; font-size: 14px;"),
                        div(style = "font-weight: bold;", as.character(row_data[[col]]))
                      )
                    )
                  }
                })
              )
            )
          )
        })
      )
    })
    
    # Build final output
    tagList(
      coop_title,
      bloques_grupos
    )
  })
  
  # Count workshops
  output$workshops_count <- renderText({
    data <- filtered_workshops_data()
    if (is.null(data)) return("0")
    nrow(data)
  })

  # 11. Catches Component ------------------------------------------------------

    # Leer archivos CSV desde la carpeta
    catches_data <- reactive({
      csv_files <- list.files(path = "data/cap", pattern = "\\.csv$", full.names = TRUE)
      
      all_data <- lapply(csv_files, function(file_path) {
        tryCatch({
          # Extraer nombre del archivo
          file_name <- basename(file_path)
          
          # Buscar patrón: Sp_NOMBRE_St_ENTIDAD.csv
          match <- regexpr("Sp_(.*?)_St_(.*?)\\.csv", file_name, perl = TRUE)
          if (match == -1) return(NULL)
          
          captures <- regmatches(file_name, match)
          matches <- regmatches(file_name, regexec("Sp_(.*?)_St_(.*?)\\.csv", file_name, perl = TRUE))[[1]]
          
          species <- gsub("_", " ", matches[2])
          state <- gsub("_", " ", matches[3])
          
          list(
            file_path = file_path,
            species = species,
            state = state
          )
        }, error = function(e) {
          warning(paste("Error processing filename:", file_path, "Error:", e$message))
          return(NULL)
        })
      })
      
      all_data <- all_data[!sapply(all_data, is.null)]
      return(all_data)
    })

    # Extraer entidades únicas
    catches_entities <- reactive({
      data <- catches_data()
      if (length(data) == 0) return(character(0))
      
      states <- sapply(data, function(x) x$state)
      sort(unique(states))
    })

    # Actualizar selectInput de entidades
    observe({
      entities <- catches_entities()
      updateSelectInput(session, "catches_entity", 
                        choices = entities,
                        selected = if(length(entities) > 0) entities[1] else NULL)
    })

    # Filtrar por entidad seleccionada
    filtered_by_entity <- reactive({
      req(input$catches_entity)
      Filter(function(x) x$state == input$catches_entity, catches_data())
    })

    # Extraer especies disponibles para la entidad seleccionada
    catches_species <- reactive({
      data <- filtered_by_entity()
      if (length(data) == 0) return(character(0))
      
      species <- sapply(data, function(x) x$species)
      sort(unique(species))
    })

    # Actualizar selectInput de especies
    observe({
      species <- catches_species()
      updateSelectInput(session, "catches_species", 
                        choices = species,
                        selected = if(length(species) > 0) species[1] else NULL)
    })

    # Leer archivo para especie seleccionada
    selected_species_data <- reactive({
      req(input$catches_entity, input$catches_species)
      selected_file <- Filter(function(x) x$state == input$catches_entity && x$species == input$catches_species, catches_data())
      if (length(selected_file) == 0) return(NULL)
      
      file_path <- selected_file[[1]]$file_path
      
      # Leer CSV con opciones adicionales para manejo de errores
      data <- tryCatch({
        # Intentar leer con readr para mejor manejo de columnas
        if (requireNamespace("readr", quietly = TRUE)) {
          data <- readr::read_csv(file_path, show_col_types = FALSE)
          # Convertir a data.frame
          data <- as.data.frame(data)
        } else {
          # Alternativa con utils::read.csv
          data <- utils::read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
        }
        data
      }, error = function(e) {
        warning(paste("No se pudo leer el archivo:", file_path, "Error:", e$message))
        return(NULL)
      })
      
      if (is.null(data)) return(NULL)
      
      # Verificar columnas necesarias de manera más flexible
      expected_cols <- c("Año", "Mes", "PESO DESEMBARCADO_KILOGRAMOS", "DIAS EFECTIVOS", "NUMERO EMBARCACIONES")
      actual_cols <- colnames(data)
      
      # Imprimir nombres de columnas para diagnóstico
      message("Columnas encontradas: ", paste(actual_cols, collapse = ", "))
      
      # Verificar si faltan columnas
      if (!all(expected_cols %in% actual_cols)) {
        # Intentar mapeo flexible ignorando mayúsculas/minúsculas, espacios, acentos
        col_map <- list(
          "año" = "Año",
          "ano" = "Año",
          "anio" = "Año",
          "mes" = "Mes",
          "peso" = "PESO DESEMBARCADO_KILOGRAMOS",
          "peso desembarcado" = "PESO DESEMBARCADO_KILOGRAMOS",
          "peso_desembarcado" = "PESO DESEMBARCADO_KILOGRAMOS",
          "peso desembarcado_kilogramos" = "PESO DESEMBARCADO_KILOGRAMOS",
          "dias" = "DIAS EFECTIVOS",
          "dias efectivos" = "DIAS EFECTIVOS",
          "dias_efectivos" = "DIAS EFECTIVOS",
          "numero" = "NUMERO EMBARCACIONES",
          "numero embarcaciones" = "NUMERO EMBARCACIONES",
          "numero_embarcaciones" = "NUMERO EMBARCACIONES",
          "embarcaciones" = "NUMERO EMBARCACIONES"
        )
        
        # Normalizar nombres de columnas
        norm_cols <- tolower(gsub("[[:space:]_]", "", actual_cols))
        mapped_cols <- actual_cols
        
        # Intentar mapear las columnas
        for (i in seq_along(norm_cols)) {
          for (pat in names(col_map)) {
            if (grepl(pat, norm_cols[i], fixed = TRUE)) {
              mapped_cols[i] <- col_map[[pat]]
              break
            }
          }
        }
        
        # Aplicar mapeo si encontramos coincidencias para todas las columnas esperadas
        if (all(expected_cols %in% mapped_cols)) {
          colnames(data) <- mapped_cols
          message("Columnas mapeadas exitosamente.")
        } else {
          missing_cols <- expected_cols[!expected_cols %in% mapped_cols]
          warning(paste("Faltan columnas necesarias:", paste(missing_cols, collapse = ", ")))
          return(NULL)
        }
      }
      
      # Convertir a tipos numéricos de manera segura
      for (col in c("Año", "Mes", "PESO DESEMBARCADO_KILOGRAMOS", "DIAS EFECTIVOS", "NUMERO EMBARCACIONES")) {
        if (col %in% colnames(data)) {
          data[[col]] <- as.numeric(as.character(data[[col]]))
        }
      }
      
      # Crear columna de fecha para gráficas
      data$Date <- as.Date(paste(data$Año, data$Mes, "15", sep = "-"), format = "%Y-%m-%d")
      
      # Ordenar cronológicamente
      data <- data[order(data$Date), ]
      
      return(data)
    })

    # Mostrar título
    output$catches_info <- renderUI({
      req(input$catches_species, input$catches_entity)
      div(
        style = "margin-bottom: 20px; background-color: #1a365d; padding: 15px; border-radius: 10px;",
        h3(
          paste(input$catches_species, "-", input$catches_entity), 
          style = "color: white; margin: 0; text-align: center;"
        )
      )
    })

    # Gráfica: peso
    output$catches_weight_plot <- renderPlotly({
      data <- selected_species_data()
      if (is.null(data)) return(NULL)
      
      plot_ly(data, x = ~Date, y = ~`PESO DESEMBARCADO_KILOGRAMOS`, type = 'scatter', mode = 'lines+markers',
              line = list(color = '#2874A6', width = 2),
              marker = list(color = '#2874A6', size = 5)) %>%
        layout(xaxis = list(title = "Fecha"),
              yaxis = list(title = "Peso (kg)"),
              margin = list(l = 50, r = 20, b = 50, t = 30),
              hovermode = "closest")
    })

    # Gráfica: días efectivos
    output$catches_days_plot <- renderPlotly({
      data <- selected_species_data()
      if (is.null(data)) return(NULL)
      
      plot_ly(data, x = ~Date, y = ~`DIAS EFECTIVOS`, type = 'scatter', mode = 'lines+markers',
              line = list(color = '#27AE60', width = 2),
              marker = list(color = '#27AE60', size = 5)) %>%
        layout(xaxis = list(title = "Fecha"),
              yaxis = list(title = "Días"),
              margin = list(l = 50, r = 20, b = 50, t = 30),
              hovermode = "closest")
    })

    # Gráfica: embarcaciones
    output$catches_vessels_plot <- renderPlotly({
      data <- selected_species_data()
      if (is.null(data)) return(NULL)
      
      plot_ly(data, x = ~Date, y = ~`NUMERO EMBARCACIONES`, type = 'scatter', mode = 'lines+markers',
              line = list(color = '#8E44AD', width = 2),
              marker = list(color = '#8E44AD', size = 5)) %>%
        layout(xaxis = list(title = "Fecha"),
              yaxis = list(title = "Embarcaciones"),
              margin = list(l = 50, r = 20, b = 50, t = 30),
              hovermode = "closest")
    })

    # Mostrar número de archivos procesados
    output$catches_files_count <- renderText({
      data <- catches_data()
      length(data)
    })
  # Download handlers ---------------------------------------------------------
  # 1. Reemplazamos el botón de "Descargar Reporte" con un enlace a Google Drive
  output$download_report <- renderUI({
    # Ajusta esta URL a la de tu archivo en Google Drive
    google_drive_url <- "https://docs.google.com/document/d/1645zm7fnmqksrKUbwsoJ-2CSI5NOlT11KJt2HIKH8wY/edit?usp=sharing"

    tags$a(
      href = google_drive_url,
      class = "btn btn-download",    # Clase CSS que usabas antes para estilizar
      target = "_blank",             # Abre en una nueva pestaña/ventana
      "Descargar Reporte"            # Texto que se mostrará en el botón
    )
  })

  # 2. Reemplazamos el botón de "Descargar Datos" con un enlace a Zenodo
  output$download_data <- renderUI({
    # Ajusta esta URL de Zenodo a tu DOI o archivo específico
    zenodo_url <- "https://drive.google.com/drive/folders/1IiJIfAkTDK-ro8wEGhbHSTmebjw5XVA_"

    tags$a(
      href = zenodo_url,
      class = "btn btn-download",
      target = "_blank",
      "Descargar Datos"
    )
  })
  
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
  output$adaptation_implementation_count <- renderText({
    cooperatives <- adaptation_implementation()
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