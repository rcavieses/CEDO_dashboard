# ui.R
# UI definition for Fisheries and Climate Vulnerability Assessment Dashboard

# Load required libraries
library(shiny)
library(leaflet)
library(DT)
library(plotly)
library(shinyjs)
library(htmltools)
library(scales)
library(shinycssloaders)

# UI Definition
ui <- fluidPage(
  useShinyjs(),
  
  # Custom CSS and fonts
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&display=swap", rel = "stylesheet"),
    # Agregar Font Awesome para los iconos
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css")
  ),
  
  # Barra de contacto
  div(class = "contact-top-bar",
      div(class = "contact-container",
          # Lado izquierdo - Información de contacto
          div(class = "contact-info",
              a(href = "hemnalini@cedo.org", 
                icon("envelope"), 
                class = "contact-item"),
              a(href = "tel:+1-52-638-382-0113", 
                icon("phone"), " +1 52 638-382-0113 y 0115", 
                class = "contact-item")
          ),
          # Lado derecho - Redes sociales
          div(class = "social-icons",
              a(href = "https://www.facebook.com/www.cedo.org", target = "_blank", 
                icon("facebook-f"), class = "social-icon"),
              a(href = "https://twitter.com/cedo_mex", target = "_blank", 
                icon("twitter"), class = "social-icon"),
              a(href = "https://www.instagram.com/cedointercultural/", target = "_blank", 
                icon("instagram"), class = "social-icon"),
              a(href = "https://www.youtube.com/channel/UCzefr0RNoKZa6fnwgG9QOlQ/videos", target = "_blank", 
                icon("youtube"), class = "social-icon"),
              a(href = "https://www.linkedin.com/company/cedo-centro-intercultural-del-desierto-y-del-oc-ano/", target = "_blank", 
                icon("linkedin-in"), class = "social-icon")
          )
      )
  ),
  
  # Header with title
  div(class = "dashboard-header",
      titlePanel("Evaluaciones de las pesquerías, riesgo socioeconómico, biológico y recomendaciones de medidas de adaptación para comunidades costeras selectas en México")
  ),
  
  # Horizontal navigation bar
  navbarPage(
    title = NULL,
    id = "mainTabs",
    tabPanel("Resumen ejecutivo", value = "summary"),
    tabPanel("Variables ambientales", value = "environment"),
    tabPanel("Mapas de cambios en distribución", value = "distribution"),
    tabPanel("Crecimiento y tamaño de la población", value = "population"),
    tabPanel("Mapa Vulnerabilidad regional", value = "vulnerability"),
    tabPanel("Determinantes de captura", value = "capture"),
    #tabPanel("Riesgo por especies", value = "risk"),
    tabPanel("Mapas de áreas potenciales de pesca", value = "fishing"),
    tabPanel("Acciones de adaptación para SCPP", value = "workshops"),
    tabPanel("Acciones generales de adaptación", value = "adaptation")
    
  ),
  
  # Main content area with sidebar layout
  sidebarLayout(
    # Sidebar panel with filters
    sidebarPanel(
      width = 3,
      
            
      # Label for filters section
      h4("Filtros:", class = "filter-heading"),
      
      # Environmental Variables Filters
      conditionalPanel(
        condition = "input.mainTabs === 'environment'",
        div(class = "filter-section",
          selectInput("env_entity", "Entidad:", choices = NULL),
          h5("Variables:"),
          checkboxGroupInput("env_variables", NULL, 
                           choices = c("Temperatura (C)", "Clorofila (mg/m3)", "PDO", "MEI"),
                           selected = "Temperatura (C)")
        )
      ),
      
      # Species Risk Filters
      #conditionalPanel(
      #  condition = "input.mainTabs === 'risk'",
      #  div(class = "filter-section",
      #    selectInput("risk_species", "Especie:", choices = NULL)
      #  )
      #),
      
      # Fishing Areas Filters
      conditionalPanel(
        condition = "input.mainTabs === 'fishing'",
        div(class = "filter-section",
          selectInput("fishing_locality", "Cooperativa:", choices = NULL)
        )
      ),
      
      
      # Adaptation Actions Filters ----------------------------------------------------------
      conditionalPanel(
        condition = "input.mainTabs === 'adaptation'",
        div(class = "filter-section",
          # 1. Filtro de plazo de implementación
          selectInput("adaptation_timeframe", "Plazo de implementación:", choices = NULL),
          
          # 2. Radio buttons para tipo de estrategia - se cargarán dinámicamente
          h4("Tipo de estrategia:", style = "margin-top: 20px;"),
          
          # Contenedor con estilo para los radio buttons
          div(
            style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border-left: 4px solid #1a365d;",
            # Solo definimos el ID del input, las opciones se cargarán dinámicamente
            radioButtons(
              "adaptation_type", 
              label = NULL, 
              choices = c("Todas" = "all"),  # Valor inicial, se actualizará
              selected = "all"
            )
          ),
          
          # 3. Opción para mostrar todos los registros
          checkboxInput(
            "show_all_records", 
            "Mostrar todos los registros", 
            value = TRUE
          )
        )
      ),
      
      # Population Filters ----------------------------------------------------------
      conditionalPanel(
        condition = "input.mainTabs === 'population'",
        div(class = "filter-section",
          selectInput("population_species", "Especie:", choices = NULL)
        )
      ),
      
      # Capture Determinants Filters ----------------------------------------------------------
      conditionalPanel(
        condition = "input.mainTabs === 'capture'",
        div(class = "filter-section",
          selectInput("capture_species", "Seleccionar Especie:", choices = NULL)
        )
      ),
      
      # Vulnerability Filters ----------------------------------------------------------
      conditionalPanel(
        condition = "input.mainTabs === 'vulnerability'",
        div(class = "filter-section",
          selectInput("vuln_entity", "Entidad:", choices = NULL),
          selectInput("vuln_municipality", "Municipio:", choices = NULL),
          selectInput("vuln_scenario", "Escenario Climático:",
                     choices = c("Histórico" = "historical", 
                                "SSP126" = "ssp126", 
                                "SSP585" = "ssp585"),
                     selected = "ssp585"),
          selectInput("vuln_variable", "Variable:",
                     choices = c("Vulnerabilidad" = "Vulnerabilidad",
                                "Adaptabilidad" = "Adaptabilidad",
                                "Exposición" = "Exposicion",
                                "Sensibilidad" = "Sensibilidad"),
                     selected = "Vulnerabilidad")
        )
      ),
      
      # Distribution Filters
      conditionalPanel(
        condition = "input.mainTabs === 'distribution'",
        div(class = "filter-section",
          # Selector unificado de especie
          selectInput("dist_species", "Especie:", choices = NULL)
        )
      ),
            
      # Workshops Filters
      conditionalPanel(
        condition = "input.mainTabs === 'workshops'",
        div(class = "filter-section",
          selectInput("workshops_cooperative", "Cooperativa:", choices = NULL)
        )
      ),
      
      # Download and methodology buttons at bottom of sidebar
      div(class = "sidebar-buttons",
         uiOutput("download_report"),
          uiOutput("download_data"),
          actionButton("show_methodology", "Metodología", 
                     class = "btn-info",
                     icon = icon("info-circle"))
      )
    ),
    
    # Main panel with content
    mainPanel(
      width = 9,
      
      # Executive Summary ----------------------------------------------------------
      conditionalPanel(
        condition = "input.mainTabs === 'summary'",
        div(class = "tab-content",
            h3("Resumen Ejecutivo", class = "section-title"),
            uiOutput("executive_summary")
        )
      ),
      
      # Environmental Variables ----------------------------------------------------------
      conditionalPanel(
        condition = "input.mainTabs === 'environment'",
        div(class = "tab-content",
            h3("Variables Ambientales", class = "section-title"),
            withSpinner(plotlyOutput("env_time_series", height = "500px")),
            div(class = "help-text", "Seleccione una entidad y variable para visualizar los datos históricos.")
        )
      ),
      
      # Species Risk ----------------------------------------------------------
      #conditionalPanel(
      #  condition = "input.mainTabs === 'risk'",
      #  div(class = "tab-content",
      #      h3("Evaluación de Riesgo por Especies", class = "section-title"),
      #      withSpinner(uiOutput("risk_image")),
      #      div(class = "help-text", 
      #          "Total de especies analizadas: ", 
      #          textOutput("risk_species_count", inline = TRUE))
      #  )
      #),
      
      # Fishing Areas ----------------------------------------------------------
      conditionalPanel(
        condition = "input.mainTabs === 'fishing'",
        div(class = "tab-content",
            h3("Mapas de Áreas Potenciales de Pesca", class = "section-title"),
            withSpinner(uiOutput("fishing_map")),
            div(class = "help-text", 
                "Total de cooperativas: ", 
                textOutput("fishing_localities_count", inline = TRUE))
        )
      ),
      
      # Adaptation Actions ----------------------------------------------------------
     conditionalPanel(
      condition = "input.mainTabs === 'adaptation'",
      div(class = "tab-content",
          h3("Acciones de Adaptación", class = "section-title"),
          withSpinner(DTOutput("adaptation_table")),
          uiOutput("adaptation_help_text")
      )
    ),
      
      # Population Growth and Size ----------------------------------------------------------
      conditionalPanel(
        condition = "input.mainTabs === 'population'",
        div(class = "tab-content",
            h3("Crecimiento y Tamaño de la Población", class = "section-title"),
            uiOutput("combined_cards")
        )
      ),

      # Capture Determinants ----------------------------------------------------------
      conditionalPanel(
        condition = "input.mainTabs === 'capture'",
        div(class = "tab-content",
            h3("Determinantes de la Captura", class = "section-title"),
            
            # Texto explicativo
            uiOutput("capture_explanation"),
            
            # Tabla de datos
            withSpinner(DTOutput("capture_table"))
        )
      ),
      
      # Regional Vulnerability ----------------------------------------------------------
      conditionalPanel(
        condition = "input.mainTabs === 'vulnerability'",
        div(class = "tab-content",
            h3("Vulnerabilidad Regional", class = "section-title"),
            
            fluidRow(
              column(
                width = 8,
                withSpinner(leafletOutput("vulnerability_map", height = "500px"))
              ),
              column(
                width = 4,
                div(class = "locality-counter",
                    style = "background-color: #1a365d; color: white; 
                    padding: 15px; border-radius: 8px;
                    box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                    h4("Localidades analizadas:", style = "margin: 0;"),
                    textOutput("vulnerability_localities_count", inline = TRUE)
                ),
                
                h4("Acerca de esta visualización:", style = "margin-top: 20px;"),
                p("Este mapa interactivo muestra la distribución espacial de los indicadores de vulnerabilidad en las comunidades costeras. El tamaño de cada círculo representa el tamaño de la población, mientras que el color indica la variable de vulnerabilidad seleccionada."),
                p("Haga clic en los círculos para ver información detallada de cada localidad.")
              )
            ),
            
            withSpinner(plotlyOutput("vulnerability_scatter", height = "800px"))
        )
      ),
      
      # Distribution Changes
      conditionalPanel(
        condition = "input.mainTabs === 'distribution'",
        div(class = "tab-content",
            h3("Mapas de Cambios en Distribución", class = "section-title"),
            
            # Ambos mapas en columnas (uiOutput("distribution_maps") se genera en server)
            withSpinner(uiOutput("distribution_maps")),
            
            # Tabla con datos de tabla7 filtrada por la especie
            h4("Porcentaje de cambio en distribución:", style = "margin-top: 20px;"),
            withSpinner(uiOutput("tabla7_boxes"))
        )
      ),
      
      # Workshops
      conditionalPanel(
        condition = "input.mainTabs === 'workshops'",
        div(class = "tab-content",
            h3("Talleres", class = "section-title"),
            
            # Panel con el nombre de la cooperativa y los bloques
            withSpinner(uiOutput("workshops_panel")),
            
            # Muestra el número de filas filtradas
            div(class = "help-text", 
                "Total de registros filtrados: ", 
                textOutput("workshops_count", inline = TRUE))
        )
      )
    )
  ),
  
  # Footer
  tags$footer(
    div(class = "footer-container",
        div(class = "footer-content",
            div(class = "logo-row",
                tags$img(src = "logo1.png", class = "partner-logo"),
                tags$img(src = "logo2.png", class = "partner-logo"),
                tags$img(src = "logo3.png", class = "partner-logo"),
                tags$img(src = "logo4.png", class = "partner-logo")
            ),
            div(class = "copyright",
                p("© 2025 CEDO intercultural. All Rights Reserved.")
            )
        )
    )
  )
)