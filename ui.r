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

# UI Definition
ui <- fluidPage(
  useShinyjs(),
  
  # Custom CSS and fonts
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&display=swap", rel = "stylesheet")
  ),
  
  # Header with title
  div(class = "dashboard-header",
      titlePanel("Evaluación de Pesquerías y Vulnerabilidad Climática")
  ),
  
  # Main layout
  sidebarLayout(
    # Sidebar panel
    sidebarPanel(
      width = 3,
      div(class = "logo-container-sidebar",
          tags$img(src = "logo1.png", width = "65%", class = "logo")
      ),
      
      # Section navigation
      selectInput("mainTabs", "Sección:",
                  choices = c(
                    "Resumen Ejecutivo" = "summary",
                    "Variables Ambientales" = "environment",
                    "Riesgo por Especies" = "risk",
                    "Áreas Potenciales de Pesca" = "fishing",
                    "Acciones de Adaptación" = "adaptation",
                    "Crecimiento y Tamaño de Población" = "population",
                    "Determinantes de Captura" = "capture",
                    "Vulnerabilidad Regional" = "vulnerability",
                    "Cambios en Distribución" = "distribution",
                    "Talleres" = "workshops"
                  ),
                  selected = "summary"
      ),
      
      # Dynamic filters based on selected section
      conditionalPanel(
        condition = "input.mainTabs === 'environment'",
        selectInput("env_entity", "Seleccionar Entidad:", choices = NULL),
        selectInput("env_variable", "Seleccionar Variable:", choices = NULL)
      ),
      
      conditionalPanel(
        condition = "input.mainTabs === 'risk'",
        selectInput("risk_species", "Seleccionar Especie:", choices = NULL)
      ),
      
      conditionalPanel(
        condition = "input.mainTabs === 'fishing'",
        selectInput("fishing_locality", "Seleccionar Localidad:", choices = NULL)
      ),
      
      conditionalPanel(
        condition = "input.mainTabs === 'adaptation'",
        selectInput("adaptation_cooperative", "Seleccionar Cooperativa:", choices = NULL)
      ),
      
      conditionalPanel(
        condition = "input.mainTabs === 'population'",
        selectInput("population_species", "Seleccionar Especie:", choices = NULL)
      ),
      
      conditionalPanel(
        condition = "input.mainTabs === 'capture'",
        selectInput("capture_species", "Seleccionar Especie:", choices = NULL)
      ),
      
      conditionalPanel(
        condition = "input.mainTabs === 'vulnerability'",
        selectInput("vuln_entity", "Seleccionar Entidad:", choices = NULL),
        selectInput("vuln_municipality", "Seleccionar Municipio:", choices = NULL),
        selectInput("vuln_scenario", "Seleccionar Escenario Climático:",
                    choices = c("Histórico" = "historical", 
                                "SSP126" = "ssp126", 
                                "SSP585" = "ssp585"),
                    selected = "ssp585"),
        selectInput("vuln_variable", "Seleccionar Variable:",
                    choices = c("Vulnerabilidad" = "Vulnerabilidad",
                                "Adaptabilidad" = "Adaptabilidad",
                                "Exposición" = "Exposicion",
                                "Sensibilidad" = "Sensibilidad"),
                    selected = "Vulnerabilidad")
      ),
      
      conditionalPanel(
        condition = "input.mainTabs === 'distribution'",
        selectInput("dist_tabs", "Tipo de Visualización:",
                    choices = c("Modelos de Distribución" = "models", 
                                "Datos de Ocurrencia" = "occurrence"),
                    selected = "models"),
        conditionalPanel(
          condition = "input.dist_tabs === 'models'",
          selectInput("dist_html_species", "Seleccionar Especie:", choices = NULL)
        ),
        conditionalPanel(
          condition = "input.dist_tabs === 'occurrence'",
          selectInput("dist_csv_species", "Seleccionar Especie:", choices = NULL)
        )
      ),
      
      conditionalPanel(
        condition = "input.mainTabs === 'workshops'",
        selectInput("workshops_cooperative", "Seleccionar Cooperativa:", choices = NULL)
      ),
      
      tags$hr(),
      
      # Download buttons
      div(class = "download-buttons",
          style = "display: flex; flex-direction: column; gap: 10px;",
          downloadButton(
            outputId = "download_report",
            label = "Descargar Reporte",
            class = "btn-download"
          ),
          downloadButton(
            outputId = "download_data",
            label = "Descargar Datos",
            class = "btn-download"
          )
      ),
      
      div(class = "report-instructions",
          style = "margin-top: 15px; font-size: 0.9em; color: #4a5568;",
          actionButton("show_methodology", "Metodología", 
                      class = "btn-block",
                      icon = icon("info-circle"))
      )
    ),
    
    # Main panel with content
    mainPanel(
      width = 9,
      
      # Executive Summary
      conditionalPanel(
        condition = "input.mainTabs === 'summary'",
        div(class = "tab-content",
            h3("Resumen Ejecutivo", class = "section-title"),
            uiOutput("executive_summary")
        )
      ),
      
      # Environmental Variables
      conditionalPanel(
        condition = "input.mainTabs === 'environment'",
        div(class = "tab-content",
            h3("Variables Ambientales", class = "section-title"),
            withSpinner(plotlyOutput("env_time_series", height = "500px")),
            div(class = "help-text", "Seleccione una entidad y variable para visualizar los datos históricos.")
        )
      ),
      
      # Species Risk
      conditionalPanel(
        condition = "input.mainTabs === 'risk'",
        div(class = "tab-content",
            h3("Evaluación de Riesgo por Especies", class = "section-title"),
            withSpinner(uiOutput("risk_image")),
            div(class = "help-text", 
                "Total de especies analizadas: ", 
                textOutput("risk_species_count", inline = TRUE))
        )
      ),
      
      # Fishing Areas
      conditionalPanel(
        condition = "input.mainTabs === 'fishing'",
        div(class = "tab-content",
            h3("Mapas de Áreas Potenciales de Pesca", class = "section-title"),
            withSpinner(uiOutput("fishing_map")),
            div(class = "help-text", 
                "Total de localidades: ", 
                textOutput("fishing_localities_count", inline = TRUE))
        )
      ),
      
      # Adaptation Actions
      conditionalPanel(
        condition = "input.mainTabs === 'adaptation'",
        div(class = "tab-content",
            h3("Acciones de Adaptación", class = "section-title"),
            withSpinner(DTOutput("adaptation_table")),
            div(class = "help-text", 
                "Total de cooperativas: ", 
                textOutput("adaptation_cooperatives_count", inline = TRUE))
        )
      ),
      
      # Population Growth and Size
      conditionalPanel(
        condition = "input.mainTabs === 'population'",
        div(class = "tab-content",
            h3("Crecimiento y Tamaño de la Población", class = "section-title"),
            withSpinner(DTOutput("population_table"))
        )
      ),
      
      # Capture Determinants
      conditionalPanel(
        condition = "input.mainTabs === 'capture'",
        div(class = "tab-content",
            h3("Determinantes de la Captura", class = "section-title"),
            withSpinner(DTOutput("capture_table"))
        )
      ),
      
      # Regional Vulnerability
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
            
            withSpinner(plotlyOutput("vulnerability_scatter", height = "400px"))
        )
      ),
      
      # Distribution Changes
      conditionalPanel(
        condition = "input.mainTabs === 'distribution'",
        div(class = "tab-content",
            h3("Cambios en Distribución", class = "section-title"),
            
            conditionalPanel(
              condition = "input.dist_tabs === 'models'",
              withSpinner(uiOutput("dist_html_map"))
            ),
            
            conditionalPanel(
              condition = "input.dist_tabs === 'occurrence'",
              withSpinner(leafletOutput("dist_csv_map", height = "500px"))
            )
        )
      ),
      
      # Workshops
      conditionalPanel(
        condition = "input.mainTabs === 'workshops'",
        div(class = "tab-content",
            h3("Talleres", class = "section-title"),
            withSpinner(DTOutput("workshops_table")),
            div(class = "help-text", 
                "Total de talleres realizados: ", 
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
                tags$img(src = "logo2.png", class = "partner-logo")
            ),
            div(class = "copyright",
                p("© 2024 WWF Dashboard. All Rights Reserved.")
            )
        )
    )
  )
)