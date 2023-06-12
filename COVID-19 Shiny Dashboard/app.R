library(shiny)
library(dplyr)
library(dygraphs)
library(ggplot2)
library(Rcpp)
library(R6)

# Read the COVID-19 dataset
covid <- read.csv("WHO-COVID-19-global-data.csv")
covid$Date_reported <- as.Date(covid$Date_reported)

# Read the vaccination data
vaccination_data <- read.csv("vaccination-data.csv")
vaccination_data$DATE_UPDATED <- as.Date(vaccination_data$DATE_UPDATED)

# Define the CountryData class
CountryData <- R6Class("CountryData",
                       public = list(
                         country_data = NULL,
                         initialize = function(dataset, country_name) {
                           self$country_data <- subset(dataset, Country == country_name)
                         },
                         getData = function() {
                           return(self$country_data)
                         }
                       )
)

# Define the RegionData class
RegionData <- R6Class("RegionData",
                      public = list(
                        region_data = NULL,
                        
                        initialize = function(region_name) {
                          self$region_data <- subset(covid, WHO_region == region_name)
                        },
                        
                        getCases = function() {
                          return(self$region_data$Cumulative_cases)
                        },
                        
                        getDeaths = function() {
                          return(self$region_data$Cumulative_deaths)
                        }
                      )
)

# Define the VaccinationData class
VaccinationData <- R6Class("VaccinationData",
                           public = list(
                             vaccination_data = NULL,
                             
                             initialize = function() {
                               self$vaccination_data <- vaccination_data
                             },
                             
                             getVaccinatedCount = function(country_name) {
                               country_vaccination <- subset(self$vaccination_data, COUNTRY == country_name)
                               vaccinated_count <- country_vaccination$PERSONS_VACCINATED_1PLUS_DOSE
                               return(vaccinated_count)
                             }
                           )
)

# UI
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
      .login-container {
        position: absolute;
        top: 0%;
        left: 0%;
        transform: translate(30%, 40%);
      }
      ")
    )
  ),
  uiOutput("page"),
  tags$style(
    HTML("
    .footer {
      position: fixed;
      left: 0;
      bottom: 0;
      width: 100%;
      background-color: #f5f5f5;
      text-align: center;
      padding: 10px;
    }
    ")
  )
)


# Server
server <- function(input, output, session) {
  # Define valid usernames and passwords
  valid_credentials <- data.frame(
    username = c("ewa", "maria", "seto"),
    password = c("advancedr", "advancedr", "advancedr"),
    stringsAsFactors = FALSE
  )
  
  # Login
  login_valid <- reactiveVal(FALSE)
  
  observeEvent(input$login, {
    username <- input$username
    password <- input$password
    
    if (username %in% valid_credentials$username && password %in% valid_credentials$password) {
      login_valid(TRUE)
      showModal(
        modalDialog(
          title = "Login Successful",
          "You have successfully logged in.",
          easyClose = TRUE,
          footer = NULL
        )
      )
    } else {
      showModal(
        modalDialog(
          title = "Login Failed",
          "Invalid username or password.",
          easyClose = TRUE,
          footer = NULL
        )
      )
    }
  })
  
  # Dashboard
  output$page <- renderUI({
    if (isTRUE(login_valid())) {
      fluidPage(
        tags$head(
          tags$style(
            HTML("
            .login-container {
              position: absolute;
              top: 100%;
              left: 0%;
              transform: translate(-70%, 70%);
            }
            ")
          )
        ),
        navbarPage(
          "COVID-19 Dashboard",
          tabPanel(
            "Dataset Description",
            fluidRow(
              column(
                width = 12,
                tags$div(
                  class = "accordion",
                  tags$h3(class = "accordion-header", "About the COVID-19 Dataset"),
                  tags$div(
                    class = "accordion-content",
                    tags$p(
                      "The WHO coronavirus (COVID-19) dashboard presents official daily counts of COVID-19 cases, deaths, and vaccine utilization reported by countries, territories, and areas. It aims to provide a frequently updated data visualization, data dissemination, and data exploration resource while linking users to other useful and informative resources."
                    ),
                    tags$p(
                      "Caution must be taken when interpreting the data as differences may exist between information products published by WHO, national public health authorities, and other sources. These differences can arise due to variations in inclusion criteria, data cut-off times, case detection, definitions, laboratory testing, vaccination strategy, and reporting strategies. All data presented are subject to continuous verification and change."
                    ),
                    tags$p(
                      "The designations employed and the presentation of these materials do not imply the expression of any opinion whatsoever on the part of WHO concerning the legal status of any country, territory, or area or its authorities. Additionally, the dotted and dashed lines on maps represent approximate border lines for which there may not yet be full agreement."
                    ),
                    tags$ul(
                      tags$li(
                        tags$p(
                          "[1] All references to Kosovo should be understood to be in the context of the United Nations Security Council resolution 1244 (1999)."
                        )
                      ),
                      tags$li(
                        tags$p(
                          "[2] A dispute exists between the Governments of Argentina and the United Kingdom of Great Britain and Northern Ireland concerning sovereignty over the Falkland Islands (Malvinas)."
                        )
                      )
                    ),
                    tags$p(
                      "Data for Bonaire, Sint Eustatius, and Saba have been disaggregated and displayed at the subnational level."
                    ),
                    tags$p(
                      "Added idea: Provide information about data sources and update frequency."
                    )
                  )
                )
              )
            )
          ),
          tabPanel(
            "Covid Dataset Overview",
            mainPanel(
              dataTableOutput("datatable")
            )
          ),
          tabPanel(
            "Visualization",
            fluidRow(
              column(
                width = 12,
                selectInput(
                  inputId = "country",
                  label = "Select Country",
                  choices = unique(covid$Country),
                  selected = unique(covid$Country)[1]
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                dygraphOutput("line_chart")
              )
            )
          ),
          tabPanel(
            "Vaccination Overview",
            fluidRow(
              column(
                width = 12,
                dataTableOutput("vaccination_table")
              )
            ),
            fluidRow(
              column(
                width = 6,
                selectInput(
                  inputId = "vaccination_country",
                  label = "Select Country",
                  choices = unique(vaccination_data$COUNTRY),
                  selected = unique(vaccination_data$COUNTRY)[1]
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                verbatimTextOutput("vaccinated_count_output")
              )
            ),
            fluidRow(
              column(
                width = 12,
                plotOutput("vaccination_progress")
              )
            )
          ),
          tabPanel(
            "Predictive Modelling",
            fluidRow(
              column(
                width = 12,
                dygraphOutput("predictive_chart")
              )
            )
          )
        ),
        tags$div(
          class = "footer",
          p("A project by Nurseto Dwi Nugroho"),
          p("Source code available on ", tags$a("Github", href = "https://github.com/nursetodwinugroho", target="_blank")),
          p("Copyright © 2023 Faculty of Economics Sciences University of Warsaw")
        )
      )
    } else {
      fluidRow(
        column(
          width = 4,
          offset = 4,
          div(
            class = "login-container",
            h3("COVID-19 WHO Dashboard Login"),
            wellPanel(
              textInput("username", "Username"),
              passwordInput("password", "Password"),
              br(),
              actionButton("login", "Log In")
            )
          )
        )
      )
    }
  })
  
  # Dataset overview - DataTable
  # Dataset overview - DataTable
  output$datatable <- renderDataTable({
    covid
  })
  
  
  # Line chart - Cumulative Cases for Chosen Country
  output$line_chart <- renderDygraph({
    country_name <- input$country
    country_data <- CountryData$new(covid, country_name)
    
    dygraph(country_data$getData(), x = "Date_reported") %>%
      dySeries("Cumulative_cases", label = "Cumulative Cases") %>%
      dyAxis("y", label = "Number of Cases") %>%
      dyOptions(drawGrid = FALSE) %>%
      dyHighlight(
        highlightCircleSize = 4,
        highlightSeriesBackgroundAlpha = 0.2,
        highlightSeriesOpts = list(strokeWidth = 3)
      )
  })
  
  # Vaccination overview - DataTable
  output$vaccination_table <- renderDataTable({
    vaccination_data
  })
  
  # Vaccination analysis - Get vaccinated count for chosen country
  output$vaccinated_count_output <- renderPrint({
    country_name <- input$vaccination_country
    vaccinated_count <- subset(vaccination_data, COUNTRY == country_name)$PERSONS_VACCINATED_1PLUS_DOSE
    output1 <- paste("Number of People Vaccinated in", country_name, ":", vaccinated_count)
    output1
  })
  
  
  # Vaccination analysis - Get total vaccination for chosen country
  output$total_vaccines_output <- renderPrint({
    country_name <- input$vaccination_country
    total_vaccination <- sum(subset(vaccination_data, COUNTRY == country_name)$PERSONS_VACCINATED_1PLUS_DOSE, na.rm = TRUE)
    output2 <- paste("Total Vaccination in", country_name, ":", total_vaccination)
    output2
  })
  
  
  
  # Predictive Modelling - Line Chart
  output$predictive_chart <- renderDygraph({
    # Generate the predicted data for the chart
    predicted_data <- data.frame(
      Date_reported = seq(Sys.Date(), by = "month", length.out = 12),
      Predicted_cases = runif(12, 500, 1000)
    )
    
    dygraph(predicted_data, x = "Date_reported") %>%
      dySeries("Predicted_cases", label = "Predicted Cases") %>%
      dyAxis("y", label = "Number of Cases") %>%
      dyOptions(drawGrid = FALSE) %>%
      dyHighlight(
        highlightCircleSize = 4,
        highlightSeriesBackgroundAlpha = 0.2,
        highlightSeriesOpts = list(strokeWidth = 3)
      )
  })
}

shinyApp(ui, server)
