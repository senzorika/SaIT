# SESONED Sensory summer school, SDU Odense, Denmark
# Vladimir Vietoris, 2024
# Load necessary libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(openxlsx)
library(dplyr)

# Define the User Interface for the application
ui <- dashboardPage(
  dashboardHeader(title = "TDS Measurement for Beer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Collection", tabName = "dataCollection", icon = icon("edit")),
      menuItem("Data Visualization", tabName = "dataVisualization", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      # Tab for Data Collection
      tabItem(tabName = "dataCollection",
              fluidRow(
                box(title = "Instructions", width = 12,
                    p("Select a sample, mark the dominant sensations you perceive over time, and click 'Save Data' to record your observations.")
                )
              ),
              fluidRow(
                box(title = "Sample Selection", width = 12,
                    selectInput("sample", "Select a sample:", choices = LETTERS[1:6])
                )
              ),
              fluidRow(
                box(title = "Dominant Sensations", width = 12,
                    radioButtons("dominant", "Select the dominant sensation:",
                                 choices = list("Bitter" = "bitter", 
                                                "Sweet" = "sweet",
                                                "Sour" = "sour",
                                                "Fruity" = "fruity",
                                                "Hoppy" = "hoppy",
                                                "Malty" = "malty",
                                                "Caramel" = "caramel",
                                                "Citrus" = "citrus",
                                                "Spicy" = "spicy",
                                                "Nutty" = "nutty"))
                )
              ),
              fluidRow(
                box(title = "Timing", width = 12,
                    sliderInput("time", "Time (seconds):", 
                                min = 0, max = 20, value = 0, step = 1, animate = animationOptions(interval = 1000, loop = FALSE))
                )
              ),
              fluidRow(
                box(title = "Actions", width = 12,
                    actionButton("save", "Save Data")
                )
              )
      ),
      
      # Tab for Data Visualization
      tabItem(tabName = "dataVisualization",
              fluidRow(
                box(title = "Selected Dominant Sensations Over Time", width = 12,
                    plotOutput("timePlot")
                )
              ),
              fluidRow(
                box(title = "Dominance Rate Over Time", width = 12,
                    plotOutput("dominanceRatePlot")
                )
              ),
              fluidRow(
                box(title = "Download Data", width = 12,
                    downloadButton("downloadData", "Download Data as XLSX")
                )
              )
      )
    )
  )
)

# Define server logic for the application
server <- function(input, output, session) {
  # Reactive values to store selected dominant sensations over time
  values <- reactiveValues(data = data.frame(Sample = character(), Time = integer(), Dominant = character(), stringsAsFactors = FALSE))
  
  observeEvent(input$time, {
    if(input$time > 0 && !is.null(input$dominant)) {
      new_data <- data.frame(Sample = input$sample, 
                             Time = input$time, 
                             Dominant = input$dominant, 
                             stringsAsFactors = FALSE)
      values$data <- rbind(values$data, new_data)
    }
  }, ignoreNULL = FALSE)  # Important to handle time = 0 correctly
  
  observeEvent(input$save, {
    values$TDS_BEER <- values$data
    showModal(modalDialog(
      title = "Data Saved",
      "Your TDS data has been saved.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  output$timePlot <- renderPlot({
    req(nrow(values$data) > 0)  # Ensure there's data before plotting
    ggplot(values$data, aes(x = Time, fill = Dominant)) +
      geom_histogram(binwidth = 1, position = "stack") +
      labs(title = "Selected Dominant Sensations Over Time", x = "Time (seconds)", y = "Count") +
      theme_minimal()
  })
  
  output$dominanceRatePlot <- renderPlot({
    req(nrow(values$data) > 0)  # Ensure there's data before plotting
    dominance_data <- values$data %>%
      group_by(Time, Dominant) %>%
      summarise(Count = n(), .groups = 'drop')  # Use .groups = 'drop' to avoid grouping warnings
    ggplot(dominance_data, aes(x = Time, y = (Count / sum(Count)), color = Dominant)) +
      geom_line(size = 1) +
      labs(title = "Dominance Rate Over Time", x = "Time (seconds)", y = "Dominance Rate") +
      theme_minimal()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("TDS_BEER_Data_", Sys.Date(), ".xlsx", sep = "") },
    content = function(file) {
      write.xlsx(values$TDS_BEER, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)