# SESONED Sensory summer school, SDU Odense, Denmark
# Vladimir Vietoris, 2024
library(shiny)
library(shinydashboard)
library(ggplot2)
library(openxlsx)
library(dplyr)
library(tidyr)
library(ca)

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "TCATA Measurement for Beer"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tabsetPanel(
      tabPanel("Data Collection",
               fluidRow(
                 box(title = "Instructions", width = 12,
                     p("Please select the sample, check the attributes you perceive over time, and click 'Save Data' to record your observations.")
                 )
               ),
               fluidRow(
                 box(title = "Sample Selection", width = 12,
                     selectInput("sample", "Select Sample:", choices = LETTERS[1:6])
                 )
               ),
               fluidRow(
                 box(title = "Attributes", width = 12,
                     checkboxGroupInput("attributes", "Select attributes:",
                                        choices = list("Bitter" = "bitter", 
                                                       "Sweet" = "sweet",
                                                       "Sour" = "sour",
                                                       "Fruity" = "fruity",
                                                       "Hoppy" = "hoppy",
                                                       "Malty" = "malty"))
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
                     actionButton("save", "Save Data"),
                     downloadButton("downloadData", "Download Data as XLSX")
                 )
               )
      ),
      tabPanel("Visualization",
               fluidRow(
                 box(title = "Sample Filter", width = 12,
                     selectInput("filterSample", "Filter by Sample:", choices = LETTERS[1:6], selected = LETTERS[1:6], multiple = TRUE)
                 )
               ),
               fluidRow(
                 box(title = "Selected Attributes Over Time", width = 12,
                     plotOutput("timePlot"),
                     verbatimTextOutput("selectedAttributes")
                 )
               ),
               fluidRow(
                 box(title = "Correspondence Analysis", width = 12,
                     plotOutput("caPlot")
                 )
               )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values to store selected attributes over time
  values <- reactiveValues(data = data.frame(Sample = character(), Time = integer(), Attribute = character(), stringsAsFactors = FALSE),
                           TCATA_BEER = NULL)
  
  # Observe slider input and update reactive values
  observeEvent(input$time, {
    if(input$time > 0) {
      new_data <- data.frame(Sample = rep(input$sample, length(input$attributes)), 
                             Time = rep(input$time, length(input$attributes)), 
                             Attribute = input$attributes, 
                             stringsAsFactors = FALSE)
      values$data <- rbind(values$data, new_data)
    }
  })
  
  # Save data to TCATA_BEER when button is clicked
  observeEvent(input$save, {
    values$TCATA_BEER <- values$data
    showModal(modalDialog(
      title = "Data Saved",
      "Your TCATA data has been saved.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Display selected attributes over time
  output$selectedAttributes <- renderPrint({
    values$data
  })
  
  # Visualize selected attributes over time
  output$timePlot <- renderPlot({
    filtered_data <- values$data %>% filter(Sample %in% input$filterSample)
    if (nrow(filtered_data) > 0) {
      ggplot(filtered_data, aes(x = Time, fill = Attribute)) +
        geom_histogram(binwidth = 5, position = "stack") +
        labs(title = "Selected Attributes Over Time", x = "Time (seconds)", y = "Count") +
        theme_minimal()
    }
  })
  
  # Perform Correspondence Analysis and plot
  output$caPlot <- renderPlot({
    if (nrow(values$data) > 0) {
      # Prepare data for CA
      ca_data <- values$data %>%
        group_by(Sample, Attribute) %>%
        summarize(Count = n()) %>%
        spread(Attribute, Count, fill = 0)
      
      ca_result <- ca(ca_data[,-1]) # Perform CA excluding the 'Sample' column
      
      # Plot CA
      ca_df <- as.data.frame(ca_result$rowcoord)
      ca_df$Sample <- ca_data$Sample
      ggplot(ca_df, aes(x = Dim1, y = Dim2, label = Sample)) +
        geom_point() +
        geom_text(vjust = 1.5, hjust = 1.5) +
        labs(title = "Correspondence Analysis of Samples", x = "Dimension 1", y = "Dimension 2") +
        theme_minimal()
    }
  })
  
  # Download data as XLSX
  output$downloadData <- downloadHandler(
    filename = function() { paste("TCATA_Data_", Sys.Date(), ".xlsx", sep = "") },
    content = function(file) {
      write.xlsx(values$TCATA_BEER, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)