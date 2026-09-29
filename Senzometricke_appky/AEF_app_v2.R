# SESONED Sensory summer school, SDU Odense, Denmark
# Vladimir Vietoris, 2024
library(shiny)
library(shinyjs)
library(openxlsx)
library(ggplot2)
library(tidyr)

# Define UI for application
ui <- fluidPage(
  useShinyjs(),
  titlePanel("AEF-FC (Attack-Evolution-Finish Free Comments) sensory consumer method"),
  
  tabsetPanel(
    tabPanel("Data Acquisition",
             sidebarLayout(
               sidebarPanel(
                 actionButton("start_button", "Start Timer"),
                 numericInput("aef_time", "Current Time (seconds):", value = 0, min = 0, step = 1),
                 textInput("start_question", "Describe the Start sensation:"),
                 textInput("evolution_question", "Describe the Evolution sensation:"),
                 textInput("finish_question", "Describe the Finish sensation:"),
                 actionButton("save_aef", "Save AEF Data"),
                 actionButton("save_excel", "Save to Excel"),
                 hr(),
                 h3("Instructions:"),
                 p("1. Click 'Start Timer' to begin recording time."),
                 p("2. Fill in the descriptions for Start, Evolution, and Finish sensations."),
                 p("3. Click 'Save AEF Data' to record the data and stop the timer."),
                 p("4. Click 'Save to Excel' to save all data to an Excel file.")
               ),
               mainPanel()
             )
    ),
    tabPanel("Data Visualization",
             mainPanel(
               plotOutput("aefPlot"),
               tableOutput("aefData")
             )
    )
  ),
  tags$script(HTML("
    $(document).ready(function() {
      $('#aef_time').prop('readonly', true);
    });
  "))
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store data
  values <- reactiveValues(
    aef_data = data.frame(Time = numeric(0), Start = character(0), Evolution = character(0), Finish = character(0)),
    aef_time = 0,
    aef_running = FALSE,
    aef_interval_id = NULL
  )
  
  # Helper function to update inputs
  updateInputs <- function() {
    updateNumericInput(session, "aef_time", value = 0)
    updateTextInput(session, "start_question", value = "")
    updateTextInput(session, "evolution_question", value = "")
    updateTextInput(session, "finish_question", value = "")
  }
  
  # Start button for AEF
  observeEvent(input$start_button, {
    values$aef_running <- TRUE
    values$aef_time <- 0
    updateInputs()
    if (!is.null(values$aef_interval_id)) {
      runjs("clearInterval(aefInterval);")
    }
    values$aef_interval_id <- runjs("var aefInterval = setInterval(function() { Shiny.setInputValue('aef_time', Math.round($('#aef_time').val()) + 1); }, 1000);")
  })
  
  # Save AEF data
  observeEvent(input$save_aef, {
    if (values$aef_running) {
      if (input$start_question == "" || input$evolution_question == "" || input$finish_question == "") {
        showNotification("Please fill in all the sensation descriptions", type = "error")
      } else {
        new_aef_data <- data.frame(Time = input$aef_time, Start = input$start_question, Evolution = input$evolution_question, Finish = input$finish_question)
        values$aef_data <- rbind(values$aef_data, new_aef_data)
        showNotification("AEF data saved", type = "message")
        # Stop and reset timer
        runjs("clearInterval(aefInterval);")
        values$aef_running <- FALSE
        values$aef_time <- 0
        updateInputs()
      }
    } else {
      showNotification("Please start the timer before saving data", type = "error")
    }
  })
  
  # Update time for AEF
  observe({
    if (values$aef_running) {
      invalidateLater(1000, session)
      isolate({
        values$aef_time <- values$aef_time + 1
        updateNumericInput(session, "aef_time", value = values$aef_time)
      })
    }
  })
  
  # Save to Excel
  observeEvent(input$save_excel, {
    if (nrow(values$aef_data) > 0) {
      wb <- createWorkbook()
      addWorksheet(wb, "AEF Data")
      writeData(wb, "AEF Data", values$aef_data)
      
      # Create a second sheet with visualization data
      addWorksheet(wb, "Visualization")
      aef_long <- pivot_longer(values$aef_data, cols = c("Start", "Evolution", "Finish"), names_to = "Phase", values_to = "Sensation")
      writeData(wb, "Visualization", aef_long)
      
      # Save workbook
      saveWorkbook(wb, "AEF_sensory_evaluation.xlsx", overwrite = TRUE)
      showNotification("Data saved to AEF_sensory_evaluation.xlsx", type = "message")
    } else {
      showNotification("No data to save", type = "error")
    }
  })
  
  # Plot data for AEF
  output$aefPlot <- renderPlot({
    if (nrow(values$aef_data) > 0) {
      aef_long <- pivot_longer(values$aef_data, cols = c("Start", "Evolution", "Finish"), names_to = "Phase", values_to = "Sensation")
      aef_long$Phase <- factor(aef_long$Phase, levels = c("Start", "Evolution", "Finish")) # Ensure correct order
      total_counts <- nrow(aef_long)
      ggplot(aef_long, aes(x = Phase, fill = Sensation)) +
        geom_bar(aes(y = (..count..)/total_counts), stat = "count", position = "dodge") +
        labs(title = "AEF Sensations", x = "Phase", y = "Ratio of Answers") +
        scale_fill_manual(values = c("red", "green", "blue", "yellow", "purple", "cyan", "orange", "pink", "brown", "gray")) +
        scale_y_continuous(labels = scales::percent_format())
    }
  })
  
  # Data summary
  output$aefData <- renderTable({
    values$aef_data
  })
}

# Run the application
shinyApp(ui = ui, server = server)