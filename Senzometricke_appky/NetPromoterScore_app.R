library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(ggplot2)
library(readr)

ui <- dashboardPage(
  dashboardHeader(title = "Net Promoter Score (NPS)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("NPS Dotazník", tabName = "survey", icon = icon("clipboard")),
      menuItem("Výsledky", tabName = "results", icon = icon("chart-bar")),
      menuItem("Export", tabName = "export", icon = icon("download"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "survey",
              fluidRow(
                box(title = "NPS Dotazník", status = "primary", solidHeader = TRUE, width = 12,
                    sliderInput("rating", "Ako pravdepodobné je, že by ste odporučili náš produkt priateľovi alebo kolegovi?",
                                min = 0, max = 10, value = 0),
                    textInput("error_description", "Opíšte chybu (ak je hodnotenie 6 a menej):", ""),
                    actionButton("submit", "Odoslať")
                )
              )
      ),
      tabItem(tabName = "results",
              fluidRow(
                box(title = "Výsledky NPS", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("npsPlot"),
                    verbatimTextOutput("npsScore"),
                    plotlyOutput("satisfactionGauge")
                )
              )
      ),
      tabItem(tabName = "export",
              fluidRow(
                box(title = "Exportovať Dáta a Grafy", status = "primary", solidHeader = TRUE, width = 12,
                    downloadButton("downloadData", "Stiahnuť Dáta"),
                    downloadButton("downloadNpsPlot", "Stiahnuť NPS Graf"),
                    downloadButton("downloadGaugePlot", "Stiahnuť Tachometer Graf")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  ratings <- reactiveValues(data = data.frame(Rating = integer(), ErrorDescription = character(), stringsAsFactors = FALSE))
  
  observeEvent(input$submit, {
    if (input$rating <= 6 && input$error_description == "") {
      showModal(modalDialog(
        title = "Chyba",
        "Prosím, opíšte chybu pri hodnotení 6 a menej.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      new_entry <- data.frame(Rating = input$rating, ErrorDescription = input$error_description, stringsAsFactors = FALSE)
      ratings$data <- rbind(ratings$data, new_entry)
      updateSliderInput(session, "rating", value = 0)
      updateTextInput(session, "error_description", value = "")
    }
  })
  
  output$npsPlot <- renderPlotly({
    if(nrow(ratings$data) == 0) return(NULL)
    
    ratings_summary <- ratings$data %>%
      mutate(Category = case_when(
        Rating >= 9 ~ "Promoter",
        Rating >= 7 ~ "Passive",
        TRUE ~ "Detractor"
      )) %>%
      count(Category)
    
    plot_ly(ratings_summary, x = ~Category, y = ~n, type = 'bar', marker = list(color = c('green', 'yellow', 'red'))) %>%
      layout(title = 'Distribúcia NPS', xaxis = list(title = 'Kategória'), yaxis = list(title = 'Počet'))
  })
  
  output$npsScore <- renderPrint({
    if(nrow(ratings$data) == 0) return("Nie sú dostupné žiadne údaje")
    
    n_promoters <- sum(ratings$data$Rating >= 9)
    n_detractors <- sum(ratings$data$Rating <= 6)
    n_total <- nrow(ratings$data)
    
    nps <- ((n_promoters - n_detractors) / n_total) * 100
    paste("Net Promoter Score (NPS):", round(nps, 2))
  })
  
  output$satisfactionGauge <- renderPlotly({
    if(nrow(ratings$data) == 0) return(NULL)
    
    avg_rating <- mean(ratings$data$Rating)
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = avg_rating,
      gauge = list(
        axis = list(range = list(0, 10)),
        steps = list(
          list(range = c(0, 6), color = "red"),
          list(range = c(6, 8), color = "yellow"),
          list(range = c(8, 10), color = "green")
        ),
        threshold = list(
          line = list(color = "black", width = 4),
          thickness = 0.75,
          value = avg_rating
        )
      )
    ) %>%
      layout(title = "Miera Spokojnosti Zákazníkov")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("nps_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write_csv(ratings$data, file)
    }
  )
  
  output$downloadNpsPlot <- downloadHandler(
    filename = function() {
      paste("nps_plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      p <- ratings$data %>%
        mutate(Category = case_when(
          Rating >= 9 ~ "Promoter",
          Rating >= 7 ~ "Passive",
          TRUE ~ "Detractor"
        )) %>%
        count(Category) %>%
        ggplot(aes(x = Category, y = n, fill = Category)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("Promoter" = "green", "Passive" = "yellow", "Detractor" = "red")) +
        theme_minimal() +
        labs(title = "Distribúcia NPS", x = "Kategória", y = "Počet")
      
      ggsave(file, plot = p)
    }
  )
  
  output$downloadGaugePlot <- downloadHandler(
    filename = function() {
      paste("gauge_plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      avg_rating <- mean(ratings$data$Rating)
      
      p <- plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = avg_rating,
        gauge = list(
          axis = list(range = list(0, 10)),
          steps = list(
            list(range = c(0, 6), color = "red"),
            list(range = c(6, 8), color = "yellow"),
            list(range = c(8, 10), color = "lightgreen")
          ),
          threshold = list(
            line = list(color = "black", width = 4),
            thickness = 0.75,
            value = avg_rating
          )
        )
      ) %>%
        layout(title = "Miera Spokojnosti Zákazníkov")
      
      export(p, file = file)
    }
  )
}

shinyApp(ui, server)