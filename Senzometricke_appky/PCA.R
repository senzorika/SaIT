# Načítanie potrebných knižníc
library(shiny)
library(shinythemes)
library(factoextra) 
library(plotly)     
library(ggplot2)
library(ggrepel)    # Pre inteligentné rozmiestnenie textu
library(MASS)       # Pre LDA

# ======================================================================================
# UI - Používateľské rozhranie
# ======================================================================================
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  
  titlePanel("Interaktívna Analýza: PCA s Predikciou pomocou LDA"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Nahrávanie dát"),
      fileInput("file1", "Vyberte CSV (s stĺpcom pre skupiny)",
                multiple = FALSE,
                accept = c("text/csv", ".csv")),
      tags$hr(),
      h4("2. Nastavenia CSV a Skupín"),
      checkboxInput("header", "Súbor obsahuje hlavičku", TRUE),
      radioButtons("sep", "Oddeľovač stĺpcov",
                   choices = c(Čiarka = ",", Bodkočiarka = ";", Tabulátor = "\t"),
                   selected = ","),
      checkboxInput("rownames", "Použiť prvý stĺpec ako názvy riadkov", TRUE),
      uiOutput("group_selector_ui"), # Dynamický výber stĺpca so skupinami
      tags$hr(),
      h4("3. Nastavenia PCA grafov"),
      sliderInput("point_size", "Veľkosť bodov:", min = 0.5, max = 5, value = 2.5, step = 0.1),
      sliderInput("alpha", "Priehľadnosť bodov:", min = 0.1, max = 1, value = 0.7, step = 0.1),
      checkboxInput("show_labels", "Zobraziť názvy produktov", TRUE),
      checkboxInput("add_ellipses", "Zobraziť elipsy skupín", TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("O aplikácii",
                 h3("Vitajte v aplikácii spájajúcej PCA a LDA!"),
                 p("Táto aplikácia vizualizuje dáta pomocou PCA a následne umožňuje interaktívne klasifikovať produkty pomocou LDA."),
                 h4("Ako postupovať:"),
                 tags$ol(
                   tags$li("Pripravte si dáta v CSV formáte. Súbor musí obsahovať aspoň jeden stĺpec s kategóriami/skupinami (napr. 'Premium', 'Standard')."),
                   tags$li("Nahrajte dáta a v ľavom paneli v časti 'Nastavenia CSV a Skupín' vyberte stĺpec, ktorý definuje skupiny."),
                   tags$li("Prejdite na záložku 'Interaktívna PCA & LDA'."),
                   tags$li(strong("Kliknite na ktorýkoľvek produkt v grafe."), " Výsledok predikcie sa okamžite zobrazí priamo pod grafom.")
                 )
        ),
        tabPanel("Náhľad dát", dataTableOutput("dataTable")),
        
        # --- ZLÚČENÁ ZÁLOŽKA PRE GRAF A PREDIKCIU ---
        tabPanel("Interaktívna PCA & LDA", 
                 h4("Graf indivíduí (Produktov)"),
                 p("Kliknite na bod v grafe pre zobrazenie LDA predikcie."),
                 plotlyOutput("pcaIndPlot", height = "550px"),
                 hr(),
                 h4("Výsledok LDA predikcie"),
                 verbatimTextOutput("lda_prediction_output")
        ),
        
        tabPanel("Biplot (Scores & Loadings)", plotlyOutput("pcaBiplot", height = "600px")),
        tabPanel("Graf premenných", plotlyOutput("pcaVarPlot", height = "600px")),
        tabPanel("Scree Plot", plotlyOutput("screePlot", height = "600px")),
        tabPanel("Súhrn PCA", verbatimTextOutput("pcaSummary"))
      )
    )
  )
)

# ======================================================================================
# SERVER - Logika aplikácie
# ======================================================================================
server <- function(input, output, session) {
  
  # --- REAKTÍVNE HODNOTY ---
  full_data <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath, header = input$header, sep = input$sep, stringsAsFactors = TRUE)
    if(input$rownames && ncol(df) > 1) {
      rownames(df) <- make.unique(as.character(df[, 1]))
      df <- df[, -1]
    }
    return(df)
  })
  
  output$group_selector_ui <- renderUI({
    req(full_data())
    choices <- names(full_data())[sapply(full_data(), function(col) is.factor(col) || is.character(col))]
    selectInput("group_col", "Vyberte stĺpec so skupinami:", choices = choices)
  })
  
  numeric_data <- reactive({ req(full_data()); full_data()[, sapply(full_data(), is.numeric), drop = FALSE] })
  grouping_variable <- reactive({ req(input$group_col); full_data()[[input$group_col]] })
  pca_results <- reactive({ req(numeric_data()); prcomp(numeric_data(), scale. = TRUE, center = TRUE) })
  lda_model <- reactive({ req(numeric_data(), grouping_variable()); lda(grouping_variable() ~ ., data = numeric_data()) })
  prediction_text <- reactiveVal("Zatiaľ nebol vybraný žiadny produkt. Kliknite na bod v grafe vyššie.")
  
  # --- VÝSTUPY ---
  output$dataTable <- renderDataTable({ full_data() })
  output$pcaSummary <- renderPrint({ summary(pca_results()) })
  
  # Upravený graf indivíduí s farbami podľa skupín
  output$pcaIndPlot <- renderPlotly({
    req(pca_results(), grouping_variable())
    p <- fviz_pca_ind(pca_results(), geom.ind = c("point", if(input$show_labels) "text"), repel = TRUE,
                      pointsize = input$point_size, alpha.ind = input$alpha,
                      habillage = grouping_variable(), addEllipses = input$add_ellipses,
                      ggtheme = theme_minimal(), title = "Mapa produktov (farebne odlíšené skupiny)")
    ggplotly(p, tooltip = "all", source = "pca_plot")
  })
  
  # Sledovanie kliknutia na graf a vykonanie predikcie
  observeEvent(event_data("plotly_click", source = "pca_plot"), {
    click_data <- event_data("plotly_click", source = "pca_plot")
    req(click_data)
    
    product_index <- click_data$pointNumber + 1
    selected_product_data <- numeric_data()[product_index, , drop = FALSE]
    product_name <- rownames(selected_product_data)
    
    prediction <- predict(lda_model(), newdata = selected_product_data)
    
    predicted_class <- as.character(prediction$class)
    probabilities <- round(prediction$posterior * 100, 2)
    prob_text <- paste(colnames(probabilities), ": ", probabilities, "%", collapse = "\n  ")
    
    final_text <- paste(
      "Produkt:", product_name, "\n",
      "-------------------------------------------\n",
      "Modelom predikovaná skupina: ", predicted_class, "\n\n",
      "Pravdepodobnosti príslušnosti k skupinám:\n  ", prob_text, sep=""
    )
    prediction_text(final_text)
  })
  
  output$lda_prediction_output <- renderPrint({ cat(prediction_text()) })
  
  # --- OPRAVENÝ BIPLOT, KTORÝ ZOBRAZUJE VŠETKY NÁZVY ---
  output$pcaBiplot <- renderPlotly({
    pca_res <- pca_results()
    req(pca_res)
    
    scores_data <- as.data.frame(pca_res$x); scores_data$name <- rownames(scores_data)
    loadings_data <- as.data.frame(pca_res$rotation); loadings_data$name <- rownames(loadings_data)
    scale_factor <- max(abs(scores_data[, 1:2])) / max(abs(loadings_data[, 1:2])) * 0.8
    explained_var <- summary(pca_res)$importance[2, 1:2] * 100
    
    p <- ggplot() +
      geom_point(data = scores_data, aes(x = PC1, y = PC2, text = name), 
                 color = "cornflowerblue", size = input$point_size, alpha = input$alpha) +
      geom_segment(data = loadings_data, aes(x = 0, y = 0, xend = PC1 * scale_factor, yend = PC2 * scale_factor),
                   arrow = arrow(length = unit(0.2, "cm")), color = "darkred") +
      labs(title = "Biplot - Produkty a Premenné",
           subtitle = "Modré body: Produkty (Scores) | Červené šípky: Premenné (Loadings)",
           x = sprintf("Komponent 1 (%.1f%%)", explained_var[1]),
           y = sprintf("Komponent 2 (%.1f%%)", explained_var[2])) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
      theme_minimal() + coord_equal()
    
    if (input$show_labels) {
      p <- p + geom_text_repel(data = scores_data, aes(x = PC1, y = PC2, label = name), 
                               color = "cornflowerblue", box.padding = 0.5)
    }
    
    p_plotly <- ggplotly(p, tooltip = "text") %>%
      add_annotations(data = loadings_data, x = ~PC1 * scale_factor * 1.15, y = ~PC2 * scale_factor * 1.15,
                      text = ~name, showarrow = FALSE, xanchor = 'center', yanchor = 'middle',
                      font = list(color = "darkred", size = 12))
    p_plotly
  })
  
  output$pcaVarPlot <- renderPlotly({
    req(pca_results())
    p <- fviz_pca_var(pca_results(), repel = TRUE, ggtheme = theme_minimal(), col.var = "contrib",
                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), title = "Mapa premenných")
    ggplotly(p)
  })
  
  output$screePlot <- renderPlotly({
    req(pca_results())
    p <- fviz_eig(pca_results(), addlabels = TRUE, ggtheme = theme_minimal(), main = "Scree Plot")
    ggplotly(p)
  })
}

# Spojenie UI a Servera
shinyApp(ui = ui, server = server)