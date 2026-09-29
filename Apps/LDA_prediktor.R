# Načítanie potrebných knižníc
library(shiny)
library(shinythemes)
library(factoextra) 
library(plotly)     
library(ggplot2)
library(ggrepel)
library(MASS)

# ======================================================================================
# UI - Používateľské rozhranie
# ======================================================================================
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("Interaktívny PCA & LDA Prediktor"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Nahrávanie dát"),
      fileInput("file1", "Vyberte CSV (s stĺpcom pre skupiny)", accept = ".csv"),
      tags$hr(),
      h4("2. Nastavenia"),
      checkboxInput("header", "Súbor obsahuje hlavičku", TRUE),
      radioButtons("sep", "Oddeľovač stĺpcov", choices = c(Čiarka = ",", Bodkočiarka = ";"), selected = ","),
      checkboxInput("rownames", "Prvý stĺpec sú názvy", TRUE),
      uiOutput("group_selector_ui"),
      tags$hr(),
      h4("3. Nastavenia grafov"),
      sliderInput("point_size", "Veľkosť bodov:", min = 0.5, max = 5, value = 2.5, step = 0.1),
      checkboxInput("add_ellipses", "Zobraziť elipsy skupín", TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("O aplikácii & Prediktor", 
                 fluidRow(
                   # --- ĽAVÝ STĹPEC S GRAFOM ---
                   column(7,
                          h3("Interaktívna PCA & LDA Analýza"),
                          p("Graf zobrazuje produkty farebne rozdelené podľa skupín. Kliknutím na existujúci bod zobrazíte jeho pôvodnú klasifikáciu."),
                          plotlyOutput("pcaIndPlot", height = "550px")
                   ),
                   # --- PRAVÝ STĹPEC S PREDIKTOROM ---
                   column(5,
                          h3("Prediktor pre nový produkt"),
                          p("Zadajte senzorické hodnoty nového produktu a kliknite na tlačidlo pre jeho zobrazenie v grafe a klasifikáciu."),
                          wellPanel(
                            uiOutput("predictor_form_ui"), # Dynamicky generovaný formulár
                            actionButton("predict_button", "Predikovať & Zobraziť", icon = icon("magic"), class = "btn-primary"),
                            hr(),
                            h4("Výsledok predikcie"),
                            verbatimTextOutput("lda_prediction_output")
                          )
                   )
                 )
        ),
        tabPanel("Náhľad dát", dataTableOutput("dataTable")),
        tabPanel("Biplot (Scores & Loadings)", plotlyOutput("pcaBiplot", height = "600px")),
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
  full_data <- reactive({ req(input$file1); read.csv(input$file1$datapath, header = input$header, sep = input$sep, stringsAsFactors = TRUE) })
  
  data_with_rownames <- reactive({
    df <- full_data()
    if(input$rownames && ncol(df) > 1) {
      rownames(df) <- make.unique(as.character(df[, 1]))
      df <- df[, -1]
    }
    return(df)
  })
  
  output$group_selector_ui <- renderUI({
    req(data_with_rownames())
    choices <- names(data_with_rownames())[sapply(data_with_rownames(), is.factor)]
    selectInput("group_col", "Stĺpec so skupinami:", choices = choices)
  })
  
  numeric_data <- reactive({ req(data_with_rownames()); data_with_rownames()[, sapply(data_with_rownames(), is.numeric), drop = FALSE] })
  grouping_variable <- reactive({ req(input$group_col); data_with_rownames()[[input$group_col]] })
  pca_results <- reactive({ req(numeric_data()); prcomp(numeric_data(), scale. = TRUE, center = TRUE) })
  lda_model <- reactive({ req(numeric_data(), grouping_variable()); lda(grouping_variable() ~ ., data = numeric_data()) })
  
  prediction_text <- reactiveVal("Zadajte hodnoty do formulára alebo kliknite na bod v grafe.")
  new_product_pca_coords <- reactiveVal(NULL) # Súradnice pre nový produkt
  
  # --- DYNAMICKÝ FORMULÁR PRE PREDIKTOR ---
  output$predictor_form_ui <- renderUI({
    req(numeric_data())
    predictor_names <- colnames(numeric_data())
    
    lapply(predictor_names, function(name) {
      numericInput(paste0("pred_", name), label = name, value = 5, min = 1, max = 10, step = 0.5)
    })
  })
  
  # --- VÝSTUPY ---
  output$dataTable <- renderDataTable({ data_with_rownames() })
  output$pcaSummary <- renderPrint({ summary(pca_results()) })
  
  # --- HLAVNÝ INTERAKTÍVNY GRAF ---
  output$pcaIndPlot <- renderPlotly({
    req(pca_results(), grouping_variable())
    
    p <- fviz_pca_ind(pca_results(), 
                      geom.ind = "point", # Text pridáme neskôr cez plotly pre lepší výkon
                      pointsize = input$point_size, 
                      habillage = grouping_variable(), 
                      addEllipses = input$add_ellipses,
                      ggtheme = theme_minimal(), 
                      title = "Mapa produktov")
    
    p_plotly <- ggplotly(p, tooltip = "all", source = "pca_plot")
    
    # Pridanie nového produktu (hviezdy) do grafu, ak existuje
    if (!is.null(new_coords <- new_product_pca_coords())) {
      p_plotly <- p_plotly %>% add_markers(
        x = new_coords[1, "PC1"], 
        y = new_coords[1, "PC2"],
        text = "Nový (predikovaný) produkt",
        name = "Nový produkt",
        symbol = I("star"), 
        color = I("black"), 
        size = I(150),
        hoverinfo = "text"
      )
    }
    
    p_plotly
  })
  
  # --- LOGIKA PRE PREDIKCIE ---
  
  # 1. Predikcia po kliknutí na TLAČIDLO (pre nový produkt)
  observeEvent(input$predict_button, {
    req(numeric_data(), lda_model())
    
    # Zozbieranie dát z formulára
    predictor_names <- colnames(numeric_data())
    new_product_values <- sapply(predictor_names, function(name) input[[paste0("pred_", name)]])
    new_product_data <- as.data.frame(t(new_product_values))
    colnames(new_product_data) <- predictor_names
    
    # Projekcia nového bodu do existujúceho PCA priestoru
    projected_pca <- predict(pca_results(), newdata = new_product_data)
    new_product_pca_coords(projected_pca) # Uloženie súradníc pre graf
    
    # LDA predikcia
    prediction <- predict(lda_model(), newdata = new_product_data)
    
    # Formátovanie výstupu
    predicted_class <- as.character(prediction$class)
    probabilities <- round(prediction$posterior * 100, 2)
    prob_text <- paste(colnames(probabilities), ": ", probabilities, "%", collapse = "\n  ")
    
    final_text <- paste(
      "--- Predikcia pre NOVÝ produkt ---\n",
      "Modelom predikovaná skupina: ", predicted_class, "\n\n",
      "Pravdepodobnosti príslušnosti k skupinám:\n  ", prob_text, sep=""
    )
    prediction_text(final_text)
  })
  
  # 2. Predikcia po kliknutí na BOD V GRAFE (pre existujúci produkt)
  observeEvent(event_data("plotly_click", source = "pca_plot"), {
    click_data <- event_data("plotly_click", source = "pca_plot")
    req(click_data)
    
    # Ignorujeme kliknutia na hviezdu (nový produkt)
    if (click_data$curveNumber > length(levels(grouping_variable())) - 1) return()
    
    product_index <- click_data$pointNumber + 1
    selected_product_data <- numeric_data()[product_index, , drop = FALSE]
    product_name <- rownames(selected_product_data)
    original_class <- as.character(grouping_variable()[product_index])
    
    prediction <- predict(lda_model(), newdata = selected_product_data)
    predicted_class <- as.character(prediction$class)
    
    final_text <- paste(
      "--- Analýza existujúceho produktu ---\n",
      "Produkt:", product_name, "\n",
      "Pôvodná skupina:", original_class, "\n",
      "Modelom predikovaná skupina:", predicted_class, "\n"
    )
    prediction_text(final_text)
    new_product_pca_coords(NULL) # Skryjeme hviezdu, ak bola zobrazená
  })
  
  # Zobrazenie výsledku predikcie
  output$lda_prediction_output <- renderPrint({ cat(prediction_text()) })
  
  # --- OSTATNÉ GRAFY A VÝSTUPY ---
  output$pcaBiplot <- renderPlotly({
    req(pca_results())
    scores <- as.data.frame(pca_results()$x); scores$name <- rownames(scores)
    loadings <- as.data.frame(pca_results()$rotation); loadings$name <- rownames(loadings)
    scale_f <- max(abs(scores[,1:2]))/max(abs(loadings[,1:2]))*0.8
    
    p <- ggplot() +
      geom_point(data = scores, aes(x=PC1, y=PC2, text=name), color="cornflowerblue", size=2) +
      geom_segment(data=loadings, aes(x=0,y=0,xend=PC1*scale_f,yend=PC2*scale_f), arrow=arrow(length=unit(0.2,"cm")), color="darkred") +
      labs(title="Biplot", x="PC1", y="PC2") + theme_minimal() + coord_equal()
    
    ggplotly(p, tooltip="text") %>% add_annotations(data=loadings, x=~PC1*scale_f*1.15, y=~PC2*scale_f*1.15, text=~name, showarrow=F, font=list(color="darkred"))
  })
}

# Spojenie UI a Servera
shinyApp(ui = ui, server = server)