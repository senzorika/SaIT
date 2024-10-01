# Openfoodfacts explorer, V.Vietoris 2024
# online verzia: https://pr020a-sensorylab-fbp.shinyapps.io/kody_R/
# Načítanie potrebných knižníc
library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(DT)
library(tidyverse)
library(writexl)
library(ggplot2)

# Definícia UI
ui <- fluidPage(
  titlePanel("Prieskumník Open Food Facts"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("product_name", "Zadajte názov produktu:", value = ""),
      selectInput("category", "Vyberte kategóriu:", choices = c("Všetko", "Snacks", "Nápoje", "Mliečne výrobky", "Cereálie")),
      sliderInput("energy_range", "Energia (kcal) rozsah:",
                  min = 0, max = 1000, value = c(0, 1000)),
      actionButton("fetch_data", "Načítať dáta"),
      downloadButton("save_xlsx", "Uložiť do XLSX")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Dátová tabuľka", DTOutput("data_table")),
        tabPanel("Sumár", 
                 textOutput("summary_text"), 
                 plotOutput("calories_plot")) # Výstup pre graf kalórií
      )
    )
  )
)

# Definícia serverovej logiky
server <- function(input, output, session) {
  
  # Reaktívna hodnota na uloženie datasetu
  product_data <- reactiveVal()
  
  # Funkcia na získanie dát z Open Food Facts API
  fetch_openfoodfacts_data <- function(product_name, category) {
    base_url <- "https://world.openfoodfacts.org/cgi/search.pl"
    params <- list(
      search_terms = product_name,
      search_simple = 1,
      action = "process",
      json = 1
    )
    
    if (category != "Všetko") {
      params$tagtype_0 <- "categories"
      params$tag_contains_0 <- "contains"
      params$tag_0 <- category
    }
    
    # Načítanie dát z API
    response <- GET(base_url, query = params)
    
    # Konverzia JSON odpovede do R zoznamu
    content_list <- content(response, as = "parsed", type = "application/json")
    
    # Konverzia zoznamu na dataframe
    if (!is.null(content_list$products)) {
      df <- content_list$products %>%
        purrr::map_df(~ {
          tibble(
            product_name = .x$product_name,
            brand = .x$brands,
            category = .x$categories_tags %>% paste(collapse = ", "),
            country = .x$countries_tags %>% paste(collapse = ", "),
            ingredients = .x$ingredients_text,
            energy_kcal = as.numeric(.x$nutriments$energy_100g),
            fat_g = as.numeric(.x$nutriments$fat_100g),
            carbs_g = as.numeric(.x$nutriments$carbohydrates_100g),
            sugars_g = as.numeric(.x$nutriments$sugars_100g),
            proteins_g = as.numeric(.x$nutriments$proteins_100g),
            salt_g = as.numeric(.x$nutriments$salt_100g),
            nutriscore = .x$nutriscore_grade,  # Pridanie nutriscore
            weblink = ifelse(!is.null(.x$url), paste0('<a href="', .x$url, '" target="_blank">Odkaz</a>'), NA)  # Pridanie HTML odkazu na produkt
          )
        })
      return(df)
    } else {
      return(NULL)
    }
  }
  
  # Načítanie dát po kliknutí na tlačidlo
  observeEvent(input$fetch_data, {
    showModal(modalDialog("Načítavanie dát... Prosím, čakajte.", easyClose = FALSE))
    
    df <- fetch_openfoodfacts_data(input$product_name, input$category)
    
    # Aktualizácia reaktívnej hodnoty
    if (!is.null(df)) {
      product_data(df)
    } else {
      showModal(modalDialog("Žiadne dáta sa nenašli.", easyClose = TRUE))
    }
    
    removeModal()
  })
  
  # Vykreslenie dátovej tabuľky
  output$data_table <- renderDT({
    req(product_data())  # Uistite sa, že product_data je dostupná
    df <- product_data()
    
    # Filtrovanie dát na základe rozsahu energie
    df_filtered <- df %>%
      filter(energy_kcal >= input$energy_range[1] & energy_kcal <= input$energy_range[2])
    
    # Vykreslenie dátovej tabuľky s nutriscore a odkazom na produkt
    datatable(
      df_filtered, 
      options = list(pageLength = 10, autoWidth = TRUE), 
      escape = FALSE,  # Escape = FALSE umožňuje vykresliť HTML odkazy
      rownames = FALSE
    ) %>% 
      formatStyle("nutriscore", 
                  backgroundColor = styleEqual(c("a", "b", "c", "d", "e"), 
                                               c("green", "lightgreen", "yellow", "orange", "red")))
  })
  
  # Vykreslenie sumárneho textu
  output$summary_text <- renderText({
    req(product_data())  # Uistite sa, že product_data je dostupná
    df <- product_data()
    filtered_df <- df %>% filter(energy_kcal >= input$energy_range[1] & energy_kcal <= input$energy_range[2])
    
    # Výpočet sumárnych štatistík
    avg_energy <- mean(filtered_df$energy_kcal, na.rm = TRUE)
    avg_fat <- mean(filtered_df$fat_g, na.rm = TRUE)
    avg_carbs <- mean(filtered_df$carbs_g, na.rm = TRUE)
    total_countries <- filtered_df$country %>% unique() %>% length()
    nutriscore_distribution <- table(filtered_df$nutriscore)
    
    paste(
      "Sumár vybraných dát:",
      sprintf("\nPriemerná energia: %.2f kcal", avg_energy),
      sprintf("\nPriemerný obsah tuku: %.2f g", avg_fat),
      sprintf("\nPriemerné množstvo sacharidov: %.2f g", avg_carbs),
      sprintf("\nPočet krajín: %d", total_countries),
      sprintf("\nRozdelenie nutriscore: %s", paste(names(nutriscore_distribution), nutriscore_distribution, sep = ": ", collapse = ", ")),  # Rozdelenie nutriscore
      sep = "\n"
    )
  })
  
  # Vykreslenie grafu pre kalórie
  output$calories_plot <- renderPlot({
    req(product_data())  # Uistite sa, že product_data je dostupná
    df <- product_data()
    filtered_df <- df %>% filter(energy_kcal >= input$energy_range[1] & energy_kcal <= input$energy_range[2])
    
    # Výpočet priemernej hodnoty kalórií
    avg_energy <- mean(filtered_df$energy_kcal, na.rm = TRUE)
    
    # Pridanie stĺpca pre farby na základe priemeru
    filtered_df <- filtered_df %>%
      mutate(color = ifelse(energy_kcal > avg_energy, "red", "skyblue"))
    
    # Vytvorenie grafu pre rozdelenie kalórií s podmienenými farbami
    ggplot(filtered_df, aes(x = reorder(product_name, -energy_kcal), y = energy_kcal, fill = color)) +
      geom_bar(stat = "identity") +
      scale_fill_identity() +
      coord_flip() +
      theme_minimal() +
      labs(title = "Rozdelenie kalórií vybraných produktov", x = "Názov produktu", y = "Kalórie (kcal)") +
      theme(legend.position = "none")  # Skrytie legendy, keďže máme priamu farebnú škálu
  })
  
  # Uloženie dát do Excel súboru
  output$save_xlsx <- downloadHandler(
    filename = function() {
      paste("openfoodfacts_data_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      df <- product_data()
      if (!is.null(df)) {
        # Filtrovanie dát na základe rozsahu energie
        df_filtered <- df %>%
          filter(energy_kcal >= input$energy_range[1] & energy_kcal <= input$energy_range[2])
        
        # Uloženie do Excelu
        write_xlsx(df_filtered, path = file)
      }
    }
  )
}

# Spustenie aplikácie
shinyApp(ui = ui, server = server)
