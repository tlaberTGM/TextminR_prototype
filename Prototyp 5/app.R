library(shiny)
library(shinyjs)
library(leaflet)
library(ggplot2)
library(reshape2)
library(stringr)
library(tidytext)
library(wordcloud2)
library(dplyr)
library(highcharter)
library(scales)
library(httr)
library(jsonlite)
library(tm)
library(htmltools)

texts <- read.csv("translations.csv", sep = "|")

getText <- function(id, language) {
  return(texts[texts$id == id, language])
}

getLanguage <- function() {
  return(language())
}

scatterplot_dep = function() {
  htmlDependency(
    version = "0.1",
    name = "scatterplot-assets",
    src = "assets",
    script = list(src = "js/script.js", type = "module")
  )
}

url <- "https://api.textminr.tech/texts/metadata_with_texts"
headers <- c(
  'accept' = 'application/json',
  'X-API-Key' = 'horse8Snyder)candle.'
)

count_words <- function(text, option) {
  if(option == "absolut") {
    result <- str_count(text, fixed(word, ignore_case = TRUE))
  } else {
    result <- str_count(text, fixed(word, ignore_case = TRUE)) / str_count(text, "\\b\\w+\\b")
  }
  return(result)
}

getMap <- function(years_start, years_end, word, option, output, sampled) {
  
  if(sampled) {
    res <- GET(url, add_headers(.headers=headers), query=list(minYear=years_start, maxYear=years_end, combined_texts="false", sampled="true"))
  } else {
    res <- GET(url, add_headers(.headers=headers), query=list(minYear=years_start, maxYear=years_end, combined_texts="false"))
  }
  content <- fromJSON(rawToChar(res$content))
  data <- content$data
  
  colnames(data)[1] <- "id"
  data$id <- NULL
  data$title <- NULL
  data$year <- NULL
  data$language <- NULL
  data$source <- NULL
  data$birth_place <- NA
  data$country <- NA
  data$lat <- NA
  data$long <- NA
  data$freq <- NA
  
  res <- GET("https://api.textminr.tech/authors", add_headers(.headers=headers))
  content_author <- fromJSON(rawToChar(res$content))
  authorData <- content_author$data
  
  if(sampled) {
    for(i in 1:nrow(data)) {
      text <- data$text[i]
      temp_freq <- 0
      
      temp_freq <- temp_freq + count_words(text, option)
      
      if(option == "absolut") {
        data$freq[i] <- temp_freq
      } else {
        data$freq[i] <- temp_freq / j
      }
      
      filtered <- authorData %>% filter(name == data$author[i])
      
      if (nrow(filtered) > 0) {
        data$birth_place[i] <- filtered$birth_place
        data$country[i] <- filtered$country
        data$lat[i] <- filtered$lat
        data$long[i] <- filtered$long
      }
    }
  } else {
    for(i in 1:nrow(data)) {
      text <- data$text[i][[1]]
      temp_freq <- 0
      
      for(j in 1:nrow(text)) {
        temp_freq <- temp_freq + count_words(text$part[j], option)
      }
      
      if(option == "absolut") {
        data$freq[i] <- temp_freq
      } else {
        data$freq[i] <- temp_freq / j
      }
      
      filtered <- authorData %>% filter(name == data$author[i])
      
      if (nrow(filtered) > 0) {
        data$birth_place[i] <- filtered$birth_place
        data$country[i] <- filtered$country
        data$lat[i] <- filtered$lat
        data$long[i] <- filtered$long
      }
    }
  }
  
  data <- na.omit(data)
  data$lat <- as.numeric(data$lat)
  data$long <- as.numeric(data$long)
  
  map <- download_map_data("custom/world")
  
  if(output == "pro Stadt") {
    if(option == "absolut") {
      data_bubble <- data %>%
        group_by(birth_place) %>%
        summarise(
          lat = first(lat),
          long = first(long),
          freq = sum(freq, na.rm = TRUE)
        ) %>%
        ungroup()
    } else {
      data_bubble <- data %>%
        group_by(birth_place) %>%
        summarise(
          lat = first(lat),
          long = first(long),
          freq = mean(freq, na.rm = TRUE)
        ) %>%
        ungroup()
      
      data_bubble$freq <- round(data_bubble$freq * 100, 2)
    }
    
    colnames(data_bubble)[1] <- "name"
    colnames(data_bubble)[3] <- "lon"
    colnames(data_bubble)[4] <- "z"
    data_bubble <- subset(data_bubble, z != 0)
    
    return(highchart(type = "map") |> 
             hc_add_series(
               mapData = map,
               name = "Basemap"
             ) %>%
             hc_add_series(
               data = data_bubble, 
               type = "mapbubble",
               name = "Countries", 
               maxSize = "12%"
             ) %>%
             hc_mapNavigation(enabled = TRUE))
    
  } else {
    if(option == "absolut") {
      data_countries <- data %>%
        group_by(country) %>%
        summarise(freq = sum(freq, na.rm = TRUE)) %>%
        ungroup()
    } else {
      data_countries <- data %>%
        group_by(country) %>%
        summarise(freq = mean(freq, na.rm = TRUE)) %>%
        ungroup()
      
      data_countries$freq <- round(data_countries$freq * 100, 2)
    }
    
    colnames(data_countries)[1] <- "code"
    colnames(data_countries)[2] <- "value"
    data_countries <- subset(data_countries, value != 0)
    
    return(highchart(type = "map") |> 
             hc_add_series_map(
               map = map,
               df = data_countries, 
               value = "value",
               joinBy = c("hc-a2", "code"), 
               name = "Countries"
             ) %>%
             hc_mapNavigation(enabled = TRUE))
  }
}

getTrendgraph <- function(years_start, years_end, words, option, sampled, output) {
  url <- "https://api.textminr.tech/texts/metadata_with_texts"
  headers <- c(
    'accept' = 'application/json',
    'X-API-Key' = 'horse8Snyder)candle.'
  )
  
  words <- strsplit(words, ",")[[1]]
  words <- trimws(words)
  if (!length(words)) {
    return(NULL)
  }
  
  count_words <- function(text, option) {
    if(option == "absolut") {
      result <- str_count(text, fixed(word, ignore_case = TRUE))
    } else {
      result <- str_count(text, fixed(word, ignore_case = TRUE)) / str_count(text, "\\b\\w+\\b")
    }
    return(result)
  }
  
  if(sampled) {
    res <- GET(url, add_headers(.headers=headers), query=list(minYear=years_start, maxYear=years_end, combined_texts="false", sampled="true"))
  } else {
    res <- GET(url, add_headers(.headers=headers), query=list(minYear=years_start, maxYear=years_end, combined_texts="false"))
  }
  content <- fromJSON(rawToChar(res$content))
  data_trendgraph <- content$data %>% select(-c(`_id`, author, title, language, source))
  
  for(word in words) {
    data_trendgraph[[word]]
    
    if(sampled) {
      for(i in 1:nrow(data_trendgraph)) {
        text <- data_trendgraph$text[i]
        data_trendgraph[[word]][i] <- tryCatch({
          count_words(text, option)
        }, error = function(e) {
          NA
        })
      }
    } else {
      for(i in 1:nrow(data_trendgraph)) {
        text <- data_trendgraph$text[i][[1]]
        temp_freq <- 0
        
        for(j in 1:nrow(text)) {
          temp_freq <- temp_freq + count_words(text$part[j], option)
        }
        
        data_trendgraph[[word]][i] <- temp_freq
      }
    }
  }
  
  data_trendgraph_plot <- data.frame(year = unique(data_trendgraph$year))
  
  for(word in words) {
    data_trendgraph_plot[[word]]
    temp_plot <- data_trendgraph %>%
      group_by(year) %>%
      summarise(!!word := sum(across(all_of(word)), na.rm = TRUE)) %>%
      ungroup()
    
    data_trendgraph_plot[[word]] <- temp_plot[[word]]
  }
  
  if(option == "normalisiert") {
    for(word in words) {
      min_value <- min(data_trendgraph_plot[[word]], na.rm = TRUE)
      max_value <- max(data_trendgraph_plot[[word]], na.rm = TRUE)
      
      data_trendgraph_plot[[word]] <- (data_trendgraph_plot[[word]] - min_value) / (max_value - min_value)
    }
  }
  
  data_melted <- melt(data_trendgraph_plot, id.vars = c("year"))
  
  if(output == "Regression") {
    ggplot() +
      geom_smooth(data_melted, mapping = aes(year, value, color = variable), se = FALSE) +
      labs(title = "Wortfrequenzen (über alle Jahre)", x = "Zeit", y = "Frequenz")
  } else {
    ggplot() +
      geom_line(data_melted, mapping = aes(year, value, color = variable), se = FALSE) +
      labs(title = "Wortfrequenzen (über alle Jahre)", x = "Zeit", y = "Frequenz")
  }
}

getWordcloud <- function(years_start, years_end, lim) {
  
  res <- GET(url, add_headers(.headers=headers), query=list(minYear=years_start, maxYear=years_end, sampled="true"))
  content <- fromJSON(rawToChar(res$content))
  data_wordcloud <- content$data
  
  data_wordcloud$`_id` <- NULL
  data_wordcloud$author <- NULL
  data_wordcloud$title <- NULL
  data_wordcloud$year <- NULL
  data_wordcloud$language <- NULL
  data_wordcloud$source <- NULL
  
  corpus <- Corpus(VectorSource(data_wordcloud$text))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("german"))
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  
  tdm <- TermDocumentMatrix(corpus)
  
  m <- as.matrix(tdm)
  word_freqs <- sort(rowSums(m), decreasing = TRUE)
  top_words <- data.frame(word = names(word_freqs), freq = word_freqs)
  
  top_words <- head(top_words, lim)
  
  wordcloud2(data.frame(name = top_words$word, size = top_words$freq),
             size = 1, backgroundColor = "white", fontFamily = "sans-serif"
  )
}

ui <- navbarPage("TexminR - Prototyp",
  useShinyjs(),
  
  tabPanel("Home",
            
    sidebarLayout(
      sidebarPanel(
        h1("Wort des Tages", position = "center"),
        p("*Beispiel*"),
        
        h3("Der Gebrauch von *Beispiel* in den letzten ", textOutput("years_home"), " Jahren"),
        
        plotOutput("empty_plot_home"),
        
        sliderInput("range_home", label = "",
                    min = 1800, max = 2023, value = c(1923, 2023), sep = "", step = 1)
      ),
      
      mainPanel(
        leafletOutput("map_leaflet", width = "1000px", height = "730px")
      )
    )
   ),
   
   tabPanel(getText("line_title", language()),
            
    sidebarLayout(
      sidebarPanel(
        h1(getText("line_title", language()), position = "center"),
        p(getText("line_decription", language())),
        
        textInput("words", "Wörter mit Beistrich getrennt:", value = "Mutter, Vater"),
        
        sliderInput("range_datenanalyse", label = "",
                    min = 1600, max = 1900, value = c(1840, 1860), sep = "", step = 1),
        
        radioButtons("rb", "Form der Wortfrequenzen:",
                     choices = list(
                       "absolut", "relativ", "normalisiert")
        ),
        
        actionButton("liniendiagramme_1", label = NULL, icon = icon("info-circle"), class = "btn btn-info"),
        br(),
        br(),
        
        radioButtons("rb_2", "Ausgangstextumfang:",
                     choices = list(
                       "Volltext", "Stichproben")
        ),
        
        actionButton("liniendiagramme_2", label = NULL, icon = icon("info-circle"), class = "btn btn-info"),
        br(),
        br(),
        
        radioButtons("rb_3", "Ausgabeformat:",
                     choices = list(
                       "Originaldaten", "Regression")
        ),
        actionButton("liniendiagramme_3", label = NULL, icon = icon("info-circle"), class = "btn btn-info"),
        br(),
        br(),
        
        actionButton("btn_generate_trendgraph", "Generate", class = "btn-primary")
      ),
      
      mainPanel(
        plotOutput("trendgraph", width = "100%", height = "100%")
      )
    )
   ),
   
   tabPanel("Kartendiagramme",
            
    sidebarLayout(
      sidebarPanel(
        h1("Kartendiagramme", position = "center"),
        p("*Beispiel*"),
        
        textInput("word_map", "gewünschtes Wort", value = "Mutter"),
        
        sliderInput("range_map", label = "",
                    min = 1700, max = 1900, value = c(1840, 1850), sep = "", step = 1),
        
        radioButtons("rb_map_2", "Wortfrequenzen:",
                     choices = list(
                       "absolut", "relativ")
        ),
        
        actionButton("map_1", label = NULL, icon = icon("info-circle"), class = "btn btn-info"),
        br(),
        br(),
        
        radioButtons("rb_map_3", "Ausgangstextumang:",
                     choices = list(
                       "Volltext", "Stichproben")
        ),
        
        actionButton("map_2", label = NULL, icon = icon("info-circle"), class = "btn btn-info"),
        br(),
        br(),
        
        radioButtons("rb_map", "Ausgabevormat:",
                     choices = list(
                       "pro Stadt", "pro Land")
        ),
        
        actionButton("map_3", label = NULL, icon = icon("info-circle"), class = "btn btn-info"),
        br(),
        br(),
        
        actionButton("btn_generate_map", "Generate", class = "btn-primary")
      ),
      
      mainPanel(
        highchartOutput("map", width = "100%", height = "100%")
      )
    )
   ),
   
   tabPanel("Wordcloud",
      sidebarLayout(
        sidebarPanel(
          h1("Wordcloud"),
          
          sliderInput("num_words", "Anzahl an Wörtern in der Wordcloud:",
                      min = 1, max = 500, value = 300, step = 10),
          sliderInput("lim", "",
                      min = 1600, max = 1900, value = c(1840, 1850), sep = "", step = 1),
          actionButton("btn_generate_wordcloud", "Generate", class = "btn-primary")
        ),
        
        mainPanel(
          wordcloud2Output("wordcloud", width = "100%", height = "600px")
        )
      )
   ),
   
   tabPanel("Scatterplot",
            
    sidebarLayout(
      sidebarPanel(
        h1("Scatterplot"),
        
        sliderInput("range", "Filter nach Jahren", 1700, 2000, c(1800, 1900), sep = ""),
        actionButton("btn_generate_scatterplot", "Generate", class = "btn-primary")
      ),
      
      mainPanel(
        div(id = "scatterplot"),
        
        attachDependencies(
          "scatterplot",
          scatterplot_dep()
        )
      )
    )
   ),
   
   tabPanel("File Upload",
            
      div(class = "container",
          div(class = "custom-file-input",
              fileInput("file", label = "Choose a file")
          )
      ),
      
      verbatimTextOutput("file_info"),
   )
)

server <- function(input, output, session) {
  
  output$years_home <- renderText({
    paste(input$range_home[2] - input$range_home[1])
  })
  
  output$years_datenanalyse <- renderText({
    paste(input$range_datenanalyse[2] - input$range_datenanalyse[1])
  })
  
  output$map_leaflet <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng=16.369528, lat=48.236619, popup="TGM") 
  })
  
  output$empty_plot_home <- renderPlot({
    plot(0, type = "n", xlab = "", ylab = "")
  })
  
  trendgraph_plot <- eventReactive(input$btn_generate_trendgraph, {
    if(input$rb_2 == "Volltext") {
      sampled <- FALSE
    } else {
      sampled <- TRUE
    }
    
    return(getTrendgraph(input$range_datenanalyse[1], input$range_datenanalyse[2], input$words, input$rb, sampled, input$rb_3))
  })
  
  output$trendgraph <- renderPlot({
    trendgraph_plot()
  })
  
  wordcloud_plot <- eventReactive(input$btn_generate_wordcloud, {
    getWordcloud(input$lim[1], input$lim[2], input$num_words)
  })
  
  output$wordcloud <- renderWordcloud2({
    wordcloud_plot()
  })
  
  output$textOutput <- renderText({
    input$textInput
  })
  
  map_plot <- eventReactive(input$btn_generate_map, {
    if(input$rb_map_3 == "Volltext") {
      sampled <- FALSE
    } else {
      sampled <- TRUE
    }
    
    getMap(input$range_map[1], input$range_map[2], input$word_map, input$rb_map_2, input$rb_map, sampled)
  })
  
  output$map <- renderHighchart({
    map_plot()
  })
  
  observeEvent(input$range, {
    session$sendCustomMessage("range", c(input$range[1], input$range[2]))
  })
  
  showModal(modalDialog(
    title = "Welcome to TexminR!",
    h4("Please select language:"),
    selectInput("language", "Language:", choices = c("English", "Deutsch")),
    actionButton("ok", "OK"),
    easyClose = FALSE
  ))
  
  language <- reactiveVal("English")
  
  observeEvent(input$ok, {
    language(input$language)
    removeModal()
  })
  
  observeEvent(input$liniendiagramme_1, {
    showModal(modalDialog(
      title = strong(getText("line_rb_1", language())),
      tags$div(
        strong(getText("line_rb_1_info_1_title", language())), tags$br(),
        getText("line_rb_1_info_1_text", language()), tags$br(), tags$br(),
        strong(getText("line_rb_1_info_2_title", language())), tags$br(),
        getText("line_rb_1_info_2_text", language()), tags$br(), tags$br(),
        strong(getText("line_rb_1_info_3_title", language())), tags$br(),
        getText("line_rb_1_info_3_text", language()), tags$br()
      )
    ))
  })
  
  observeEvent(input$liniendiagramme_2, {
    showModal(modalDialog(
      title = strong(getText("line_rb_2"), language()),
      tags$div(
        strong(getText("line_rb_2_1", language())), tags$br(),
        getText("line_rb_2_info_1", language()), tags$br(), tags$br(),
        strong(getText("line_rb_2_2", language())), tags$br(),
        getText("line_rb_2_info_2", language()), tags$br(), tags$br(),
      )
    ))
  })
  
  observeEvent(input$liniendiagramme_3, {
    showModal(modalDialog(
      title = strong(getText("line_rb_3"), language()),
      tags$div(
        strong(getText("line_rb_3_1", language())), tags$br(),
        getText("line_rb_3_info_1", language()), tags$br(), tags$br(),
        strong(getText("line_rb_3_2", language())), tags$br(),
        getText("line_rb_3_info_2", language()), tags$br(), tags$br(),
      )
    ))
  })
  
  observeEvent(input$map_1, {
    showModal(modalDialog(
      title = strong(getText("map_rb_1", language())),
      tags$div(
        strong(getText("map_rb_1_info_1_title", language())), tags$br(),
        getText("map_rb_1_info_1_text", language()), tags$br(), tags$br(),
        strong(getText("map_rb_1_info_2_title", language())), tags$br(),
        getText("map_rb_1_info_2_text", language()), tags$br(), tags$br(),
      )
    ))
  })
  
  observeEvent(input$map_2, {
    showModal(modalDialog(
      title = strong(getText("map_rb_2", language())),
      tags$div(
        strong(getText("map_rb_2_1", language())), tags$br(),
        getText("map_rb_2_info_1", language()), tags$br(), tags$br(),
        strong(getText("map_rb_2_2", language())), tags$br(),
        getText("map_rb_2_info_2", language()), tags$br(), tags$br(),
      )
    ))
  })
  
  observeEvent(input$map_3, {
    showModal(modalDialog(
      title = strong(getText("map_rb_3", language())),
      tags$div(
        strong(getText("map_rb_3_1", language())), tags$br(),
        getText("map_rb_3_info_1", language()), tags$br(), tags$br(),
        strong(getText("map_rb_3_2", language())), tags$br(),
        getText("map_rb_3_info_2", language()), tags$br(), tags$br(),
      )
    ))
  })
}

shinyApp(ui = ui, server = server)
