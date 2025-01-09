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
library(shinydashboard)

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
               name = "Cities", 
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