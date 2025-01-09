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
  
  observeEvent(input$file_upload, {
    # Sicherstellen, dass eine Datei hochgeladen wurde
    req(input$file_upload)
    
    # Pfad zur hochgeladenen Datei
    uploadedFilePath <- input$file_upload$datapath
    
    # Zielordner innerhalb des www-Verzeichnisses
    targetDir <- "www/pdf"
    
    # Stellen Sie sicher, dass das Zielverzeichnis existiert
    if (!dir.exists(targetDir)) {
      dir.create(targetDir, recursive = TRUE)
    }
    
    # Ziel-Pfad generieren
    targetFilePath <- file.path(targetDir, input$file_upload$name)
    
    # Datei in das Zielverzeichnis verschieben
    file.copy(uploadedFilePath, targetFilePath, overwrite = TRUE)
    
    # Optional: Rückmeldung an den Benutzer, dass die Datei erfolgreich gespeichert wurde
    # Dies könnte beispielsweise durch ein renderText() in der mainPanel() geschehen
  })
}