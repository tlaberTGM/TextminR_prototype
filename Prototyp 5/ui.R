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

  tabPanel(getText("line_title", "Deutsch"),
           
    sidebarLayout(
     sidebarPanel(
       h1("Liniendigramme", position = "center"),
       p("Unser Werkzeug ermöglicht es, die Relevanz selbst gewählter Wörter oder die Anzahl ihrer Vorkommen innerhalb eines bestimmten Zeitraums zu visualisieren."),
       
       textInput("words", "Wörter mit Beistrich getrennt:", value = "Mutter, Vater"),
       
       sliderInput("range_datenanalyse", label = "",
                   min = 1600, max = 1900, value = c(1840, 1860), sep = "", step = 1),
       
       radioButtons("rb", "Form der Wortfrequenzen:",
                    choices = list(
                      "absolut", "relativ", "skaliert")
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
       plotOutput("trendgraph")
     )
    )
  ),
  
  tabPanel("Kartendiagramme",
           
     sidebarLayout(
       sidebarPanel(
         h1("Kartendiagramme", position = "center"),
         p("Unser Werkzeug ermöglicht die geografische Visualisierung von Textdaten, um zu zeigen, wie oft ein ausgewähltes Wort in verschiedenen Regionen innerhalb eines angegebenen Zeitraums vorkommt."),
         
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
         highchartOutput("map")
       )
     )
  ),
  
  tabPanel("Wordcloud",
     sidebarLayout(
       sidebarPanel(
         h1("Wordcloud"),
         p("Unser Werkzeug ermöglicht die Erstellung von Wortwolken, die die Häufigkeit und Relevanz von Wörtern visuell darstellen."),
         
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
  
  tabPanel("File-Upload",
   sidebarLayout(
     sidebarPanel(
       h1("Eigenständiger File-Upload"),
       fileInput("file_upload", "Datei auswählen:",
                 accept = c(".pdf", ".csv", ".txt")
       )
     ),
     mainPanel(
       # Hier können Sie Ausgaben einfügen, die zeigen, was mit der hochgeladenen Datei geschie
     )
   )
  )
)

