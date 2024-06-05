
body_hoehe = function(){
    return(
        fluidPage(
            box(
                title = HTML("Nutzereingabe Baumhöhe <small>| User input tree height</small>"),
                width = "100%",
                fluidRow(
                    column(4, selectInput("method_hoehe", 
                                          label = HTML("Methode <small>| Method</small>"),
                                          choices = list("Raten | Guess" = "Raten | Guess",
                                                         "Laser" = "Laser",
                                                         "App" = "App"))),
                    column(4, selectInput("tree_hoehe", 
                                          label = HTML("Baum ID <small>| Tree ID</small>"),
                                          choices = list("B1", "B2", "B3"))),
                    column(4, numericInput("value_hoehe",
                                           label = HTML("Wert (m) <small>| Value (m)</small>"),
                                           min = 0, max = 100,
                                           value = 0))
                ),
                
                actionButton("updateHeights", label = "Update", 
                             style = "background:purple; color:white;
                             margin-top:20px; margin-bottom:20px"),
                
                sliderInput("pdata_heights",
                            label = HTML("Datenpunkt (%) <small>| Data points (%)</small>"),
                            min = 0,
                            max = 100,
                            value = 100,
                            width = "100%")
            ),
            
            box(
                title = HTML("Visualisierung <small>| Visualization</small>"),
                width = "100%",
                tabsetPanel(
                    tabPanel(HTML("Baum 1 <small>| Tree 1</small>"), plotOutput("b1boxplot_height")),
                    tabPanel(HTML("Baum 2 <small>| Tree 2</small>"), plotOutput("b2boxplot_height")),
                    tabPanel(HTML("Baum 3 <small>| Tree 3</small>"), plotOutput("b3boxplot_height")),
                    tabPanel(HTML("Zeitreihe <small>| Time series</small>"), plotOutput("timeseries_height"))
                    
                )
            )
        ))
}




body_dbh = function(){
    return(
        fluidPage(
            box(
                title = HTML("Nutzereingabe Brusthöhendurchmesser (BHD) <small>| User input DBH</small>"),
                width = "100%",
                fluidRow(
                    column(4, selectInput("method_dbh",
                                          label = HTML("Methode <small>| Method</small>"),
                                          choices = list("Raten | Guess" = "Raten | Guess",
                                                         "Massband | Tapeline" = "Massband | Tapeline",
                                                         "Kluppe | Caliper" = "Kluppe | Caliper"))),
                    column(4, selectInput("tree_dbh", 
                                          label = HTML("Baum ID <small>| Tree ID</small>"),
                                          choices = list("B1", "B2", "B3"))),
                    column(4, numericInput("value_dbh", 
                                           label = HTML("Wert (cm) <small>| Value (cm)</small>"),
                                           min = 0, max = 100,
                                           value = 0))
                ),
                
                actionButton("updateDBH", label = "Update", 
                             style = "background:purple; color:white;
                             margin-top:20px; margin-bottom:20px"),
                
                sliderInput("pdata_dbh",
                            label = HTML("Datenpunkt (%) <small>| Data points (%)</small>"),
                            min = 0,
                            max = 100,
                            value = 100,
                            width = "100%")
            ),
            
            box(
                title = HTML("Visualisierung <small>| Visualization</small>"),
                
                width = "100%",
                tabsetPanel(
                    tabPanel(HTML("Baum 1 <small>| Tree 1</small>"), plotOutput("b1boxplot_dbh")),
                    tabPanel(HTML("Baum 2 <small>| Tree 2</small>"), plotOutput("b2boxplot_dbh")),
                    tabPanel(HTML("Baum 3 <small>| Tree 3</small>"), plotOutput("b3boxplot_dbh")),
                    tabPanel(HTML("Zeitreihe <small>| Time series</small>"), plotOutput("timeseries_dbh"))
                )
            )
        ))
}


body_help = function(){
    return(
        fluidPage(
            box(
                title = "Was ist ein Boxplot?",
                width = "100%",
                fluidRow(column(12, img(src='boxplot_wikipedia.png', align = "left"))),
                p("Quelle: https://de.wikipedia.org/wiki/Box-Plot")
                
            ),
            box(
                title = "Messen",
                width = "100%",
                
                tabsetPanel(
                    tabPanel(HTML("Kluppe <small>| Using a Caliper</small>"),
                             tags$br(),
                             includeMarkdown("./www/help_caliper.md")),
                    tabPanel(HTML("Massband 2 <small>| Diameter tape</small>"), 
                             tags$br(),
                             includeMarkdown("./www/help_dtape.md")),
                    tabPanel(HTML("Höhe <small>| Height</small>"),
                             h4("Using a Smartphone App"),
                             fluidRow(column(12, 
                                             img(src='help_height.jpg',
                                                 style="width:400px;max-width:100%;height:auto",
                                                 align = "left"))),
                             tags$br(),
                             h4("Using Nikon Forestry Pro"),
                             p(a(
                                 HTML("Check out the tutorial"),
                                 href = "https://www.youtube.com/watch?v=SAUL76gEIUo",
                                 target = "_blank"
                             ))
                    )
                )

            )
        )
    )
}