# ToDo
# Cards eher nebeneinander
# In Card 2 tabs für die verschiedenen Bäume
# Warum gibt es nicht für jeden Datenpunkt einen Baum?

library(shiny)
# library(bslib)
library(tidyverse)
library(shinydashboard)

theme_set(theme_bw(base_size = 20))
mycss <- "
/* SliderInput color*/
.irs--shiny .irs-bar--single {
    border-radius: 8px 0 0 8px;
    background: purple;
    border-color: black;
}
.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
    color: #fff;
    text-shadow: none;
    padding: 1px 3px;
    background-color: #800080;
    border-radius: 3px;
    font-size: 11px;
    line-height: 1.333;
}

/* Sidebar distance between icon and text*/
.fa-solid, .fas {
    font-weight: 900;
    margin-right: 10px;
}

/* Sidebar input coloring
.selectize-control.single .selectize-input, .selectize-control.single .selectize-input input {
    cursor: pointer;
    background: purple;
}*/

/* options */
.option {
  color: purple;
  background: white
}

.option.selected {
  color: white;
  background: purple
}


.selectize-input.focus {
    border-color: purple;
    box-shadow: none;
}

.form-control:focus {
    border-color: purple;
}

a {
    color: purple;
}
"



body_hoehe = function(){
    return(
        fluidPage(
            box(
                title = "Nutzereingabe Baumhöhe",
                width = "100%",
                fluidRow(
                    column(4, selectInput("method_hoehe", label = "Methode",
                                          choices = list("Raten" = "Raten",
                                                         "Laser" = "Laser",
                                                         "App" = "App"))),
                    column(4, selectInput("tree_hoehe", label = "Baum ID",
                                          choices = list("B1", "B2", "B3"))),
                    column(4, numericInput("value", "Wert (m)", min = 0, max = 100,
                                           value = 0))
                ),
                
                actionButton("updateHeights", label = "Update", 
                             style = "background:purple; color:white;
                             margin-top:20px; margin-bottom:20px"),
                
                sliderInput("pdata_heights",
                            "Datenpunkte (%)",
                            min = 0,
                            max = 100,
                            value = 100,
                            width = "100%")
            ),
            
            box(
                title = "Visualisierung",
                width = "100%",
                tabsetPanel(
                    tabPanel("Baum 1", plotOutput("b1boxplot_height")),
                    tabPanel("Baum 2", plotOutput("b2boxplot_height")),
                    tabPanel("Baum 3", plotOutput("b3boxplot_height"))
            )
        )
    ))
}

body_dbh = function(){
    return(
        fluidPage(
            box(
                title = "Nutzereingabe BHD",
                width = "100%",
                fluidRow(
                    column(4, selectInput("method_dbh", label = "Methode",
                                          choices = list("Raten" = "Raten",
                                                         "Massband" = "Massband",
                                                         "Kluppe" = "Kluppe"))),
                    column(4, selectInput("tree_dbh", label = "Baum ID",
                                          choices = list("B1", "B2", "B3"))),
                    column(4, numericInput("value_dbh", "Wert (cm)", min = 0, max = 100,
                                           value = 0))
                ),
                
                actionButton("updateDBH", label = "Update", 
                             style = "background:purple; color:white;
                             margin-top:20px; margin-bottom:20px"),
                
                sliderInput("pdata_dbh",
                            "Datenpunkte (%)",
                            min = 0,
                            max = 100,
                            value = 100,
                            width = "100%")
            ),
            
            box(
                title = "Visualisierung",
                width = "100%",
                tabsetPanel(
                    tabPanel("Baum 1", plotOutput("b1boxplot_dbh")),
                    tabPanel("Baum 2", plotOutput("b2boxplot_dbh")),
                    tabPanel("Baum 3", plotOutput("b3boxplot_dbh"))
                )
            )
        ))
}

body_raten = function(){
    return(
        fluidPage(
            box(
                title = "Visualisierung",
                width = "100%",
                tabsetPanel(
                    tabPanel("Baum 1", plotOutput("b1boxplot_raten")),
                    tabPanel("Baum 2", plotOutput("b2boxplot_raten")),
                    tabPanel("Baum 3", plotOutput("b3boxplot_raten"))
                )
            )
        )
    )
}

body_help = function(){
    return(
        fluidPage(
            box(
                title = "Was ist ein Boxplot?",
                width = "100%",
                fluidRow(column(12, img(src='boxplot_wikipedia.png', align = "left"))),
                p("Quelle: https://de.wikipedia.org/wiki/Box-Plot")
                
            )
        )
    )
}

# Define UI for application that draws a histogram
ui <- dashboardPage(
    skin = "black",
    dashboardHeader(title = "Waldinventur"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Höhe", tabName = "hoehe", icon = icon("text-height")),
            menuItem("Durchmesser", tabName = "dbh", icon = icon("text-width")),
            menuItem("Help", tabName = "help", icon = icon("home"))
        )
    ),
    dashboardBody(
        tags$style(mycss),
        
        tabItems(
            tabItem(tabName = "hoehe",
                    body_hoehe()),
            tabItem(tabName = "dbh",
                    body_dbh()),
            tabItem(tabName = "help",
                    body_help())
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    values <- reactiveValues(heights = read.csv("height_data.csv"),
                             dbh = read.csv("dbh_data.csv"))
    
    # heights ----
    heights = reactive({
        # Load df
        heights = read.csv("height_data.csv")

        # Add new data
        new_heights = data.frame(method = input$method_hoehe,
                                 wert = input$value,
                                 baum = input$tree_hoehe)
        heights = bind_rows(heights, new_heights)

        # Save df
        tryCatch(
            {        
                write.csv(heights, "height_data.csv",
                          row.names = F) 
                showNotification("Data was saved successfully.",
                                 type = "message")
            },
            error = function(e){
                showNotification("Error: Data was not saved. Please check if the csv file is closed.",
                                 type = "error")
                },
            warning = function(e){
                showNotification("Error: Data was not saved. Please check if the csv file is closed.",
                                 type = "error")
            }
        )
       
        return(heights)
        
    })
    
    observeEvent(input$updateHeights, {
        # Update reactive values
        values$heights = heights()
    })
    
    get.heights.boxplot = function(baumID){
        # generate bins based on input$bins from ui.R
        heights = values$heights %>% 
            filter(baum == baumID)
        pdata = input$pdata_heights / 100
        
        neuster_eintrag = heights %>% 
            group_by(method) %>% 
            mutate(n = 1:n()) %>% 
            filter(n == max(n))

        return(
            heights %>% 
                group_by(method) %>% 
                sample_frac(., pdata) %>% 
                ggplot(., aes(x = method, y = wert)) +
                geom_boxplot() +
                geom_jitter(size = 2) +
                geom_point(neuster_eintrag,
                        mapping = aes(x = method, y = wert),
                        col = "purple", size = 3, shape = 8) +
                labs(x = "Methode zur Höhenschätzung",
                     y = "Höhe (m)")
        )
    }
    
    output$b1boxplot_height <- renderPlot({
        get.heights.boxplot(baumID = "B1")
    })
    
    output$b2boxplot_height <- renderPlot({
        get.heights.boxplot(baumID = "B2")
    })
    
    
    output$b3boxplot_height <- renderPlot({
        get.heights.boxplot(baumID = "B3")
    })
    
    
    # dbh ----
    dbh = reactive({
        # Load df
        dbh = read.csv("dbh_data.csv")
        
        # Add new data
        new_dbh = data.frame(method = input$method_hoehe,
                                 wert = input$value,
                                 baum = input$tree_hoehe)
        dbh = bind_rows(dbh, new_dbh)
        
        # Save df
        tryCatch(
            {        
                write.csv(dbh, "dbh_data.csv",
                          row.names = F) 
                showNotification("Data was saved successfully.",
                                 type = "message")
            },
            error = function(e){
                showNotification("Error: Data was not saved. Please check if the csv file is closed.",
                                 type = "error")
            },
            warning = function(e){
                showNotification("Error: Data was not saved. Please check if the csv file is closed.",
                                 type = "error")
            }
        )
        
        return(dbh)
        
    })
    
    observeEvent(input$updateHeights, {
        # Update reactive values
        values$dbh = dbh()
    })
    
    get.dbh.boxplot = function(baumID){
        # generate bins based on input$bins from ui.R
        dbh = values$dbh %>% 
            filter(baum == baumID)
        pdata = input$pdata_dbh / 100
        
        neuster_eintrag = dbh %>% 
            group_by(method) %>% 
            mutate(n = 1:n()) %>% 
            filter(n == max(n))
        
        return(
            dbh %>% 
                group_by(method) %>% 
                sample_frac(., pdata) %>% 
                ggplot(., aes(x = method, y = wert)) +
                geom_boxplot() +
                geom_jitter(size = 2) +
                geom_point(neuster_eintrag,
                           mapping = aes(x = method, y = wert),
                           col = "purple", size = 3, shape = 8) +
                labs(x = "Methode zur Durchmesserschätzung",
                     y = "Höhe (m)")
        )
    }
    
    output$b1boxplot_dbh <- renderPlot({
        get.dbh.boxplot(baumID = "B1")
    })
    
    output$b2boxplot_dbh <- renderPlot({
        get.dbh.boxplot(baumID = "B2")
    })
    
    
    output$b3boxplot_dbh <- renderPlot({
        get.dbh.boxplot(baumID = "B3")
    })
    
    
    # Raten ----
    get.rate.plot = function(){
        # generate bins based on input$bins from ui.R
        dbh = values$dbh %>% mutate(typ = "BHD")
        height = values$heights %>% mutate(typ = "Höhe")
        df = bind_rows(dbh, height) %>% 
            filter(method = "Raten")
        
        pdata = input$pdata_dbh / 100
        
        return(
            dbh %>% 
                ggplot(., aes(x = method, y = wert)) +
                geom_boxplot() +
                geom_jitter() +
                geom_point(neuster_eintrag,
                           mapping = aes(x = method, y = wert),
                           col = "purple", size = 1.5) +
                labs(x = "Methode zur Höhenschätzung",
                     y = "Höhe (m)")
        )
    }
}

# Run the application 
shinyApp(ui = ui, server = server)
