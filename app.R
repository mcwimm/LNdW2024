#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(tidyverse)
theme_set(theme_bw(base_size = 20))

# Define UI for application that draws a histogram
ui <- page_sidebar(
    #sidebar = sidebar(),
    
    # Application title
    titlePanel("Tree heights: guess or measure"),
    

    # Sidebar with a slider input for number of bins 
    card(
        card_header("Nutzereingabe"),
        
        fluidRow(
            column(4, selectInput("method", label = "Methode",
                        choices = list("Raten" = "Raten",
                                       "Laser" = "Laser",
                                       "App" = "App"))),
            column(4, selectInput("tree", label = "Baum ID",
                        choices = list("B1", "B2"))),
            column(4, numericInput("value", "Wert (m)", min = 0, max = 100,
                         value = 0))
        ),
        
        actionButton("updateHeights", label = "Update"),
        
        sliderInput("pdata",
                    "Datenpunkte (%)",
                    min = 1,
                    max = 100,
                    value = 100,
                    width = "100%")
    ),
    card(
        card_header("Baum 1"),
        plotOutput("b1boxplot")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    values <- reactiveValues(heights = read.csv("height_data.csv"))
    
    
    heights = reactive({
        # Load df
        heights = read.csv("height_data.csv")
        print(nrow(heights))
        # Add new data
        new_heights = data.frame(method = input$method,
                                 wert = input$value,
                                 baum = input$tree)
        heights = bind_rows(heights, new_heights)
        print(nrow(heights))
        
        # Save df
        write.csv(dd, "height_data.csv",
                  row.names = F)        
        return(heights)
        
    })
    
    observeEvent(input$updateHeights, {
        # Update reactive values
        values$heights = heights()
    })
    
    
    output$b1boxplot <- renderPlot({
        # generate bins based on input$bins from ui.R
        heights = values$heights
        pdata = input$pdata / 100
        
        heights %>% 
            filter(baum == "B1") %>% 
            group_by(method) %>% 
            sample_frac(., pdata) %>% 
            ggplot(., aes(x = method, y = wert)) +
            geom_boxplot() +
            geom_jitter() +
            labs(x = "Methode zur Höhenschätzung",
                 y = "Höhe (m)")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
