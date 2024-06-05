source("setup.R")


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
    print("Working directory")
    print(getwd())
    
    get.csv = function(key){
        fn = paste("data/", key, "_data.csv", sep = "")
        if (file.exists(fn)){
            df = read.csv(fn)
        } else {
            df <- read.table(text = "",
                                  colClasses = c("character", "numeric", "character", "numeric"),
                                  col.names = c("method", "wert", "baum", "zeit"))
        }
        return(df)
    }
    values <- reactiveValues(heights = get.csv("height"),
                             dbh = get.csv("dbh"))
    
    get.sys.time = function(){
        tt = format(Sys.time(), "%H %M %S")
        tt = strsplit(tt, " ")[[1]]
        return(round(as.numeric(tt[1]) + as.numeric(tt[2])/60 + as.numeric(tt[2])/60/60, 3))
    }
    
    # heights ----
    heights = reactive({
        # Load df
        heights = values$heights

        if (input$value_hoehe <= 0){
            showNotification("Error: Eingabewert <= 0.",
                             type = "error")
        } else {
            # Add new data
            new_heights = data.frame(method = input$method_hoehe,
                                     wert = input$value_hoehe,
                                     baum = input$tree_hoehe, 
                                     zeit = get.sys.time())
            heights = bind_rows(heights, new_heights)

            # Save df
            tryCatch(
                {        
                    write.csv(heights, "data/height_data.csv",
                              row.names = F) 
                    showNotification("Data was saved successfully.",
                                     type = "message")
                },
                error = function(e){
                    print(e)
                    print("WARN")
                    showNotification("Error: Data was not saved. Please check if the csv file is closed.",
                                     type = "error")
                },
                warning = function(e){
                    print(e)
                    print("ERR")
                    showNotification("Error: Data was not saved. Please check if the csv file is closed.",
                                     type = "error")
                }
            )
        }
        return(heights)
    })
    
    observeEvent(input$updateHeights, {
        # Update reactive values
        values$heights = heights() %>% 
            mutate(method = factor(method,
                                   levels = c("Laser", "App",
                                              "Raten | Guess")))
    })
    
    get.empty.plot = function(){
        return(
            data.frame(message = "Keine Werte vorhanden. | No values available.") %>% 
                ggplot(., aes(10, 10)) +
                geom_text(aes(label = message), size = 12) +
                theme_void()
        )
    }
    
    get.heights.boxplot = function(baumID){
        if (nrow(values$heights) == 0){
            return(get.empty.plot())
        } else {
            # generate bins based on input$bins from ui.R
            heights = values$heights %>% 
                filter(baum == baumID) 
            if (nrow(heights) == 0){
                return(get.empty.plot())
            } else {
                pdata = input$pdata_heights / 100
                
                heights = heights %>% 
                    mutate(n = 1:n(),
                           col = ifelse(n == max(n), "neu", 'alt'))
                return(
                    heights %>% 
                        group_by(method) %>% 
                        sample_frac(., pdata) %>% 
                        ggplot(., aes(x = method, y = wert)) +
                        geom_boxplot(col = "grey53") +
                        geom_point(position=position_jitterdodge(),
                                   aes(shape = method, col = col, size = col),
                                   show.legend = F) +
                        scale_size_manual(values = c(3, 4)) + 
                        scale_color_manual(values = c("black", "purple")) +
                        labs(x = "Methode zur Höhenschätzung | \nMethod to estimate height",
                             y = "Höhe | Height (m)")
                )
            }   
        }
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
    
    label_facet <- function(original_var){
        lev <- levels(as.factor(original_var))
        lab <- paste0("Baum | Tree", ": ", lev)
        names(lab) <- lev
        return(lab)  
    }
    
    get.timeseriesplot = function(key){
        if (key == "heights"){
            df = values$heights
            ylab = "Höhe | Height (m)"
            pdata = input$pdata_heights / 100
        } else {
            df = values$dbh
            ylab = "BHD | DBH (m)"
            pdata = input$pdata_dbh / 100
        }
        if (nrow(df) == 0){
            return(get.empty.plot())
        } else {
            df %>% 
                group_by(method) %>% 
                sample_frac(., pdata) %>% 
                ggplot(., aes(x = zeit, y = wert, col = method)) +
                geom_point() +
                geom_line(alpha = 0.75) +
                scale_color_viridis_d(end = 0.8) +
                facet_wrap(~baum, 
                           labeller = labeller(baum = label_facet(df$baum))) +
                labs(x = "Uhrzeit | Time",
                     y = ylab,
                     col = "Methode | Method") +
                theme(legend.position = "bottom")
            
        }
    }
    
    output$timeseries_height <- renderPlot({
        get.timeseriesplot(key = "heights")
    })
    
    
    # dbh ----
    dbh = reactive({
        # Load df
        dbh = values$dbh
        if (input$value_dbh <= 0){
            showNotification("Error: Eingabewert <= 0.",
                             type = "error")
        } else {
            # Add new data
            new_dbh = data.frame(method = input$method_dbh,
                                 wert = input$value_dbh,
                                 baum = input$tree_dbh, 
                                 zeit = get.sys.time())
            dbh = bind_rows(dbh, new_dbh)
            
            # Save df
            tryCatch(
                {        
                    write.csv(dbh, "data/dbh_data.csv",
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
        }
        
        return(dbh)
        
    })
    
    observeEvent(input$updateDBH, {
        # Update reactive values
        values$dbh = dbh() %>% 
            mutate(method = factor(method,
                                   levels = c("Massband | Tapeline",
                                              "Kluppe | Caliper", 
                                              "Raten | Guess")))
    })
    
    
    get.dbh.boxplot = function(baumID){
        if (nrow(values$dbh) == 0){
            return(get.empty.plot())
        } else {
            # generate bins based on input$bins from ui.R
            dbh = values$dbh %>% 
                filter(baum == baumID) 
            if (nrow(dbh) == 0){
                return(get.empty.plot())
            } else {
                pdata = input$pdata_dbh / 100
                
                dbh = dbh %>% 
                    mutate(n = 1:n(),
                           col = ifelse(n == max(n), "neu", 'alt'))
                return(
                    dbh %>% 
                        group_by(method) %>% 
                        sample_frac(., pdata) %>% 
                        ggplot(., aes(x = method, y = wert)) +
                        geom_boxplot(col = "grey53") +
                        geom_point(position=position_jitterdodge(),
                                   aes(shape = method, col = col, size = col),
                                   show.legend = F) +
                        scale_size_manual(values = c(3, 4)) + 
                        scale_color_manual(values = c("black", "purple")) +
                        labs(x = "Methode zur BHD-Schätzung | \nMethod to estimate dbh",
                             y = "BHD | DBH (m)")
                )
            }   
        }
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
    
    
    output$timeseries_dbh <- renderPlot({
        get.timeseriesplot(key = "dbh")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
