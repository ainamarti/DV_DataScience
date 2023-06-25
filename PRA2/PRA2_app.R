#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(shinythemes)
library(tidygeocoder)
library(plotly)

setwd("/Users/Aina/Library/CloudStorage/GoogleDrive-amartiaran@uoc.edu/La meva unitat/04-semestre3/data_visualization/PRA2")

world_coordinates<-read.csv( "world_coordinates_universitiesMeans.csv")
world_coordinates<-as.data.table(world_coordinates)
universities<-read.csv("universities_coordinates.csv")
universities<-as.data.table(universities)

worldMaps_mean <- function(world_data){
  
  # Function for setting the aesthetics of the plot
  my_theme <- function () { 
    theme_bw() + theme(axis.text = element_text(size = 14),
                       axis.title = element_text(size = 14),
                       strip.text = element_text(size = 14),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       legend.position = "bottom",
                       panel.border = element_blank(), 
                       strip.background = element_rect(fill = 'white', colour = 'white'))
  }
  
  # Specify the plot for the world map
  library(RColorBrewer)
  library(ggiraph)
  g <- ggplot() + 
    geom_polygon(data = world_data, color = 'gray70', size = 0.1,
                             aes(x = long, y = lat, fill = mean_rank, group=group)) + 
    scale_fill_gradient2(low="blue", mid = "white", high="red", midpoint = 750, na.value = 'white') + 
    scale_y_continuous(limits = c(-60, 90), breaks = c()) + 
    scale_x_continuous(breaks = c()) + 
    #labs(fill = "mean rank",, title = NULL, x = NULL, y = NULL) + 
    my_theme() + xlab("") + ylab("")
  
  return(g)
}

p<-worldMaps_mean(world_coordinates)

worldMaps_max <- function(world_data){
  
  # Function for setting the aesthetics of the plot
  my_theme <- function () { 
    theme_bw() + theme(axis.text = element_text(size = 14),
                       axis.title = element_text(size = 14),
                       strip.text = element_text(size = 14),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       legend.position = "bottom",
                       panel.border = element_blank(), 
                       strip.background = element_rect(fill = 'white', colour = 'white'))
  }
  
  # Specify the plot for the world map
  library(RColorBrewer)
  library(ggiraph)
  g <- ggplot() + 
    geom_polygon_interactive(data = world_data, color = 'gray70', size = 0.1,
                             aes(x = long, y = lat, fill = max_rank, group=group)) + 
    scale_fill_gradient2(low="blue", mid = "white", high="red", midpoint = 500, na.value = 'white') + 
    scale_y_continuous(limits = c(-60, 90), breaks = c()) + 
    scale_x_continuous(breaks = c()) + 
    #labs(fill = "mean rank",, title = NULL, x = NULL, y = NULL) + 
    my_theme() + xlab("") + ylab("")
  
  return(g)
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("united"),
    # Application title
    titlePanel(h1("World University Rankings 2023",
                  style='background-color:#A4DBB5;
                     padding-left: 200px')),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(tags$style(".well {background-color:#CAE4DD;}"),
          width = 12,
            sliderInput("best",
                        "Top best universities:",
                        min = 1,
                        max = 500,
                        value = 5),
            selectInput("country", "Select a country",
                        c("All", sort(unique(universities$location))))
        ),
        # Show a plot of the generated distribution
        mainPanel (width = 12,
                  dataTableOutput("table_best"),
                  plotOutput("plot_map")
        
    )
))



server <- function(input, output) {
    tableData <- reactive({country<-input$country
                        best<-input$best
                        country_list=unique(universities$location)
                        if(input$country=="All"){country_list=unique(universities$location)}
                         universities_=universities[order(Rank) & location %in% country_list][1:best]
                         })
    
  
    #observeEvent(event_trigger(), {
    #tableData$country_list<-c(input$country)
    #if(input$country=="All"){tableData$country_list<-unique(universities$location)
    #tableData$universities_<-universities[order(Rank) & location %in% tableData$country_list][1:input$best]
    #tableData$p_<-p+geom_point(data=tableData$universities_, aes(x=longitude, y=latitude), shape=21, fill="yellow", alpha=0.8)
    #}
    #})
  
    
  
    output$table_best <- renderDataTable({
      if(input$country=="All"){country_list=unique(universities$location)}
      else{country_list<-c(input$country)}
      universities[order(Rank) & location %in% country_list][1:input$best, c(-1, -12, -13)]},
        options = list(scrollX = TRUE, pageLength=5))
    
    output$plot_map <- renderPlot({
      if(input$country=="All"){country_list=unique(universities$location)}
      else{country_list<-c(input$country)}
      universities_=universities[order(Rank) & location %in% country_list][1:input$best]
      p+geom_point(data=universities_, aes(x=longitude, y=latitude, label=institution), shape=21, fill="yellow", alpha=0.8)
    }, width=900, height=500)   
    
}

# Run the application 
shinyApp(ui = ui, server = server)
