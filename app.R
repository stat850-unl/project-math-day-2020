#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)


cleaned <- readr::read_csv("~/Downloads/cleaned.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Song Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("song_name", "Song Name", choices = sort(unique(cleaned$SongName)), selected = "Hey, Soul Sister")
    ),
    
    mainPanel(
      #plotOutput("dist")
      tableOutput("dist")
    )
  )
)

server <- function(input, output) {
  
  cln_subset <- reactive({
    cleaned %>%
      filter(SongName == input$song_name) %>%
      mutate(datenum=as.Date(dates, format = "%B %d %Y"))
  })
  
 # output$dist <- renderPlot({
 #   cln_subset() %>%
  #    select(dates, TimeInHours, TimeInPlays) %>%
  #    ggplot(aes(x = dates, y = TimeInHours)) +
  #    geom_point()
    
 # })
  output$dist <- renderTable({
    cln_subset() %>%
      select(dates, TimeInHours, TimeInPlays)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
