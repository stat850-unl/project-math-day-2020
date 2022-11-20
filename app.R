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
      selectInput("song_name", "Song Name", choices = sort(unique(cleaned$SongName)), selected = "Instant Crush (feat. Julian Casablancas)")
    ),
    
    mainPanel(
      #plotOutput("dist")
      tableOutput("dist"),
      plotOutput("dateTimeListen"),
      plotOutput("dateTimeListenNC")
    )
  )
)

server <- function(input, output) {
  
  cln_subset <- reactive({
    cleaned %>%
      filter(SongName == input$song_name) %>%
      mutate(datenum=as.Date(dates, format = "%B %d %Y"))
  })
  

  output$dist <- renderTable({
    cln_subset() %>%
      select(dates, TimeInHours, TimeInPlays) %>%
      filter(!is.na(TimeInHours))
  })
  
  output$dateTimeListen <- renderPlot({
     cln_subset() %>%
     select(datenum, TimeInHours) %>%
      filter(!is.na(TimeInHours)) %>%
      ggplot(aes(x = datenum, y = TimeInHours)) +
      geom_point() +
      labs(x = "Date", y = "Cumulative Hours Listened")
    })
  
  output$dateTimeListenNC <- renderPlot({ #non-cumulative listening hours
    cln_subset() %>%
      select(datenum, dHours) %>%
      filter(!is.na(dHours)) %>%
      ggplot(aes(x = datenum, y = dHours)) +
      geom_line() +
      labs(x = "Date", y = "Hours Listened By Date")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
