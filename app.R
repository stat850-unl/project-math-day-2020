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

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  navbarPage(
    # Application title
    title = "SAAM",
    
    # Sidebar tabs
    tabPanel("Songs",
             
             tabsetPanel(
               type = "tabs",
               
               
               tabPanel(
                 "Overall Song Data",
                 
                 sidebarPanel(sliderInput(
                   "topPlayed",
                   "Hours",
                   min = 0,
                   max = 200,
                   value = c(0,70)
                 )),
                 
                 mainPanel(tableOutput("play_info"))
               ),
               
               tabPanel(
                 "Individual Song Data",
                 
                 sidebarPanel(
                   selectizeInput(
                     "song_name",
                     "Song Name",
                     choices = sort(unique(cleaned$SongName)),
                     selected = "Instant Crush (feat. Julian Casablancas)",
                     options = list(maxOptions = 10)
                   )
                 ),
                 
                 mainPanel(
                   tableOutput("song_info"),
                   tableOutput("song_big4"),
                   plotOutput("dateTimeListen"),
                   plotOutput("dateTimeListenNC")
                 )
               )
             )),
    
    
    # Second big tab
    tabPanel("Artists",
             
             tabsetPanel(
               type = "tabs",
               
               
               tabPanel(
                 "Individual Artist Data",
                 
                 sidebarPanel(
                   selectizeInput(
                     "artist_name",
                     "Artist",
                     choices = sort(unique(cleaned$Artist)),
                     selected = "Daft Punk",
                     options = list(maxOptions = 10)
                   )
                 ),
                 
                 mainPanel(tableOutput("artist_info"))
               )
             )),
    
    
    tabPanel("Albums",
             
             tabsetPanel(type = "tabs",
                         tabPanel("Album Info"))),
    
    
    tabPanel("Miscellaneous",
             
             tabsetPanel(type = "tabs",
                         
                         tabPanel(
                           "Bourgeoisie by Month"
                           
                         )))
  )
)

server <- function(input, output, session) {
  
  ###### SONGS ###########
  
  subset_plays <- reactive({
    cleaned %>%
      filter(!is.na(TimeInHours),
             TimeInHours >= input$topPlayed[1],
             TimeInHours <= input$topPlayed[2])
  })
  
  cln_subset <- reactive({
    cleaned %>%
      filter(SongName == input$song_name) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y"))
  })
  
  cln_subset_overall_info <- reactive({
    cleaned %>%
      filter(SongName == input$song_name) %>%
      filter(!is.na(TimeInHours)) %>%
      summarize(
        MaximumRank = as.integer(max(MaxRank)),
        ArtistName = unique(Artist),
        AlbumName = unique(Album)
      )
    
  })
  
  # overall
  output$play_info <- renderTable({
    subset_plays() %>%
      select(SongName, Artist, TimeInHours) %>%
      group_by(SongName) %>%
      summarise(TotalTime = max(TimeInHours)) %>%
      arrange(desc(TotalTime))
  })
  
  # individual
  output$song_info <- renderTable({
    cln_subset_overall_info() %>%
      select(MaximumRank, ArtistName, AlbumName)
  })
  
  output$song_big4 <- renderTable({
    cln_subset() %>%
      filter(!is.na(TimeInHours)) %>%
      slice_tail(n = 1) %>%
      select(RankOutOfN, ValueOutOf1, TimeInHours, TimeInPlays)
  })
  
  output$dateTimeListen <- renderPlot({
    cln_subset() %>%
      select(datenum, TimeInHours) %>%
      filter(!is.na(TimeInHours)) %>%
      ggplot(aes(x = datenum, y = TimeInHours)) +
      geom_point() +
      labs(x = "Date", y = "Cumulative Hours Listened", title = "Cumulative Hours Listened")
  })
  
  output$dateTimeListenNC <-
    renderPlot({
      #non-cumulative listening hours
      cln_subset() %>%
        select(datenum, dHours) %>%
        filter(!is.na(dHours)) %>%
        ggplot(aes(x = datenum, y = dHours)) +
        geom_line() +
        labs(x = "Date", y = "Hours Listened By Date", title = "Hours Listened by Date Added")
    })
  
  
  
  
  ########## ARTISTS #############
  
  cln_artist_subset <- reactive({
    cleaned %>%
      filter(Artist == input$artist_name) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y"),
             MaxRank = as.integer(MaxRank))
  })
  
  cln_artist_overall_info <- reactive({
    cleaned %>%
      filter(Artist == input$artist_name) %>%
      filter(!is.na(TimeInHours)) %>%
      mutate(MaxRank = as.integer(MaxRank))
    
    
  })
  
  
  
  output$artist_info <- renderTable({
    cln_artist_overall_info() %>%
      select(SongName, Album, MaxRank) %>%
      group_by(SongName) %>%
      summarise()
    
  })
  
  
  ########## ALBUMS #############
  
  
  
  
  
  
  ########## MISCELLANEOUS #############
  
}

# Run the application
shinyApp(ui = ui, server = server)
