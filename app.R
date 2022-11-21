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
  
  navbarPage(
  # Application title
    title = "SAAM",
  
  # Sidebar tabs
  tabPanel(
    "Songs",
    
    tabsetPanel(
      
      type = "tabs",
      
      
      tabPanel(
        
        "Individual Song Data",
        
    sidebarPanel(
      selectizeInput("song_name", "Song Name", choices = sort(unique(cleaned$SongName)), selected = "Instant Crush (feat. Julian Casablancas)", options = list(maxOptions = 10))
    ),
    
    mainPanel(
      tableOutput("song_info"),
      tableOutput("song_big4"),
      plotOutput("dateTimeListen"),
      plotOutput("dateTimeListenNC")
    )
  )
      )
    ),
  
  
  # Second big tab
  tabPanel(
    "Artists",
    
    tabsetPanel(
      
      type = "tabs",
      
      
      tabPanel(
        
        "Individual Artist Data",
        
        sidebarPanel(
          selectizeInput("artist_name", "Artist", choices = sort(unique(cleaned$Artist)), selected = "Daft Punk", options = list(maxOptions = 10))
        ),
        
        mainPanel(
          tableOutput("artist_info")
        )
      )
    )
  )
)
)

server <- function(input, output, session) {
  
  
  ## SONGS
  cln_subset <- reactive({
    cleaned %>%
      filter(SongName == input$song_name) %>%
      mutate(datenum=as.Date(dates, format = "%B %d %Y"))
  })
  
  cln_subset_overall_info <- reactive({
    cleaned %>%
      filter(SongName == input$song_name) %>%
      filter(!is.na(TimeInHours)) %>%
      summarize(MaximumRank = max(MaxRank), TotalHoursListened = max(TimeInHours), TotalPlays = max(TimeInPlays), ArtistName = unique(Artist), AlbumName = unique(Album))
 
  })
  
  cln_big_4_song <- reactive({
    cleaned %>%
      filter(SongName == input$song_name) %>%
      filter(!is.na(TimeInHours)) %>%
      slice_tail(n=1)
  })
  
  

  output$song_info <- renderTable({
    cln_subset_overall_info() %>%
     select(MaximumRank, TotalHoursListened, TotalPlays, ArtistName, AlbumName)
  })
  
  output$song_big4 <- renderTable({
    cln_big_4_song() %>%
      select(RankOutOfN, ValueOutOf1, TimeInHours,TimeInPlays)
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
  
  
  
  ### ARTISTS
  
  cln_artist_subset <- reactive({
    cleaned %>%
      filter(Artist == input$artist_name) %>%
      mutate(datenum=as.Date(dates, format = "%B %d %Y"),
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
      group_by(SongName, Album, MaxRank) %>%
      summarise()

  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
