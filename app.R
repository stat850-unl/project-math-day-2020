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
library(patchwork)


cleaned <- readr::read_csv("data/cleaned.csv")
cleaned_artist <- readr::read_csv("data/cleaned_artist.csv")

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
                   "topHrs",
                   "Hours",
                   min = 0,
                   max = 200,
                   value = c(0, 70)
                 ), 
                 sliderInput(
                   "topPlayed",
                   "Plays",
                   min = 0,
                   max = 2500,
                   value = c(0, 100)
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
                 "Overall Artist Data",
                 
                 sidebarPanel(
                   sliderInput(
                     "topArtistHrs",
                     "Hours",
                     min = 0,
                     max = 5000,
                     value = c(0, 100)
                   ), sliderInput(
                     "topArtistPlayed",
                     "Plays",
                     min = 0,
                     max = 30000,
                     value = c(0, 100)
                   )
                   
                 ),
                 mainPanel(tableOutput("artist_play_info"))
                 
               ),
               
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
                 
                 mainPanel(tableOutput("overall_artist_info"),
                          tableOutput("artist_song_info"),
                           plotOutput("artist_big4"),
                           plotOutput("dateTimeListenNCA"))
               )
             )),
    
    
    
    tabPanel( "Albums",
          tabsetPanel(       
            type = "tabs",
                tabPanel(
                  "Album Info",
                  sidebarPanel(
                    selectizeInput(
                      "album_name",
                      "Album",
                      choices = sort(unique(cleaned$Album)),
                      selected = "21st Century Breakdown",
                      options = list(maxOptions = 10)
                    )
                  ),
                  
                  mainPanel(tableOutput("album_info"))
                ))
  ),
  
  
  tabPanel("Miscellaneous",
           
           tabsetPanel(type = "tabs",
                       
                       tabPanel(
                         "Bourgeoisie by Month"
                         
                       )))
  
))

server <- function(input, output, session) {
  ###### SONGS ###########
  
  subset_plays <- reactive({
    cleaned %>%
      filter(
        !is.na(TimeInHours),
        TimeInHours >= input$topHrs[1],
        TimeInHours <= input$topHrs[2],
        TimeInPlays >= input$topPlayed[1],
        TimeInPlays <= input$topPlayed[2]
      )
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
      select(SongName, Artist, TimeInHours, TimeInPlays) %>%
      group_by(SongName) %>%
      summarise(TotalTime = max(TimeInHours), TotalPlays = max(TimeInPlays)) %>%
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
    
    p1 <- cln_subset() %>%
      select(datenum, TimeInHours) %>%
      filter(!is.na(TimeInHours)) %>%
      ggplot(aes(x = datenum, y = TimeInHours)) +
      geom_line() +
      labs(x = "Date", y = "Cumulative Hours Listened", title = "Cumulative Hours Listened Over Time")
    
    
    p2 <- cln_subset() %>%
      select(datenum, TimeInPlays) %>%
      filter(!is.na(TimeInPlays)) %>%
      ggplot(aes(x = datenum, y = TimeInPlays)) +
      geom_line() +
      labs(x = "Date", y = "Cumulative Plays", title = "Cumulative Plays of the Song Over Time")
    
    p3 <-  cln_subset() %>%
      select(datenum, RankOutOfN) %>%
      filter(!is.na(RankOutOfN)) %>%
      ggplot(aes(x = datenum, y = RankOutOfN)) +
      geom_line() +
      labs(x = "Date", y = "Overall Rank", title = "Overall Rank of the Song Over Time")
      
    p4 <-  cln_subset() %>%
      select(datenum, ValueOutOf1) %>%
      filter(!is.na(ValueOutOf1)) %>%
      ggplot(aes(x = datenum, y = ValueOutOf1)) +
      geom_line() +
      labs(x = "Date", y = "Song Value", title = "Overall Value of the Song Over Time")
    
    
    p3 + p4 + p1 + p2
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
  
  subset_artist_plays <- reactive({
    
    cleaned_artist %>%
      filter(
        !is.na(TimeInHours),
        TimeInHours >= input$topArtistHrs[1],
        TimeInHours <= input$topArtistHrs[2],
        TimeInPlays >= input$topArtistPlayed[1],
        TimeInPlays <= input$topArtistPlayed[2]
      )
      
  })
  
  cln_artist_by_song_info <- reactive({
    cleaned %>%
      filter(Artist == input$artist_name,
             !is.na(TimeInHours)) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y"),
             MaxRank = as.integer(MaxRank))
  })
  
  cln_artist_subset <- reactive({
    cleaned_artist %>%
      filter(Artist == input$artist_name,
             !is.na(TimeInHours)) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y"),
             MaxRank = as.integer(MaxRank)) 
  })
  
  cln_artist_overall_info <- reactive({
    cleaned_artist %>%
      filter(Artist == input$artist_name) %>%
      filter(!is.na(TimeInHours)) %>%
      summarize(
        MaximumRank = as.integer(max(MaxRank)),
        SongCount = as.integer(max(SongCount))
      )
  })
  
  
  
  output$artist_play_info <- renderTable({
    subset_artist_plays() %>%
      select(Artist, TimeInHours, TimeInPlays) %>%
      group_by(Artist) %>%
      summarise(TotalTime = max(TimeInHours), TotalPlays = max(TimeInPlays)) %>%
      arrange(desc(TotalTime))
   
  })
  
  output$artist_song_info <- renderTable({
    cln_artist_by_song_info() %>%
      select(SongName, Album, MaxRank) %>%
      group_by(SongName,Album, MaxRank) %>%
      summarise()
    
  })
  
  output$overall_artist_info <- renderTable({
    cln_artist_overall_info() %>%
      select(MaximumRank, SongCount)
  })
  
  
  output$artist_big4 <- renderPlot({
    p1a <- cln_artist_subset() %>%
      select(datenum, TimeInHours) %>%
      filter(!is.na(TimeInHours)) %>%
      ggplot(aes(x = datenum, y = TimeInHours)) +
      geom_line() +
      labs(x = "Date", y = "Cumulative Hours Listened", title = "Cumulative Hours Listened Over Time")
    
    
    p2a <- cln_artist_subset() %>%
      select(datenum, TimeInPlays) %>%
      filter(!is.na(TimeInPlays)) %>%
      ggplot(aes(x = datenum, y = TimeInPlays)) +
      geom_line() +
      labs(x = "Date", y = "Cumulative Plays", title = "Cumulative Plays of the Artist's Songs Over Time")
    
    p3a <-  cln_artist_subset() %>%
      select(datenum, RankOutOfN) %>%
      filter(!is.na(RankOutOfN)) %>%
      ggplot(aes(x = datenum, y = RankOutOfN)) +
      geom_line() +
      labs(x = "Date", y = "Overall Rank", title = "Overall Rank of the Artist Over Time")
    
    p4a <-  cln_artist_subset() %>%
      select(datenum, ValueOutOf1) %>%
      filter(!is.na(ValueOutOf1)) %>%
      ggplot(aes(x = datenum, y = ValueOutOf1)) +
      geom_line() +
      labs(x = "Date", y = "Artist Value", title = "Overall Value of the Artist Over Time")
    
    
    p3a + p4a + p1a + p2a
  })
  
  
  output$dateTimeListenNCA <-
    renderPlot({
      #non-cumulative listening hours
      cln_artist_subset() %>%
        select(datenum, dHours) %>%
        filter(!is.na(dHours)) %>%
        ggplot(aes(x = datenum, y = dHours)) +
        geom_line() +
        labs(x = "Date", y = "Hours Listened By Date", title = "Hours Listened by Date")
    })

  
  
  ########## ALBUMS #############
  
  
  cln_album_subset <- reactive({
    cleaned %>%
      filter(Album == input$album_name,!is.na(TimeInHours)) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y"),
             MaxRank = as.integer(MaxRank))
  })
  
  
  
  
  output$album_info <- renderTable({
    cln_album_subset() %>%
      select(SongName, Artist, MaxRank) %>%
      group_by(SongName, Artist, MaxRank) %>%
      summarise()
    
  })
  
  
  
  ########## MISCELLANEOUS #############
  
}

# Run the application
shinyApp(ui = ui, server = server)
