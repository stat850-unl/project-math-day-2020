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
cleaned_album <- readr::read_csv("data/cleaned_album.csv")
db_misc <-read_csv("data/db_misc.csv", col_types = minittypes, na = c("-", "NA"), guess_max = 1800)


# Define UI for application
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  navbarPage(
    # Application title
    title = "SAAM",
    
    # Sidebar tabs
    tabPanel(
      "Songs",
      
      tabsetPanel(
        type = "tabs",
        
        
        tabPanel(
          "Overall Song Data",
          
          sidebarPanel(
            sliderInput(
              "topHrs",
              "Hours",
              min = 0,
              max = 200,
              value = c(0, 200)
            ),
            sliderInput(
              "topPlayed",
              "Plays",
              min = 0,
              max = 2500,
              value = c(0, 2500)
            ),
            dateRangeInput(
              "song_date_range",
              "Show Songs Added Between:",
              start = "2013-08-01",
              end = "2022-09-01",
              min = "2013-08-01",
              max = NULL,
              format = "yyyy-mm-dd",
              startview = "month",
              weekstart = 0,
              language = "en",
              separator = " to "
            ),
            selectInput(
              "showNumSongs",
              "Show Top __",
              choices = c(10, 25, 50, 100, "All"),
              selected = "All"
            ),
            selectInput(
              "secondarySortingFilter",
              "Sort By:",
              choices = c(
                "-",
                "Most Time Improved",
                "Most Play Improved",
                "Most Rank Improved",
                "Most Value Improved"
              ),
              selected = "-"
            )
            ## nothing has been done with the secondary sorting filter yet!
          ),
          
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
        ),
        
        tabPanel(
          "By Month",
          
          sidebarPanel(
            selectInput("month_choice_songs",
                        "Choose Month:",
                        choices = sort(unique(
                          as.Date(cleaned$dates, format = "%B %d %Y")
                        ))),
            
            selectInput(
              "showNumSongsMonth",
              "Show Top __",
              choices = c(10, 25, 50, 75, 100, "All"),
              selected = "All"
            ),
            sliderInput(
              "topHrsMonth",
              "Hours",
              min = 0,
              max = 200,
              value = c(0, 200)
            ),
            sliderInput(
              "topPlayedMonth",
              "Plays",
              min = 0,
              max = 2500,
              value = c(0, 2500)
            ),
            selectInput(
              "secondarySortingFilterMonth",
              "Sort By:",
              choices = c(
                "-",
                "Most Time Improved",
                "Most Play Improved",
                "Most Rank Improved",
                "Most Value Improved"
              ),
              selected = "-"
            )
            ## nothing has been done with the secondary sorting filter yet!
            
          ),
          mainPanel(tableOutput("monthData"))
        )
      )
    ),
    
    
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
                     value = c(0, 500)
                   ),
                   sliderInput(
                     "topArtistPlayed",
                     "Plays",
                     min = 0,
                     max = 30000,
                     value = c(0, 500)
                   ),
                   dateRangeInput(
                     "artist_date_range",
                     "Show Artists Added Between:",
                     start = "2013-08-01",
                     end = "2022-09-01",
                     min = "2013-08-01",
                     max = NULL,
                     format = "yyyy-mm-dd",
                     startview = "month",
                     weekstart = 0,
                     language = "en",
                     separator = " to "
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
                 
                 mainPanel(
                   tableOutput("overall_artist_info"),
                   tableOutput("artist_song_info"),
                   plotOutput("artist_big4"),
                   plotOutput("dateTimeListenNCA")
                 )
               )
             )),
    
    
    # third big tab
    tabPanel("Albums",
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
                 
                 mainPanel(
                   tableOutput("overall_album_info"),
                   tableOutput("album_info"),
                 plotOutput("album_big4"),
                 plotOutput("dateTimeListenNCAl")
               )
             ))),
    
    
    tabPanel("Miscellaneous",
             
             tabsetPanel(type = "tabs",
                         
                         tabPanel(
                           "Bourgeoisie by Month",
                           sidebarPanel(
                             selectizeInput(
                               "month_name_bourgeoisie",
                               "Month",
                               choices = unique(colnames(db_misc)[5:ncol(db_misc)]),
                               selected = "January 2016",
                               options = list(maxOptions = 10)
                             )
                             
                           ),
                           mainPanel(
                             tableOutput("month_stats_bourgeoisie"),
                             tableOutput("month_songs_bourgeoisie")
                           )
                           
                         ))
    )    
  )
)




server <- function(input, output, session) {
  ###### SONGS ###########
  
  subset_plays_by_month <- reactive({
    cleaned %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      filter(
        !is.na(TimeInHours),
        TimeInHours >= input$topHrsMonth[1],
        TimeInHours <= input$topHrsMonth[2],
        TimeInPlays >= input$topPlayedMonth[1],
        TimeInPlays <= input$topPlayedMonth[2],
        datenum == as.Date(input$month_choice_songs)
      )
  })
  
  subset_plays <- reactive({
    cleaned %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      group_by(SongID) %>%
      filter(!is.na(TimeInHours)) %>%
      mutate(DateAdded = min(datenum)) %>%
      ungroup() %>%
      filter(
        !is.na(TimeInHours),
        TimeInHours >= input$topHrs[1],
        TimeInHours <= input$topHrs[2],
        TimeInPlays >= input$topPlayed[1],
        TimeInPlays <= input$topPlayed[2],
        DateAdded >= as.Date(input$song_date_range[1]),
        DateAdded <= as.Date(input$song_date_range[2])
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
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      filter(!is.na(TimeInHours)) %>%
      summarize(
        MaximumRank = as.integer(max(MaxRank)),
        ArtistName = unique(Artist),
        AlbumName = unique(Album),
        DateAdded = dplyr::first(dates)
      )
    
  })
  
  
  # overall
  output$play_info <- renderTable({
    if (input$showNumSongs == "All") {
      subset_plays() %>%
        select(SongName, Artist, TimeInHours, TimeInPlays) %>%
        group_by(SongName) %>%
        summarise(TotalTime = max(TimeInHours),
                  TotalPlays = max(TimeInPlays)) %>%
        arrange(desc(TotalTime))
    }
    else
      (subset_plays() %>%
         select(SongName, Artist, TimeInHours, TimeInPlays) %>%
         group_by(SongName) %>%
         summarise(TotalTime = max(TimeInHours),
                   TotalPlays = max(TimeInPlays)) %>%
         arrange(desc(TotalTime)) %>%
         slice_head(n = as.integer(input$showNumSongs)))
  })
  
  # individual
  output$song_info <- renderTable({
    cln_subset_overall_info() %>%
      select(MaximumRank, ArtistName, AlbumName, DateAdded)
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
  
  # by month
  output$monthData <- renderTable({
    if (input$showNumSongsMonth == "All") {
      subset_plays_by_month() %>%
        select(SongName, Artist, TimeInHours, TimeInPlays) %>%
        arrange(desc(TimeInHours))
    }
    else
      (subset_plays_by_month() %>%
         select(SongName, Artist, TimeInHours, TimeInPlays) %>%
         arrange(desc(TimeInHours)) %>%
         slice_head(n = as.integer(input$showNumSongsMonth)))
  })
  
  
  ########## ARTISTS #############
  
  subset_artist_plays <- reactive({
    cleaned_artist %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y")) %>%
      group_by(ArtistID) %>%
      filter(!is.na(TimeInHours)) %>%
      mutate(DateAdded = min(datenum)) %>%
      ungroup() %>%
      filter(
        !is.na(TimeInHours),
        TimeInHours >= input$topArtistHrs[1],
        TimeInHours <= input$topArtistHrs[2],
        TimeInPlays >= input$topArtistPlayed[1],
        TimeInPlays <= input$topArtistPlayed[2],
        DateAdded >= input$artist_date_range[1],
        DateAdded <= input$artist_date_range[2]
      )
    
  })
  
  cln_artist_by_song_info <- reactive({
    cleaned %>%
      filter(Artist == input$artist_name, !is.na(TimeInHours)) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y"),
             MaxRank = as.integer(MaxRank))
  })
  
  cln_artist_subset <- reactive({
    cleaned_artist %>%
      filter(Artist == input$artist_name, !is.na(TimeInHours)) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y"),
             MaxRank = as.integer(MaxRank))
  })
  
  cln_artist_overall_info <- reactive({
    cleaned_artist %>%
      filter(Artist == input$artist_name) %>%
      filter(!is.na(TimeInHours)) %>%
      summarize(MaximumRank = as.integer(max(MaxRank)),
                SongCount = as.integer(max(SongCount)))
  })
  
  
  
  output$artist_play_info <- renderTable({
    subset_artist_plays() %>%
      select(Artist, TimeInHours, TimeInPlays) %>%
      group_by(Artist) %>%
      summarise(TotalTime = max(TimeInHours),
                TotalPlays = max(TimeInPlays)) %>%
      arrange(desc(TotalTime))
    
  })
  
  output$artist_song_info <- renderTable({
    cln_artist_by_song_info() %>%
      select(SongName, Album, MaxRank) %>%
      group_by(SongName, Album, MaxRank) %>%
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
    cleaned_album %>%
      filter(Album == input$album_name, !is.na(TimeInHours)) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y"))
  })
  
  
  cln_album_overall_info <- reactive({
    cleaned_album %>%
      filter(Album == input$album_name) %>%
      filter(!is.na(TimeInHours)) %>%
      summarize(SongCount = as.integer(max(SongCount)))
  })
  
  cln_album_by_song_info <- reactive({
    cleaned %>%
      filter(Album == input$album_name,!is.na(TimeInHours)) %>%
      mutate(datenum = as.Date(dates, format = "%B %d %Y"))
  })
  
  
  
  output$album_info <- renderTable({
    cln_album_by_song_info() %>%
      select(SongName, Artist, MaxRank) %>%
      group_by(SongName, Artist, MaxRank) %>%
      summarise()
    
  })
  
  output$overall_album_info <- renderTable({
    cln_album_overall_info() %>%
      select(SongCount)
  })
  
  
  
  output$album_big4 <- renderPlot({
    p1al <- cln_album_subset() %>%
      select(datenum, TimeInHours) %>%
      filter(!is.na(TimeInHours)) %>%
      ggplot(aes(x = datenum, y = TimeInHours)) +
      geom_line() +
      labs(x = "Date", y = "Cumulative Hours Listened", title = "Cumulative Hours Listened Over Time")
    
    
    p2al <- cln_album_subset() %>%
      select(datenum, TimeInPlays) %>%
      filter(!is.na(TimeInPlays)) %>%
      ggplot(aes(x = datenum, y = TimeInPlays)) +
      geom_line() +
      labs(x = "Date", y = "Cumulative Plays", title = "Cumulative Plays of the Album's Songs Over Time")
    
    p3al <-  cln_album_subset() %>%
      select(datenum, RankOutOfN) %>%
      filter(!is.na(RankOutOfN)) %>%
      ggplot(aes(x = datenum, y = RankOutOfN)) +
      geom_line() +
      labs(x = "Date", y = "Overall Rank", title = "Overall Rank of the Album Over Time")
    
    p4al <-  cln_album_subset() %>%
      select(datenum, ValueOutOf1) %>%
      filter(!is.na(ValueOutOf1)) %>%
      ggplot(aes(x = datenum, y = ValueOutOf1)) +
      geom_line() +
      labs(x = "Date", y = "Album Value", title = "Overall Value of the Album Over Time")
    
    
    p3al + p4al + p1al + p2al
  })
  
  
  output$dateTimeListenNCAl <-
    renderPlot({
      #non-cumulative listening hours
      cln_album_subset() %>%
        select(datenum, dHours) %>%
        filter(!is.na(dHours)) %>%
        ggplot(aes(x = datenum, y = dHours)) +
        geom_line() +
        labs(x = "Date", y = "Hours Listened By Date", title = "Hours Listened by Date")
    })
  
  
  
  ########## MISCELLANEOUS #############
  
  
  month_subset <- reactive({
    db_misc %>%
      select(input$month_name_bourgeoisie, SongName, Artist, Album) %>%
      na.omit()
      
  })
  
  output$month_stats_bourgeoisie <- renderTable({
    month_subset() %>%
      mutate(NumberOfSongs = n()) %>%
      select(NumberOfSongs) %>%
      slice_head(n=1)
  })
  
  output$month_songs_bourgeoisie <- renderTable({
    month_subset() %>%
      mutate(n = row_number()) %>%
      select(n, SongName, Artist, Album)
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
