## Data Input App

library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)

ui <- fluidPage(
  theme = shinytheme('yeti'),
  navbarPage(
    title = 'SAAM Data Input',
    tabPanel(
      title='Song',
      mainPanel(
        tags$h3('Song Data Input:'),
        textInput('date','Date (mm-dd-yyyy):',''),
        textInput('songName', 'Song Name:',''),
        textInput('hoursListened', 'Hours Listened:', ''),
        textInput('numPlays', 'Number of Plays:',''),
        textInput('rank','Song Rank:',''),
        textInput('artistName','Artist:',''),
        textInput('albumName','Album:',''),
        actionButton('submitSongData','Submit')
      ),
      sidebarPanel(
        tags$h3('Confirm Input:'),
        verbatimTextOutput('dateOut'),
        verbatimTextOutput('songNameOut'),
        verbatimTextOutput('hoursListenedOut'),
        verbatimTextOutput('numPlaysOut'),
        verbatimTextOutput('rankOut'),
        verbatimTextOutput('artistNameOut'),
        verbatimTextOutput('albumNameOut')
      )
    )
  )
)

server <- function(input, output){
  output$dateOut <- renderText({paste(input$date)})
  output$songNameOut <- renderText({paste(input$songName)})
  output$hoursListenedOut <- renderText({paste(input$hoursListened)})
  output$numPlaysOut <- renderText({paste(input$numPlays)})
  output$rankOut <- renderText({paste(input$rank)})
  output$artistNameOut <- renderText({paste(input$artistName)})
  output$albumNameOut <- renderText({paste(input$albumName)})
}

shinyApp(ui=ui,server=server)
