Stat 850 Project Description
================
Molly Creagar, Charlie Bonk

(Math Day 2020)

## Instructions

Each member of your team should modify this document in some way and
push their modifications to the repository in a separate commit. This
will ensure that you have set your repository up in a way that ensures
all group members are working with the same repository.

Note that you must compile your readme (this document) for it to
properly display as part of your github repository.

Once you have received feedback on your project proposal (via Canvas)
you may alter this README so that it describes your final project
instead of the project proposal.


## Data Set

The 'data set' is more of a database, as there are four separate Google Sheets with possible data sources. Most of the data is stored in the "Songs" Google Sheet, though one of the requested items for the Music Database is to make a display of the songs that were featured in the `Bourgeoisie' playlist, and that information is stored in the "Miscellaneous" Google Sheet. The links are below.

- [Songs](https://docs.google.com/spreadsheets/d/1T3LxNnSTmUyefwX-i0nEmkPcg-mKYnb9RmcJ_DopIM8/edit?usp=sharing)
- [Artists](https://docs.google.com/spreadsheets/d/1phfgD-aPoxDC_T3Te8hg2_rbpPJCk-yNCua98BYwpH0/edit?usp=sharing)
- [Albums](https://docs.google.com/spreadsheets/d/1QffuHFZZ6SdaLq7HEi8XTzKmUifinZgU9hZb8WYIDIU/edit?usp=sharing)
- [Miscellaneous](https://docs.google.com/spreadsheets/d/1_i_tN719jseXvPqpufLW8pTfEgDY5CpTRw9B14Sxewc/edit?usp=sharing)

There are many variables in each of the files. Common variables acorss each file are: Song Name, Artist, Album, Hours (or some form of this), Play Count (or some form of this). Other variables are Date Added, Max Rank, Hour Differential, Play Differential, Total (all-time) Hours, Total (all-time) Plays, and others. There are currently more than 2000 songs in the database, and more are added each month.

## Short Project Description

We want to make an R Shiny app to access, analyze, and explore the various data sets. This approach would be a good fit for the data we have because:

- There is **a lot** of data and even to a trained individual who knows what they are looking for, it can be overwhelming.
- An R Shiny app would make the experience much more user friendly and much more exploratory. 
  - The user could, for example, search for statistics for a single song they like without having to know much of anything on how R or data filtering works.
- An R Shiny app also would help with managing all of the data sets at once. Since all of the hard work is done behind the visage of the app, users will not have to worry about which data set they need to look at to get the information they want: we take care of this in the server based on their user interface (UI) input.
  - Once we get the "must haves" from the owner, we can begin prepping a rough sketch of how the UI should flow and look.
  - The skeleton of the UI will be the first part of the app that is finished. Once we have an idea of what the users should be able to do and how it will be presented to them, we can begin working on the server function side to take the input and get an output for their request.


## Potential Topics to Explore Using the Data Set

-Make a searchable database of listening statistics from August 2013 - present

-Add some nice graphics (artist trends, yearly/monthly trends, etc)

-Provide a client-requested interface that can be hosted in a Shiny app and used as a replacement for the current Google Sheets method.

-Incorporate an owner-friendly element where the owner can just submit a single month's worth of data and have the database update with new statistics and graphics

## Group Members

Molly Creagar

Charlie Bonk
