
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(tuneR)
library(signal)
library(matrixStats)
library(shiny)
source("Funkcje.R")

shinyServer(function(input, output) {
  
  butclick <- 0
  inFile <- NULL
  show <- reactive({
      if(is.null(input$inputfile)) {
        ret <- FALSE;
      } else {
        ret <- TRUE; 
      }
      print((input$inputfile$name))
      print(ret)
    ret
    });
    
  genre_results <- reactive({
      print(paste(input$inputfile$datapath,input$inputfile$name, sep=""))
      file.rename(input$inputfile$datapath,paste(input$inputfile$datapath,input$inputfile$name, sep=""))
      if(is.null(input$inputfile$name)) {
        ret <- data.frame(R=0,J=0,M=0,H=0,C=0);
      } else {
        ret <- ClassifyMusicGenre(paste(input$inputfile$datapath,input$inputfile$name, sep=""), a=0.2, b=0.6, c=0.05, d=0.05, e=0.05, f=0.05, g=0); 
      }
      print(ret)
      ret
    });
  
  output$info <- renderUI({
    tags$div(class="hide container",
             style= if (show()) "display: block;" else "display: block !important;",
             tags$p("Znasz kapitalny utwór? Kolega spytał się jaką lubisz muzykę? Ciekawi Cię zgłębienie wiedzy muzycznej? Dobrze trafiłeś! Z naszą wspaniałą aplikacją będziesz mógł rozpoznać gatunek dowolnej piosenki! Wystarczy tylko wybrać plik z muzyką i gotowe!")
    )

  })
    
  output$results <- renderUI({
    tags$div(class="hide",
             style= if (show()) "display: block !important;" else "display: block;",
             tags$div(class = "page-header",
                      tags$h3(paste("Utwór", input$inputfile$name, "to:"))
                      ),
             tags$label("Rock"),
             tags$div(id="rock",
                      class=" progress shiny-html-output"
               ),
             tags$label("Jazz"),
             tags$div(id="jazz",
                      class=" progress shiny-html-output"
             ),
             tags$label("Metal"),
             tags$div(id="metal",
                      class=" progress shiny-html-output"
             ),
             tags$label("Hip-Hop"),
             tags$div(id="hiphop",
                      class=" progress shiny-html-output"
             ),
             tags$label("Muzyka klasyczna"),
             tags$div(id="classic",
                      class=" progress shiny-html-output"
             )
            )    
  })
  
  
  
  output$rock <- renderUI({
    tags$div(class = "progress-bar", 
             role="progressbar", 
             style= paste("width: ", toString(genre_results()["R"]), "%", sep=""),
             tags$span(paste(toString(genre_results()["R"]), "%", sep=""))
             )
  })
  
  output$jazz <- renderUI({
    tags$div(class = "progress-bar progress-bar-success", 
             role="progressbar", 
             style= paste("width: ", toString(genre_results()["J"]), "%", sep=""),
             tags$span(paste(toString(genre_results()["J"]), "%", sep=""))
             )
  })
  
  output$metal <- renderUI({
    tags$div(class = "progress-bar progress-bar-info", 
             role="progressbar", 
             style= paste("width: ", toString(genre_results()["M"]), "%", sep=""),
             tags$span(paste(toString(genre_results()["M"]), "%", sep=""))
             )
  })
  
  output$hiphop <- renderUI({
    tags$div(class = "progress-bar progress-bar-warning", 
             role="progressbar", 
             style= paste("width: ", toString(genre_results()["H"]), "%", sep=""),
             tags$span(paste(toString(genre_results()["H"]), "%", sep=""))
             )
  })
  
  output$classic <- renderUI({
    tags$div(class = "progress-bar progress-bar-danger", 
             role="progressbar", 
             style= paste("width: ", toString(genre_results()["C"]), "%", sep=""),
             tags$span(paste(toString(genre_results()["C"]), "%", sep=""))
             )
  })  
})