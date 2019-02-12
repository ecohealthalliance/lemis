#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(chorddiag)
library(htmlwidgets)
trade_matrices <- readRDS("trade_matrices.rds")
year_min <- min(as.numeric(names(trade_matrices)))
year_max <- max(as.numeric(names(trade_matrices)))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$chordplot <- renderChorddiag({
        yr <- as.character(input$yr)
        chorddiag(trade_matrices[[yr]], type = "bipartite")
    },
    )
})
