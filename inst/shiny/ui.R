#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(htmlwidgets)
trade_matrices <- readRDS("trade_matrices.rds")
year_min <- min(as.numeric(names(trade_matrices)))
year_max <- max(as.numeric(names(trade_matrices)))

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("LEMIS Shiny App"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("yr",
                        "Year of trade",
                        min = year_min,
                        max = year_max,
                        value = 2010,
                        sep = "")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h1("Imports From Continents to U.S. Ports"),
            chorddiag::chorddiagOutput("chordplot")
        )
    )
))
