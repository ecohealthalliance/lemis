library(shiny)
library(tidyverse)
library(plotly)
library(cartogram)
library(sf)
library(highcharter)
library(RColorBrewer)
#devtools::install_github("annegretepeek/d3treeR")

dat <- read_csv(here::here("inst/shiny/widgets/lemis_dat_format.csv")) %>% filter(!is.na(taxa))
mdat <- read_rds(here::here("inst/shiny/widgets/lemis_dat_by_taxa_sf.rds"))
mdat2 <- read_rds(here::here("inst/shiny/widgets/lemis_dat_by_country_sf.rds"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    sidebarLayout(
        sidebarPanel(width = 3,
                     sliderInput("year",
                                 "Year",
                                 min = min(dat$year, na.rm = TRUE),
                                 max = max(dat$year, na.rm = TRUE),
                                 value = 2014,
                                 sep = ""),
                     selectInput("taxa", "", choices = unique(dat$taxa))

        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Dorling",
                         fluidRow(
                             column(12, plotlyOutput("mapd"))
                         )
                ),
                # tabPanel("Dorling 2",
                #          fluidRow(
                #              column(12, plotlyOutput("mapd2"))
                #          )
                # ),
                tabPanel("Tree Map",
                         fluidRow(
                             column(12, highchartOutput("tree"))
                         )
                )
            )
        )
    )
)

server <- function(input, output) {

    output$mapd <- renderPlotly({

        w <- mdat %>%
            filter(year == input$year,
                   taxa == input$taxa
            ) %>%
            mutate(field =  n_by_country_taxa)

        w_dor <- cartogram_dorling(w, "field", k=1)
        w_dor_cenr <- w_dor %>%
            st_centroid() %>%
            st_coordinates() %>%
            as_tibble() %>%
            mutate(country_name = w_dor$country_name,
                   continent = w_dor$continent,
                   iso3c = w_dor$iso3c,
                   field = w_dor$field) %>%
            filter(field >= quantile(field, 0.90))

        plot_geo(map_data("world")) %>%
            add_sf(
                data = w_dor,
                #color = ~field,
                color = ~continent,
                split = ~iso3c,
                text = ~paste0(country_name, "\nN = ", round(field, 0)),
                hoverinfo = "text"
            ) %>%
            # add_annotations(
            #     data = w_dor_cenr,
            #     x = ~X, y = ~Y,
            #     text = ~iso3c,
            #     #textposition = 'middle right',
            #     textfont =  list(
            #         family = "sans serif",
            #         size = 14,
            #         color = toRGB("black")),
            #     showarrow = FALSE
            # ) %>%
        layout(showlegend = FALSE)
    })

    output$tree <- renderHighchart({

        dat %>%
            mutate(continent_int = as.numeric(as.factor(continent))) %>%
            filter(year == input$year,
                   taxa == input$taxa
            )%>%
            hctreemap2(
                group_vars = c("continent", "country_name"),
                size_var = "n_by_country_taxa",
                color_var = "continent_int",
                layoutAlgorithm = "squarified",
                levelIsConstant = FALSE,
                levels = list(
                    list(level = 1, dataLabels = list(enabled = TRUE)),
                    list(level = 2, dataLabels = list(enabled = TRUE))
                )
            ) %>%
            hc_colorAxis(dataClasses = color_classes(seq(1:7))) %>%
            hc_legend(enabled = F) %>%
            hc_add_theme(hc_theme_ggplot2()) %>%
            hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
             N = {point.value}")
    })

}

# Run the application
shinyApp(ui = ui, server = server)
