library(shiny)
library(tidyverse)
library(plotly)
library(sf)
library(highcharter)
library(echarts4r)
library(RColorBrewer)
library(leaflet)
library(leaflet.extras)
#devtools::install_github("annegretepeek/d3treeR")

dat <- read_csv(here::here("inst/shiny/widgets/lemis_dat_format.csv")) %>% filter(!is.na(taxa))

mdat_d <- read_rds(here::here("inst/shiny/widgets/lemis_dat_by_country_dor.rds"))
mdat_lf <- read_rds(here::here("inst/shiny/widgets/lemis_dat_by_country_lef.rds"))

#timestamp <- file.info("inst/shiny/widgets/lemis_dat_by_country_sf.rds")$ctime

# notes
# plotly
# add_trace does not work with sf data - used internal plotly function to transform input data (dat-process.R)
# plotlyProxyInvoke("addTraces"...) doesn't behave the same as add_traces().
#    a) need to specify type = "scattergeo"  (which is not needed when 2nd add_trace is used in plot_ly or plot_geo call)
#    b) it is rendering behind exisiting plot

# echarts4r
# proxy does not seems to be designed to fully redraw the plot: https://echarts4r.john-coene.com/articles/shiny.html
# but goes fast when dispose = TRUE

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
                tabPanel("Dorling - plotly",
                         fluidRow(
                             column(12, plotlyOutput("mapd"))
                         )
                ),
                tabPanel("Dorling - leaflet",
                         fluidRow(
                             column(12, leafletOutput("mapd2"))
                         )
                ),
                tabPanel("Tree Map - highcharter",
                         fluidRow(
                             column(12, highchartOutput("tree"))
                         )
                ),
                tabPanel("Tree Map - echarts4r",
                         fluidRow(
                             column(12, echarts4rOutput("tree2"))
                         )
                )
            )
        )
    )
)

server <- function(input, output) {

    # Plotly Dorling
    output$mapd <- renderPlotly({

        get_plt <- paste(input$taxa, input$year, sep = "_")

        geo <- list(
            showland = TRUE,
            landcolor = toRGB("gray95")
        )

        plot_ly(
            data = mdat_d[[get_plt]],
            #data = mdat_d[["Shell_2014"]],
            x = ~x,
            y = ~y,
            color = ~continent,
            type = "scattergeo",
            fill = "toself",
            mode = "lines",
            split = ~iso3c,
            text = ~paste0(country_name, "\nN = ", round(n_by_country_taxa, 0)),
            hoverinfo = "text"
        ) %>%
            # add_trace(
            #     data = mdat_d[["Bird_2014"]],
            #     x = ~x,
            #     y = ~y,
            #     color = ~continent,
            #     type = "scattergeo",
            #     fill = "toself",
            #     mode = "lines",
            #     split = ~iso3c,
            #     text = ~paste0(country_name, "\nN = ", round(n_by_country_taxa, 0)),
            #     hoverinfo = "text"
        # ) %>%
        layout(geo = geo, showlegend = FALSE)

    })

    # Proxy update Plotly Dorling - not working :(
    observeEvent({
        input$taxa
        input$year}, {

            get_plt <- paste(input$taxa, input$year, sep = "_")

            # plotlyProxy("mapd") %>%
            #     plotlyProxyInvoke("deleteTraces", as.list(1:100))

            # plotlyProxy("mapd") %>%
            #     plotlyProxyInvoke("addTraces",
            #                       list(
            #                           data = mdat_d[[get_plt]],
            #                           #inherit = FALSE,
            #                           x = ~x,
            #                           y = ~y,
            #                           color = ~continent,
            #                           #type = "scattergeo",
            #                           fill = "toself",
            #                           mode = "lines",
            #                           split = ~iso3c,
            #                           text = ~paste0(country_name, "\nN = ", round(n_by_country_taxa, 0)),
            #                           hoverinfo = "text")
            #     )

        })

    output$mapd2 <- renderLeaflet({

        get_plt <- paste(input$taxa, input$year, sep = "_")
        dat <-mdat_lf[["Shell_2014"]]

        pal <- colorFactor(
            palette = 'Dark2',
            domain = dat$continent
        )

        leaflet(dat) %>%
            addTiles() %>%
            addCircles(lng = ~X, lat = ~Y, weight = 1,
                       color = ~pal(continent),
                       radius = ~radius,
                       label = ~paste0(country_name, "\nN = ", round(n_by_country_taxa, 0))) %>%
            addResetMapButton() %>%
            fitBounds( lng1 = -80,
                       lng2 = 80,
                       lat1 = -40,
                       lat2 = 65 )

    })

    # Proxy update Leaflet Dorling
    observeEvent({
        input$taxa
        input$year}, {

            get_plt <- paste(input$taxa, input$year, sep = "_")
            dat <-mdat_lf[[get_plt]]

            pal <- colorFactor(
                palette = 'Dark2',
                domain = dat$continent
            )

            leafletProxy("mapd2", data = dat) %>%
                clearShapes() %>%
                addCircles(lng = ~X, lat = ~Y, weight = 1,
                           color = ~pal(continent),
                           radius = ~radius,
                           label = ~paste0(country_name, "\nN = ", round(n_by_country_taxa, 0))) %>%
                addResetMapButton() %>%
                fitBounds( lng1 = -80,
                           lng2 = 80,
                           lat1 = -40,
                           lat2 = 65 )


        })


    # Highcharter treemap -  not proxy compatable but fast
    output$tree <- renderHighchart({

        dat %>%
            mutate(continent_int = as.numeric(as.factor(continent))) %>%
            filter(year == input$year,
                   taxa == input$taxa
            ) %>%
            hctreemap2(
                group_vars = c("continent", "country_name"),
                size_var = "n_by_country_taxa",
                color_var = "continent_int",
                layoutAlgorithm = "squarified",
                levelIsConstant = FALSE,
                animation = TRUE,
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

    # echarts4r treemap
    output$tree2 <- renderEcharts4r({

        dat %>%
            filter(year == input$year,
                   taxa == input$taxa
            ) %>%
            e_charts(dispose = TRUE) %>%
            e_treemap(continent, country_name, n_by_country_taxa) %>%
            e_tooltip(trigger = "item")

    })

    # Proxy update echarts4r

    # observeEvent({
    #     input$taxa
    #     input$year}, {
    #
    #          echarts4rProxy("tree2")
    # })

}

# Run the application
shinyApp(ui = ui, server = server)
