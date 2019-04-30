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
#mdat2 <- read_rds(here::here("inst/shiny/widgets/lemis_dat_by_country_sf.rds"))
mdat_d <- read_rds(here::here("inst/shiny/widgets/lemis_dat_by_country_dor.rds"))

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

    # get_dor <- reactive({
    #
    #     w <- mdat %>%
    #         filter(year == input$year,
    #                taxa == input$taxa
    #         ) %>%
    #         mutate(field =  n_by_country_taxa)
    #
    #     w_dor <- cartogram_dorling(w, "field", k=1)
    #
    #     list(w_dor = w_dor)
    # })

    output$mapd <- renderPlotly({

        #get_plt <- paste(input$taxa, input$year, sep = "_")

        geo <- list(
            showland = TRUE,
            landcolor = toRGB("gray95")
        )

        # works
        # plot_geo( map_data("world")) %>%
        #     add_sf(data = mdat_d[["Shell_2014"]],
        #            color = ~continent,
        #            type = "scattergeo",
        #            split = ~iso3c,
        #            text = ~paste0(country_name, "\nN = ", round(n_by_country_taxa, 0)),
        #            hoverinfo = "text") %>%
        #     layout(geo = geo, showlegend = FALSE)

        # works
        # plot_ly(data = mdat_d[["Shell_2014"]],
        #         color = ~continent,
        #         type = "scattergeo",
        #         split = ~iso3c,
        #         text = ~paste0(country_name, "\nN = ", round(n_by_country_taxa, 0)),
        #         hoverinfo = "text") %>%
        #     layout(geo = geo, showlegend = FALSE)

        # works but not with dorling
        # test <- mdat_d[["Shell_2014"]] %>%
        #     st_centroid() %>%
        #     st_coordinates() %>%
        #     as_tibble() %>%
        #     mutate(country_name = mdat_d[["Shell_2014"]]$country_name,
        #            continent = mdat_d[["Shell_2014"]]$continent,
        #            iso3c = mdat_d[["Shell_2014"]]$iso3c,
        #            n_by_country_taxa = mdat_d[["Shell_2014"]]$n_by_country_taxa)
        #
        # plot_geo() %>%
        #     add_trace(data = test,#mdat_d[["Shell_2014"]],
        #               x = ~X, y = ~Y,
        #               size = ~n_by_country_taxa,
        #               color = ~continent,
        #               # mode = "marker",
        #               split = ~iso3c,
        #               text = ~paste0(country_name, "\nN = ", round(n_by_country_taxa, 0)),
        #               hoverinfo = "text",
        #               marker=list(sizeref = .1, sizemode="area")) %>%
        #     layout(geo = geo, showlegend = FALSE)

        plot_geo() %>%
            add_trace(
                data = mdat_d[["Shell_2014"]],
                x = ~x,
                y = ~y,
                color = ~continent,
                fill = "toself",
                type = "scattergeo",
                mode = "lines",
                split = ~iso3c,
                text = ~paste0(country_name, "\nN = ", round(n_by_country_taxa, 0)),
                hoverinfo = "text"
            ) %>%
            layout(geo = geo, showlegend = FALSE)

        # doesn't work (b/c sf object)
        # plot_geo() %>%
        # add_trace(
        #    # add_sf(
        #        data = mdat_d[["Shell_2014"]],
        #        color = ~continent,
        #        type = "scattergeo",
        #        # mode = "marker",
        #        #split = ~iso3c,
        #        text = ~paste0(country_name, "\nN = ", round(n_by_country_taxa, 0)),
        #        hoverinfo = "text"
        #        ) %>%
        # layout(geo = geo, showlegend = FALSE)

    })

    observeEvent({
        input$taxa
        input$year}, {

        get_plt <- paste(input$taxa, input$year, sep = "_")

        plotlyProxy("mapd") %>%
            plotlyProxyInvoke("deleteTraces", list(as.integer(0:20)))


        plotlyProxy("mapd") %>%
            plotlyProxyInvoke("addTraces",
                              list(
                                  data = mdat_d[[get_plt]],
                                  x = ~x,
                                  y = ~y
                                  # color = ~continent,
                                  # fill = "toself",
                                  # type = "scattergeo",
                                  # mode = "lines",
                                  # split = ~iso3c,
                                  # text = ~paste0(country_name, "\nN = ", round(n_by_country_taxa, 0)),
                                  # hoverinfo = "text"
                              )
            )

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
