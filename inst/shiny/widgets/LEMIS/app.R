library(shiny)
library(tidyverse)
library(plotly)
library(cartogram)
library(sf)
library(highcharter)
library(RColorBrewer)
#devtools::install_github("annegretepeek/d3treeR")

dat <- read_csv(here::here("inst/shiny/widgets/lemis_dat_format.csv")) %>% filter(!is.na(taxa))
#mdat <- read_rds(here::here("inst/shiny/widgets/lemis_dat_by_taxa_sf.rds"))

mdat_d <- read_rds(here::here("inst/shiny/widgets/lemis_dat_by_country_dor.rds"))
mdat_sf <- read_rds(here::here("inst/shiny/widgets/lemis_dat_by_country_sf.rds"))

timestamp <- file.info("inst/shiny/widgets/lemis_dat_by_country_sf.rds")$ctime

# notes
# add_trace does not work with sf data - used internal plotly function to transform input data (dat-process.R)
# plotlyProxyInvoke("addTraces"...) doesn't behave the same as add_traces().
#    a) need to specify type = "scattergeo"  (which is not needed when 2nd add_trace is used in plot_ly or plot_geo call)
#    b) it is rendering behind exisiting plot

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
                tabPanel("Dorling - ggplot",
                         fluidRow(
                             column(12, plotOutput("mapd2", hover = "plot_hover"),
                                    uiOutput("dynamic"))
                         )
                ),
                tabPanel("Tree Map - highcharter",
                         fluidRow(
                             column(12, highchartOutput("tree"))
                         )
                )
            )
        )
    )
)

server <- function(input, output) {

    # Plotly Dorling
    output$mapd <- renderPlotly({

        #get_plt <- paste(input$taxa, input$year, sep = "_")

        geo <- list(
            showland = TRUE,
            landcolor = toRGB("gray95")
        )

        plot_ly(
            data = mdat_d[["Shell_2014"]],
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

            plotlyProxy("mapd") %>%
                plotlyProxyInvoke("addTraces",
                                  list(
                                      data = mdat_d[[get_plt]],
                                      #inherit = FALSE,
                                      x = ~x,
                                      y = ~y,
                                      color = ~continent,
                                      #type = "scattergeo",
                                      fill = "toself",
                                      mode = "lines",
                                      split = ~iso3c,
                                      text = ~paste0(country_name, "\nN = ", round(n_by_country_taxa, 0)),
                                      hoverinfo = "text")
                )

        })

    # ggplot cached dorling
    output$mapd2 <- renderCachedPlot({

        get_plt <- paste(input$taxa, input$year, sep = "_")

        ggplot(mdat_sf[[get_plt]], aes(color = continent, fill = continent)) +
            geom_sf()


    }, cacheKeyExpr = {
        list(input$taxa, input$year, "dorling", timestamp) },
    cache = "session" # session or app
    )

    output$dynamic <- renderUI({
        req(input$plot_hover)
        verbatimTextOutput("vals")
    })

    output$vals <- renderPrint({
        hover <- input$plot_hover
        # print(str(hover)) # list
        # y <- nearPoints(iris, input$plot_hover)[input$var_y]
        # req(nrow(y) != 0)
        # y
        "test"
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

}

# Run the application
shinyApp(ui = ui, server = server)
