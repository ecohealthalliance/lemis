library(shiny)
library(tidyverse)
library(plotly)
library(cartogram)
library(sf)
library(treemap)


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
                                 value = 2013,
                                 sep = ""),
                     selectInput("taxa", "", choices = unique(dat$taxa)),
                     radioButtons("size", "", choices = c("Imports (n)", "Value ($)")
                     )
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Map",
                         fluidRow(
                             column(12, plotlyOutput("map") )
                         )
                ),
                tabPanel("Dorling",
                         fluidRow(
                             column(12, plotlyOutput("mapd"))
                         )
                ),
                tabPanel("Dorling 2",
                         fluidRow(
                             column(12, plotlyOutput("mapd2"))
                         )
                ),
                tabPanel("Tree Map",
                         fluidRow(
                             column(12, plotOutput("tree"))
                         )
                )
            )
        )
    )
)

server <- function(input, output) {

    get <- reactive({
        field <- ifelse(input$size == "Imports (n)", "n_by_country_taxa", "val_by_country_taxa")
        list(field = field)
    })

    output$map <- renderPlotly({
        dat %>%
            mutate(field = !!sym(get()$field)) %>%
            filter(
                #year == input$year,
                taxa == input$taxa,
                field >= quantile(field, .5)
            ) %>%
            plot_geo() %>%
            add_markers(
                x = ~centroid_lon,
                y = ~centroid_lat,
                hoverinfo="text",
                text =  ~paste0(country_name, '\n', field),
                #mode = 'markers',
                size = ~field,
                frame = ~year,
                marker=list(sizeref = 0.25, sizemode="area")
            ) %>%
            animation_opts(
                1000, easing = "elastic", redraw = FALSE
            )
    })

    output$mapd <- renderPlotly({

        w <- mdat %>%
            filter(year == 2011,
                   taxa = ) %>%
            distinct() %>%
            mutate(field = n_by_country
                   # !!sym(get()$field)
            )

        w_dor <- cartogram_dorling(w, "n_by_country") #get()$field)
        w_dor_cenr <- w_dor %>%
            st_centroid() %>%
            st_coordinates() %>%
            as_tibble() %>%
            mutate(country_name = w_dor$country_name,
                   continent = w_dor$continent,
                   field = w_dor$field) %>%
            filter(field >= quantile(field, 0.90))

        # w <- mdat %>%
        #     filter(taxa == "Fish",
        #            year == "2011") %>%
        #     mutate(field = n_by_country_taxa)
        #
        # w_dor <- cartogram_dorling(w, "n_by_country_taxa")
        # w_dor_cenr <- w_dor %>%
        #     st_centroid() %>%
        #     st_coordinates() %>%
        #     as_tibble() %>%
        #     mutate(country_name = w_dor$country_name,
        #            field = w_dor$field) %>%
        #     arrange(-field) %>%
        #     slice(1:20)

        plot_ly(stroke = I("black"), span = I(1)) %>%
            add_sf(
                data = w_dor,
                color = ~continent,
                #color = ~continent,
                split = ~country_name,
                text = ~paste0(country_name, "\n", get()$field,  ": ", round(field, 0)),
                hoverinfo = "text",
                hoveron = "fills"
            ) %>%
            add_annotations(
                data = w_dor_cenr,
                x = ~X, y = ~Y,
                text = ~country_name,
                #textposition = 'middle right',
                textfont =  list(
                    family = "sans serif",
                    size = 14,
                    color = toRGB("black")),
                showarrow = FALSE
            ) %>%
            layout(showlegend = FALSE)
    })

    output$mapd2 <- renderPlotly({

        w <- mdat2 %>%
            filter(
                year == input$year
            ) %>%
            mutate(field = n_by_country#!!sym(get()$field)
            )

        w_dor <- cartogram_dorling(w, "field")
        w_dor_cenr <- w_dor %>%
            st_centroid() %>%
            st_coordinates() %>%
            as_tibble() %>%
            mutate(country_name = w_dor$country_name,
                   most_common_taxa = w_dor$most_common_taxa,
                   field = w_dor$field) %>%
            filter(field >= quantile(field, 0.90))

        plot_ly(stroke = I("black"), span = I(1)) %>%
            add_sf(
                data = w_dor,
                color = ~most_common_taxa,
                #split = ~country_name,
                text = ~paste0(country_name, "\n",  "n_by_country: ", round(field, 0)),
                hoverinfo = "text",
                hoveron = "fills"
            ) %>%
            add_annotations(
                data = w_dor_cenr,
                x = ~X, y = ~Y,
                text = ~country_name,
                #textposition = 'middle right',
                textfont =  list(
                    family = "sans serif",
                    size = 14,
                    color = toRGB("black")),
                showarrow = FALSE
            )
    })

    output$tree <- renderPlot({


        dat %>%
            filter(year == input$year, taxa == input$taxa) %>%
            treemap(.,
                    index=c("continent","country_name"),
                    vSize="n_by_country_taxa",
                    type="index"
            )

    })

}

# Run the application
shinyApp(ui = ui, server = server)
