library(shiny)
library(tidyverse)
library(plotly)

dat <- read_csv(here::here("inst/shiny/widgets/lemis_dat_format.csv")) %>% filter(!is.na(taxa))

mdat <- read_rds(here::here("inst/shiny/widgets/lemis_dat_sf.rds"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    sidebarLayout(
        sidebarPanel(width = 3,
                     # sliderInput("year",
                     #             "Year",
                     #             min = min(dat$year, na.rm = TRUE),
                     #             max = max(dat$year, na.rm = TRUE),
                     #             value = 2013,
                     #             sep = ""),
                     selectInput("taxa", "", choices = unique(dat$taxa)),
                     radioButtons("size", "", choices = c("Imports (n)", "Value ($)")
                     )
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Bubble",
                         fluidRow(
                             column(12, plotlyOutput("map") )
                         )
                ),
                tabPanel("Dorling",
                         fluidRow(
                             column(12, plotlyOutput("mapd"))
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
                #x = ~jitter(as.numeric(as.factor(centroid_lon))),
                x = ~centroid_lon,
                #y = ~jitter(as.numeric(as.factor(centroid_lat))),
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
            filter(taxa == input$taxa,
                   year == "2011")
        w_dor <- cartogram_dorling(w, "n_by_country_taxa")

        plot_ly(stroke = I("black"), span = I(1)) %>%
            add_sf(
                data = w_dor,
                color = ~n_by_country_taxa,
                split = ~country_name,
                text = ~paste(country_name),
                hoverinfo = "text",
                hoveron = "fills"
            ) %>%
            layout(showlegend = FALSE)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
