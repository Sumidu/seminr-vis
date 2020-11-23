## app.R ##
library(shinydashboard)
library(seminr)
library(DiagrammeR)
library(glue)
library(tidyverse)

source("seminrvis.R")

ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            box(width = 9, grVizOutput("plot1", height = 500)),

            box(width = 3,
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
            ),
            box(width = 9,
                shiny::verbatimTextOutput("grcode"))
        )
    )
)









server <- function(input, output) {


    measurements <- constructs(
        reflective("Image",       multi_items("IMAG", 1:5)),
        reflective("Expectation", multi_items("CUEX", 1:3)),
        reflective("Loyalty",     multi_items("CUSL", 1:3)),
        reflective("Complaints",  single_item("CUSCO"))
    )

    structure <- relationships(
        paths(from = c("Image", "Expectation"), to = c("Complaints", "Loyalty")
        )
    )

    pls_model <- estimate_pls(data = mobi, measurements, structure)
    bs_model <- bootstrap_model(pls_model, seed = 1)

    output$grcode <- shiny::renderText({
        plot_model(bs_model)
    })

    output$plot1 <- renderGrViz({
        plot_model(bs_model) %>% grViz()
    })
}

shinyApp(ui, server)
