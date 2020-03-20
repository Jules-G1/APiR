library(shiny)
library(tidyverse)
library(ggplot2)
library(cofeatureR)
library(plotly)

mydata <- read.csv("co2_temperature_pop.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Emission of CO2 vs Temperature variations"),
    
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("region",
                               "Select a Region",
                               choices=levels(mydata$region_x),selected = levels(mydata$region_x)),
            br(),
            sliderInput("year",
                        "Select a Year",
                        min = 1990,
                        max = 2010,
                        value = 1990, animate=T)
        ),
        

        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotlyOutput("distPlot")),
                        tabPanel("Map", verbatimTextOutput("map")),
                        tabPanel("Table", tableOutput("table"))
            )
        )
    )
)

# Define server logic required to draw a histogram

mydata <- read.csv("co2_temperature_pop.csv")

server <- function(input, output) {
    
    
    output$distPlot <- renderPlotly({
        mydata_fit <- mydata %>% select(region_x, continent, annee, emissions_totale, variation_temperature, population_totale) %>% filter(region_x %in% input$region, annee == input$year) %>% group_by(region_x)
        x <- mydata_fit$variation_temperature
        y <- mydata_fit$emissions_totale
        
        plot_ly(mydata_fit, x=x, y=y, type='scatter', mode='markers', size= ~population_totale, sizes = c(10, 80), color= ~continent, marker = list(symbol = 'circle', sizemode = 'diameter', line = list(width = 2, color = '#FFFFFF')), text = ~region_x, hovertemplate = paste(
            "<b>%{text}</b><br><br>",
            "%{yaxis.title.text}: %{y:,.0f}<br>",
            "%{xaxis.title.text}: %{x:.2f}°C<br>",
            "Population: %{population_totale}",
            "<extra></extra>"
        )) %>% layout(
            xaxis = list(title = "Variation température",range = c(-1.6050, 2.5450)),
            yaxis = list(title = "Emissions totale",range = c(-4336,13529294)))
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
