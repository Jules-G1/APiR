library(shiny)
library(tidyverse)
library(ggplot2)
library(cofeatureR)
library(plotly)
library(leaflet)
library(DT)

mydata <- read.csv("co2_temperature_pop.csv", header = T, sep=";", dec=".")
mydata

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
                        tabPanel("Map", leafletOutput("map")),
                        tabPanel("Table", dataTableOutput("table"))
            )
        )
    )
)

# Define server logic required to draw a histogram

mydata <- read.csv("co2_temperature_pop.csv", header = T, sep=";", dec=".")

server <- function(input, output) {
    
    
    output$distPlot <- renderPlotly({
        mydata_fit <- mydata %>%
            select(region_x, continent, annee, emissions_totale, variation_temperature, population_totale) %>%
            filter(region_x %in% input$region, annee == input$year) %>%
            group_by(region_x)
        x <- mydata_fit$variation_temperature
        y <- mydata_fit$emissions_totale
        pop <- mydata_fit$population_totale
        
        plot_ly(mydata_fit, x=x, y=y, type='scatter', mode='markers', color= ~continent, marker = list(symbol = 'circle', size= ~population_totale, sizeref=2 * max(pop)/10000,  sizemode = 'area',sizemin=10, sizemax=110, line = list(width = 2, color = '#FFFFFF')), text = ~region_x, hovertemplate = paste(
            "<b>%{text}</b><br><br>",
            "%{yaxis.title.text}: %{y:,.0f}<br>",
            "%{xaxis.title.text}: %{x:.2f}°C<br>",
            "Population: %{marker.size:,}",
            "<extra></extra>"
        )) %>% layout(
            xaxis = list(title = "Variation température",range = c(-1.6050, 2.5450)),
            yaxis = list(title = "Emissions totale",range = c(-4336,13529294)))
    })
    
    output$table <- renderDataTable({
        datatable(mydata %>% select(region_x, continent, annee, emissions_totale, variation_temperature, population_totale) %>% filter(region_x %in% input$region, annee == input$year) %>% group_by(region_x), options = list(
            lengthMenu = list(c(10, 20, -1), c('10', '20', 'All')),
            pageLength = 10
        ))
        
    })
    
    output$map <- renderLeaflet({
        mydata_fit = mydata %>% filter(annee== input$year)
        couleurs <- colorNumeric("YlOrRd", mydata_fit$variation_temperature, n=22)
        mymap90 = leaflet(mydata_fit)%>% addTiles() %>%
            addCircleMarkers(lng = mydata_fit$lng, 
                             lat = mydata_fit$lat, 
                             fillOpacity = 10,
                             popup = ~paste("Variation de la température pour la région",mydata_fit$region_x, "pour l'année",mydata_fit$annee, ":", mydata_fit$variation_temperature),
                             color= ~couleurs(variation_temperature)) %>% 
            addLegend(pal = couleurs, values = ~variation_temperature, opacity = 0.9)
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
