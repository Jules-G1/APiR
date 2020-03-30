library(shiny)
library(tidyverse)
library(plotly)
library(leaflet)
library(DT)
mydata <- read.csv("co2_temperature_pop.csv", header = T, sep=",", dec=".")
# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(tags$style(
        HTML('
         #sidebar {
            background-color: #284157;
            color: white;
        }')
    )),
    
    # Application title
    titlePanel("Emission de C02 vs Variations de température en °C"),
    h4("J. Guillot - C. Beretti - C. Belloir"),
    
    sidebarLayout(
        sidebarPanel( id="sidebar",
            checkboxGroupInput("region",
                               "Choisir une région :",
                               choices=levels(mydata$region),selected = levels(mydata$region)),
            br(),
            sliderInput("year",
                        "Choisir une année :",
                        min = 1990,
                        max = 2010,
                        value = 1990, animate=T)
        ),
        
        
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Graphique", br(),plotlyOutput("distPlot")),
                        tabPanel("Carte", br(),leafletOutput("map")),
                        tabPanel("Table", br(),dataTableOutput("table"))
            ),
            br(),
            uiOutput("tab"),
        )
    )
)

# Define server logic required to draw a histogram



server <- function(input, output) {
    
    output$distPlot <- renderPlotly({
        mydata_fit <- mydata %>%
            select(region, continent,
                   annee, emissions_totale,
                   variation_temperature, population_totale) %>%
            filter(region %in% input$region, annee == input$year) %>%
            group_by(region)
        x <- mydata_fit$variation_temperature
        y <- mydata_fit$emissions_totale
        
        mydata_fit$size <- sqrt(mydata_fit$population_totale * 0.000009)
        
        fig <- plot_ly(mydata_fit, x = ~variation_temperature,
                       y = ~emissions_totale, color = ~continent,
                       size = ~size,
                       type = 'scatter', mode = 'markers', sizes = c(min(mydata_fit$size), max(mydata_fit$size)),
                       marker = list(symbol = 'circle', sizemode = 'diameter',
                                     line = list(width = 2, color = '#FFFFFF')),
                       text = ~paste('Region:', region,
                                     '<br>Emissions:', emissions_totale,
                                     '<br>Variation de température:', variation_temperature,'°C',
                                     '<br>Pop.:', population_totale))
        fig <- fig %>% layout(xaxis = list(title = 'Variation de temperature (en °C)',
                                           gridcolor = 'rgb(255, 255, 255)',
                                           range = c(-1.6050, 2.5450),
                                           zerolinewidth = 1,
                                           ticklen = 5,
                                           gridwidth = 2),
                              yaxis = list(title = 'Emissions de C02',
                                           gridcolor = 'rgb(255, 255, 255)',
                                           range = c(-1500000,13529294),
                                           zerolinewidth = 1,
                                           ticklen = 5,
                                           gridwith = 2))
        
        
        fig
    })
    
    output$table <- renderDataTable({
        datatable(mydata %>%
                      select(region, continent,
                             annee, emissions_totale,
                             variation_temperature, population_totale) %>%
                      filter(region %in% input$region, annee == input$year) %>%
                      group_by(region) %>%
                      arrange(desc(emissions_totale), emissions_totale),
                  options = list(
                        lengthMenu = list(c(10, 20, -1), c('10', '20', 'All')),
                        pageLength = 10
        )) 
        
    })
    
    output$map <- renderLeaflet({
        mydata_fit = mydata %>%
            select(region, lng,
                   lat, continent,
                   annee, emissions_totale,
                   variation_temperature, population_totale) %>%
            filter(region %in% input$region, annee == input$year) %>%
            group_by(region)
        
        couleurs <- colorNumeric("YlOrRd", mydata_fit$variation_temperature, n=22)
        
        mymap90 = leaflet(mydata_fit) %>%
            addTiles() %>%
            addCircleMarkers(lng = mydata_fit$lng, 
                             lat = mydata_fit$lat, 
                             fillOpacity = 10,
                             radius = ~ sqrt(population_totale * 0.000001),
                             popup = ~paste("Variation de la température en °C pour la région",
                                            mydata_fit$region,
                                            "pour l'année",
                                            mydata_fit$annee, ":",
                                            mydata_fit$variation_temperature),
                             color= ~couleurs(variation_temperature)) %>% 
            addLegend(pal = couleurs,
                      values = ~variation_temperature,
                      title = "Variation de température en °C",
                      opacity = 0.9)
    })
    
    url <- a("FAOSTAT", href="http://www.fao.org/faostat/en/#home")
    output$tab <- renderUI({
        tagList("source:", url)
    })
    
    }


# Run the application 
shinyApp(ui = ui, server = server)
