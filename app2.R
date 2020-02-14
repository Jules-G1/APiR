library(shiny)
library(tidyverse)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Emission of CO2 vs Temperature variations"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("region",
                               "Select a Region",
                               choices=levels(mydata$region_x),selected = levels(mydata$region_x)),
            #br(),
            sliderInput("year",
                        "Select a Year",
                        min = 1990,
                        max = 2010,
                        value = 2000, animate=T)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotOutput("distPlot")),
                        tabPanel("Map", verbatimTextOutput("map")),
                        tabPanel("Table", tableOutput("table"))
            )
        )
    )
)

# Define server logic required to draw a histogram

mydata <- read.csv("co2_temperature_pop.csv")

server <- function(input, output) {
    
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        df <- mydata %>% select(region_x,annee,emissions_totale,variation_temperature,population_totale) %>% filter(region_x %in% input$region, annee == input$year) %>% group_by(region_x)
        x <- df$variation_temperature
        y <- df$emissions_totale
        
        # draw the histogram with the specified number of bins
        plot(x, y,ylab="Intensité des émissions",
             xlab="Variation de temperature", xlim=c(-1.5050 , 2.5350),ylim=c(-3236,12529194))
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
