#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#require packages
library(shiny)
library(tidyverse)
library(gdata)

#import and wrangle data
taxable <- read.xls("investment_mix.xls", sheet = 1, header = T)

taxable <- taxable %>% gather(key = "asset", value = 'percentage', 3:9)

taxable <- taxable %>% mutate(allocation = 0)

retirement <- read.xls("investment_mix.xls", sheet = 2, header = T)

retirement <- retirement %>% gather(key = "asset", value = 'percentage', 3:9)

retirement <- retirement %>% mutate(allocation = 0)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Portfolio Rebalance Tool"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        numericInput(inputId = "asset_value", 
                     label = "Input Balance",
                     value = 10000),
        
        selectInput(inputId = "portfolio_type",
                    label = "Select Portfolio Type", 
                    choices = c('Taxable', 'Retirement')),
        
         sliderInput(inputId = "risk_tolerance",
                     label = "Select Risk Tolerance",
                     min = 0.5,
                     max = 10,
                     value = 4, 
                     step = 0.5)
          ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput(outputId = "folio_percentage_plot"),
         plotOutput(outputId = "folio_allocation_plot")
      )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

#reactive data frame
folio_mix <- reactive({
  taxable %>% filter(risk.tolerance == input$risk_tolerance) %>%
    mutate(allocation = input$asset_value * percentage)
  })

#percentage plot
output$folio_percentage_plot <- renderPlot({
      ggplot(folio_mix(), aes(x = asset, y = percentage))+
    geom_bar(stat = "identity")
   })

#dollar value plot
observe({
output$folio_allocation_plot <- renderPlot({
  ggplot(folio_mix(), aes(x  = asset, y = allocation))+
    geom_bar(stat = "identity")
  })
})

}

# Run the application 
shinyApp(ui = ui, server = server)

