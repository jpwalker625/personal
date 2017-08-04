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

#import data
taxable <- read.xls("investment_mix.xls", sheet = 1, header = T)
retirement <- read.xls("investment_mix.xls", sheet = 2, header = T)

#fix column names
colnames(taxable) <- gsub(pattern = '\\.', replacement = ' ', x = colnames(taxable))
colnames(retirement) <- gsub(pattern = '\\.', replacement = ' ', x = colnames(retirement))

#tidy data by gather asset columns into one
taxable <- taxable %>% gather(key = "asset", value = 'percentage', 3:9)
retirement <- retirement %>% gather(key = "asset", value = 'percentage', 3:10)

#Bind the tables
df <- rbind(taxable,retirement)

#factorize asset variable
df$asset <- factor(df$asset)

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
                    choices = c('taxable', 'retirement')),
        
         sliderInput(inputId = "risk_tolerance",
                     label = "Select Risk Tolerance",
                     min = 0.5,
                     max = 10,
                     value = 4, 
                     step = 0.5)
          ), # end of sidebarPanel
      
      # Show a plot of the generated distribution
      mainPanel(
         tableOutput(outputId = "folio_percentage_table"),
         plotOutput(outputId = "folio_allocation_plot")
      )# end of mainPanel
   ) # end of sidebarLayout
) #end of UI


# Define server logic required to draw a histogram
server <- function(input, output) {

#reactive data frame
folio_mix <- reactive({
  df %>% filter(`Investment Mix` == input$portfolio_type & 
                `Risk Tolerance` == input$risk_tolerance & 
                percentage !=0) %>%
    mutate(allocation = input$asset_value * percentage)
  })

#spread data frame for percentage table
folio_mix_spread <- reactive({folio_mix() %>%
    select(-c(`Risk Tolerance`, allocation)) %>%
    mutate(percentage = paste(percentage * 100, '%', sep = '')) %>%
    spread(key = asset, value = percentage)
})
#percentage table
output$folio_percentage_table <- renderTable({
      folio_mix_spread()
   })

#allocation plot
output$folio_allocation_plot <- renderPlot({
  ggplot(folio_mix(), aes(x  = asset, y = allocation, label = paste('$', allocation, sep = '')))+
    geom_bar(stat = "identity", aes(fill = asset)) +
    geom_label()
})

} #end of server function

# Run the application 
shinyApp(ui = ui, server = server)

