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
library(purrr)
library(gdata)
library(plotly)

#########################################
##### Portfolio Rebalance Variables #####
#########################################

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

#######################################
##### Compound Interest Variables #####
#######################################

compound_df <- data_frame(rate = c("daily", "monthly", "quarterly", "bi-annually", "annually"),
                          n = c(365, 12, 4, 2, 1))


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Portfolio Rebalance Tool"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        h3("Portfolio Rebalance"),
        
        numericInput(inputId = "asset_value", 
                     label = "Investment Balance",
                     value = 10000),
        
        selectInput(inputId = "portfolio_type",
                    label = "Select Portfolio Type", 
                    choices = c('taxable', 'retirement')
                    ),
        
        sliderInput(inputId = "risk_tolerance",
                     label = "Select Risk Tolerance",
                     min = 0.5,
                     max = 10,
                     value = 4, 
                     step = 0.5),
        hr(),
        
        h3("Compound Interest Calculator"),
      
        numericInput(inputId = 'principal_investment',
                     label = "Input Principal Amount",
                     value = 1000),
        
        sliderInput(inputId = "annual_interest_rate",
                     label = "Annual Interest Rate (%)",
                     value = 10,
                     min = 1, 
                     max = 100),
        
        numericInput(inputId = "investment_period",
                    label = "Investment Period (years)", 
                    value = 5
                    ),
        
        selectInput(inputId = "compound_interval",
                    label = "Compound Interval",
                    choices = c("daily", "monthly", "quarterly", "bi-annually", "annually"),
                    selected = "Annually")
        
        ), #end of sidebarPanel
      
      # Show visualizations
      mainPanel(
         tableOutput(outputId = "folio_percentage_table"),
         
         plotOutput(outputId = "folio_allocation_plot"),
         
         hr(),
         
         plotlyOutput(outputId = "return_rate_plot"),
         
         tableOutput(outputId = "temp_table")
         
      )# end of mainPanel
   ) # end of sidebarLayout
) #end of UI


#Define server logic
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
  folio_mix() %>%
  ggplot(aes(x = asset, y = allocation, label = paste('$', allocation, sep = ''))) +
    geom_bar(stat = "identity", aes(fill = asset)) +
    geom_label() +
    scale_y_continuous(labels = scales::dollar) +
    labs(title = "Portfolio Allocation", x = "Asset", y = "Allocation (in U.S. Dollars)")
})

#Compound Interest Variables & Equation

#p = the principal investment amount
p <- reactive({input$principal_investment})

#r = the annual interest rate
r <- reactive({input$annual_interest_rate/100})

#n = the number of times that interest is compounded per year
n <- reactive({
  compound_df %>% 
    filter(rate == input$compound_interval) %>% 
    select(n)
  })
  
#t = the number of years the money is invested for
t <-  reactive({input$investment_period})

# compound_interst <- function(p,r,n,t){
#   v <- p*(1+r/n)^(n*t)
#   return(v)
# }
  
# compound_interest <- reactive({
#   for(i in 1:t()){
#  v <- print(p()*(1+r()/n())^(n()*i))
#   returns <- v
#   returns <- c(returns, v)
#   }
# })

#compound_interest equation

compound_interest <- function(p,r,n,t){
  return_arr = c()
  for(i in 1:t){
   v <- p*(1+r/n)^(n*i)
   return_arr <- c(return_arr, v)
  }
  return(return_arr)
  }

#to be matched with the compound interest rates
years <- reactive({1:t()})

#execute reactive compound_interest function
returns <- reactive({compound_interest(p(),r(),n(),t())})

returns_df <- reactive({bind_cols(returns = returns(), years = years())})

output$temp_table <- renderTable({returns_df()})

output$return_rate_plot <- renderPlotly({
   plot_ly(returns_df(), x = ~years, y = ~returns, 
           mode = 'lines+markers', 
           text = ~paste('Value after', years(), 'years: ','$', round(returns(),digits = 2))) %>%
             layout(title = 'The Power of Compounding Interest', 
                    xaxis = list(title = 'Investment Period'),
                    yaxis = list(title = "Value (in U.S. Dollars"))
})    

# output$return_rate_plot <- renderPlot({
#   returns_df() %>% 
#     ggplot(aes(x = years, y = returns)) +
#     geom_point() +
#     geom_line() +
#     scale_y_continuous(labels = scales::dollar) +
#     scale_x_continuous(breaks = 1:t()) +
#     labs(title = "The Power of Compounding Interest", 
#           x = "Investment Period", 
#           y = "Value (in U.S. Dollars)")


} #end of server function

# Run the application 
shinyApp(ui = ui, server = server)
