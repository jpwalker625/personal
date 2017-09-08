library(tidyverse)
library(purrr)
#Compound Interest Equation

##equation variables
##will need to develop a data frame that calculates the compound interest for an 
##interval between 1:n years 
# V = P(1+r/n)(nt)

# V = the future value of the investment

# P = the principal investment amount
p <- 1000

# r = the annual interest rate
r <- .20

# n = the number of times that interest is compounded per year
n <- 365

# t = the number of years the money is invested for
t <- 10

years <- 1:t
returns <- 0


for(i in 1:t){
    v <- p*(1+r/n)^(n*i)
    
    returns[i] <- v
  }

returns <- bind_cols(returns = returns, years = years)


compound_interst <- function(p,r,n,t){
  v <- p*(1+r/n)^(n*t)
  return(v)
}


a <- pmap(.f = compound_interst, .l = list(p,r,n, 1:t))

a <- unlist(a)
