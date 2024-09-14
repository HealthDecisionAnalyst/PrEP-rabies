library(ggplot2)
library(dplyr)

##### Source the function for generating ICER for Tonardo plot
source("Function_Tornado_ICER.R")

##### Set discount_rate, which can be one of {0,0.015,0.03}
discount_rate <-0.0           # No discounting in this case


##### Define the base, lower, and upper values for parameters used in the Tornado plot analysis
# Parameters included:
# - p.PrEP.preventRabies: Probability that PrEP prevents rabies (Base: 0.6, Range: 0 to 0.95)
# - p.dog.bite: Probability of a dog bite (Base: 0.001, Range: 0.0001 to 0.01)
# - p.rabidDog: Probability that the dog is rabid (Base: 0.3, Range: 0.1 to 0.86)
# - p.infected: Fixed probability of infection after a bite (Fixed at 0.19)
# - p.startPEP: Probability of starting PEP after exposure (Base: 0.5, Range: 0.1 to 0.9)
# - p.noPrEP.alive: Probability of survival without PrEP (Base: 0.94, Range: 0.89 to 0.99)
# - p.PrEP.alive: Fixed probability of survival with PrEP (Fixed at 1)
# - cost.PrEP: Cost of PrEP intervention (Base: 5, Range: 2 to 45, in US dollars)
# - cost.PEP1 and cost.PEP2or3: Both are fixed at 0 and treated as cost.PEP

## Parameter = c("p.PrEP.preventRabies", "p.dog.bite", "p.rabidDog", "p.infected", "p.startPEP",
##             "p.noPrEP.alive", "p.PrEP.alive", "cost.PrEP", "cost.PEP1","cost.PEP2or3")

##### Base scenario: Costs and effects with base parameter values
# BaseCost: Repeated base cost of PrEP for all 9 parameters
BaseCost <- rep(5,9)

# Parameters for base effectiveness calculations
# BaseEffect <-QALYs.cases.avoided.df
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.5, 0.94, 1, 5, 0, 0) ; BaseEffect_1to9 <-Tornado_ICER(parameters)
BaseEffect <- rep(BaseEffect_1to9,9) 

##### Lower scenario: Costs and effects with lower-bound parameter values
LowerCost = c(5,5,5,5,5,5,5,2,5)                               # Lower cost for PrEP at its minimum
# Running Tornado_ICER function with various lower-bound parameter sets
parameters <- c(0, 0.001, 0.3, 0.19, 0.5, 0.94, 1, 5, 0, 0)    ; LowerEffect1 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.0001, 0.3, 0.19, 0.5, 0.94, 1, 5, 0, 0) ; LowerEffect2 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.1, 0.19, 0.5, 0.94, 1, 5, 0, 0)  ; LowerEffect3 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.5, 0.94, 1, 5, 0, 0)  ; LowerEffect4 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.1, 0.94, 1, 5, 0, 0)  ; LowerEffect5 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.5, 0.89, 1, 5, 0, 0)  ; LowerEffect6 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.5, 0.94, 1, 5, 0, 0)  ; LowerEffect7 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.5, 0.94, 1, 2, 0, 0)  ; LowerEffect8 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.5, 0.94, 1, 5, 0, 0)  ; LowerEffect9 <-Tornado_ICER(parameters)
# Combine lower-bound effects for all scenarios
LowerEffect <- c(LowerEffect1,LowerEffect2,LowerEffect3,LowerEffect4,LowerEffect5,LowerEffect6,LowerEffect7,LowerEffect8,LowerEffect9) 

##### Upper scenario: Costs and effects with upper-bound parameter values
UpperCost = c(5,5,5,5,5,5,5,45,5)                              # Upper cost for PrEP at its maximum
# Running Tornado_ICER function with various upper-bound parameter sets
parameters <- c(0.95, 0.001, 0.3, 0.19, 0.5, 0.94, 1, 5, 0, 0) ; UpperEffect1 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.01, 0.3, 0.19, 0.5, 0.94, 1, 5, 0, 0)   ; UpperEffect2 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.86, 0.19, 0.5, 0.94, 1, 5, 0, 0) ; UpperEffect3 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.5, 0.94, 1, 5, 0, 0)  ; UpperEffect4 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.9, 0.94, 1, 5, 0, 0)  ; UpperEffect5 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.5, 0.99, 1, 5, 0, 0)  ; UpperEffect6 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.5, 0.94, 1, 5, 0, 0)  ; UpperEffect7 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.5, 0.94, 1, 45, 0, 0) ; UpperEffect8 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.5, 0.94, 1, 5, 0, 0)  ; UpperEffect9 <-Tornado_ICER(parameters)
# Combine upper-bound effects for all scenarios
UpperEffect <- c(UpperEffect1,UpperEffect2,UpperEffect3,UpperEffect4,UpperEffect5,UpperEffect6,UpperEffect7,UpperEffect8,UpperEffect9) 

##### Create a dataframe with the base, lower, and upper cost and effect values for Tornado plot
# parameters <- c(0.6, 0.001, 0.3, 0.19, 0.5, 0.94, 1, 5, 0, 0)  # base parameters
data.Tornado <- data.frame( 
  Parameter =  c("p.PrEP.preventRabies", "p.dog.bite", "p.rabidDog", "p.infected", "p.startPEP",
                   "p.noPrEP.alive", "p.PrEP.alive", "cost.PrEP", "cost.PEP"), BaseCost, BaseEffect, LowerCost, LowerEffect, UpperCost, UpperEffect)

##### Calculate ICER (Incremental Cost-Effectiveness Ratio) for base, lower, and upper scenarios
data.Tornado <- data.Tornado %>%
  mutate(
    ICER.base = (BaseCost) / (BaseEffect),
    ICER.lower = (LowerCost) / (LowerEffect),
    ICER.upper = (UpperCost) / (UpperEffect)
  )

##### Calculate the difference in ICER between upper/lower and base scenarios
data.Tornado <- data.Tornado %>%
  mutate(
    ICER.diff.upper.to.base = ICER.upper - ICER.base,
    ICER.diff.lower.to.base = ICER.lower - ICER.base
  )

##### Create a dataframe for plotting the Tornado plot
dataForTornado <- data.frame(
  variable=c("Pprevent3","Pbite","CostPrEP","Prabid", "PstartPEP", "Pprevent1"),  
  ## define name: High = Upper Limit = Increasing
  ## define name: Low = Lower Limit = Decreasing
  Level=c("Increasing", "Increasing", "Increasing","Increasing", "Increasing", "Increasing", "Decreasing","Decreasing","Decreasing","Decreasing","Decreasing","Decreasing"),
  ICER.dif= c(data.Tornado$ICER.diff.upper.to.base[c(1,2,8,3,5,6)],  # Upper Limit = Increasing
              data.Tornado$ICER.diff.lower.to.base[c(1,2,8,3,5,6)]), # Lower Limit = Decreasing
  ICER.base= rep(data.Tornado$ICER.base,12)                          # Base ICER values (varies with discount rate)
)



