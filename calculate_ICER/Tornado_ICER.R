library(ggplot2)
library(dplyr)

##### Source the function for generating ICER for Tonardo plot
source("Function_Tornado_ICER.R")

##### Set discount_rate, which can be {0,0.015,0.03}
discount_rate <-0.0

##### Define parameter sets for base/lower/upper cost and effect data for Tornado plot
## Parameter = c("p.PrEP.preventRabies", "p.dog.bite", "p.rabidDog", "p.infected", "p.startPEP",
##             "p.noPrEP.alive", "p.PrEP.alive", "cost.PrEP", "cost.PEP1","cost.PEP2or3")

# parameters:                base / lower / upper
# "p.PrEP.preventRabies":     0.6 /  0    / 0.95
# "p.dog.bite":             0.001 /0.0001 / 0.01
# "p.rabidDog":               0.3 /  0.1. / 0.86
# "p.infected":  fix  at     0.19         
# "p.startPEP":               0.5 /  0.1  / 0.9
# "p.noPrEP.alive":          0.94 /  0.89 / 0.99 
# "p.PrEP.alive": fix at       1
# "cost.PrEP":                 5  /  2    / 45   (Unit:international dollar)
# "cost.PEP1":    fix at       0
# "cost.PEP2or3": fix at       0
# Note, "cost.PEP1" and "cost.PEP2or3" will refer to as cost.PEP, which is fix at 0

BaseCost <- rep(5,9)
# Repeat same parameters set for 9 times, as the BaseCost is = rep(5,9) 
# BaseEffect <-QALYs.cases.avoided.df
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.5, 0.94, 1, 5, 0, 0) ; BaseEffect_1to9 <-Tornado_ICER(parameters)
BaseEffect <- rep(BaseEffect_1to9,9) 

LowerCost = c(5,5,5,5,5,5,5,2,5)
parameters <- c(0, 0.001, 0.3, 0.19, 0.5, 0.94, 1, 5, 0, 0)    ; LowerEffect1 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.0001, 0.3, 0.19, 0.5, 0.94, 1, 5, 0, 0) ; LowerEffect2 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.1, 0.19, 0.5, 0.94, 1, 5, 0, 0)  ; LowerEffect3 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.5, 0.94, 1, 5, 0, 0)  ; LowerEffect4 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.1, 0.94, 1, 5, 0, 0)  ; LowerEffect5 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.5, 0.89, 1, 5, 0, 0)  ; LowerEffect6 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.5, 0.94, 1, 5, 0, 0)  ; LowerEffect7 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.5, 0.94, 1, 2, 0, 0)  ; LowerEffect8 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.5, 0.94, 1, 5, 0, 0)  ; LowerEffect9 <-Tornado_ICER(parameters)
LowerEffect <- c(LowerEffect1,LowerEffect2,LowerEffect3,LowerEffect4,LowerEffect5,LowerEffect6,LowerEffect7,LowerEffect8,LowerEffect9) 

UpperCost = c(5,5,5,5,5,5,5,45,5)
parameters <- c(0.95, 0.001, 0.3, 0.19, 0.5, 0.94, 1, 5, 0, 0) ; UpperEffect1 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.01, 0.3, 0.19, 0.5, 0.94, 1, 5, 0, 0)   ; UpperEffect2 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.86, 0.19, 0.5, 0.94, 1, 5, 0, 0) ; UpperEffect3 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.5, 0.94, 1, 5, 0, 0)  ; UpperEffect4 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.9, 0.94, 1, 5, 0, 0)  ; UpperEffect5 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.5, 0.99, 1, 5, 0, 0)  ; UpperEffect6 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.5, 0.94, 1, 5, 0, 0)  ; UpperEffect7 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.5, 0.94, 1, 45, 0, 0) ; UpperEffect8 <-Tornado_ICER(parameters)
parameters <- c(0.6, 0.001, 0.3, 0.19, 0.5, 0.94, 1, 5, 0, 0)  ; UpperEffect9 <-Tornado_ICER(parameters)
UpperEffect <- c(UpperEffect1,UpperEffect2,UpperEffect3,UpperEffect4,UpperEffect5,UpperEffect6,UpperEffect7,UpperEffect8,UpperEffect9) 


# create dataframe for result for base/lower/upper cost and effect data for Tornado plot
#parameters <- c(0.6, 0.001, 0.3, 0.19, 0.5, 0.94, 1, 5, 0, 0)  # base parameters
data.Tornado <- data.frame( 
  Parameter =  c("p.PrEP.preventRabies", "p.dog.bite", "p.rabidDog", "p.infected", "p.startPEP",
                   "p.noPrEP.alive", "p.PrEP.alive", "cost.PrEP", "cost.PEP"), BaseCost, BaseEffect, LowerCost, LowerEffect, UpperCost, UpperEffect)

# Calculate ICER values
data.Tornado <- data.Tornado %>%
  mutate(
    ICER.base = (BaseCost) / (BaseEffect)
  )
data.Tornado <- data.Tornado %>%
  mutate(
    ICER.lower = (LowerCost) / (LowerEffect)
  )
data.Tornado <- data.Tornado %>%
  mutate(
    ICER.upper = (UpperCost) / (UpperEffect)
  )

# Calculate different of ICER from the base
data.Tornado <- data.Tornado %>%
  mutate(
    ICER.diff.upper.to.base = ICER.upper - ICER.base
  )
data.Tornado <- data.Tornado %>%
  mutate(
    ICER.diff.lower.to.base = ICER.lower - ICER.base
  )


dataForTornado <- data.frame(
  variable=c("Pprevent3","Pbite","CostPrEP","Prabid", "PstartPEP", "Pprevent1"),  
  ## define name: High = Upper Limit = Increasing
  ## define name: Low = Lower Limit = Decreasing
  Level=c("Increasing", "Increasing", "Increasing","Increasing", "Increasing", "Increasing", "Decreasing","Decreasing","Decreasing","Decreasing","Decreasing","Decreasing"),
  ICER.dif= c(data.Tornado$ICER.diff.upper.to.base[c(1,2,8,3,5,6)],  # Upper Limit = Increasing
              data.Tornado$ICER.diff.lower.to.base[c(1,2,8,3,5,6)]), # Lower Limit = Decreasing
  ICER.base= rep(data.Tornado$ICER.base,12)                          # data.Tornado$ICER.base will vary corresponded to the discount_rate
)



