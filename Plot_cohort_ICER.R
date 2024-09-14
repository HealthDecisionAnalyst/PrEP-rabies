#### Individual plot of the impact of varying each key parameter in a one-way sensitivity analysis ####
# Figure 3 in the paper

library(ggplot2)
library(dplyr)
# call function cohort_ICER
##### Source the function for generating ICER for Tonardo plot
source("Function_cohort_ICER.R")

cohort_incremental <-cohort_ICER(parameters)
#### Individual plot of the impact of varying each key parameter in a one-way sensitivity analysis ####
# Figure 3 in the paper
par(mfrow=c(2,3)) 
plot(p.PrEP.preventRabies_ICERs[,2],p.PrEP.preventRabies_ICERs[,3], ylab="ICERs", xlab="Pprevent3", cex = 0.25, cex.lab=1.25)
plot(p.dog.bite_ICERs[,2],p.dog.bite_ICERs[,3], ylab="ICERs", xlab="Pbite", cex = 0.25, cex.lab=1.25)
plot(cost.PrEP_ICERs[,2],cost.PrEP_ICERs[,3], ylab="ICERs", xlab="CostPrEP", cex = 0.25, cex.lab=1.25)
plot(p.rabidDog_ICERs[,2],p.rabidDog_ICERs[,3], ylab="ICERs", xlab="Prabid", cex = 0.25, cex.lab=1.25)
plot(p.startPE_ICERs[,2],p.startPE_ICERs[,3], ylab="ICERs", xlab="PstartPEP", cex = 0.25, cex.lab=1.25)
plot(p.noPrEP.alive_ICERs[,2],p.noPrEP.alive_ICERs[,3], ylab="ICERs", xlab="Pprevent1", cex = 0.25, cex.lab=1.25)

### END Individual plot ####

