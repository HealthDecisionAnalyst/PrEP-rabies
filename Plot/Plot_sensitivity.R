#### Individual plots of the impact of varying each key parameter in a one-way sensitivity analysis. 
par(mfrow=c(2,3)) 
plot(cohort_incremental_for_p.PrEP.preventRabies$p.PrEP.preventRabies,cohort_incremental_for_p.PrEP.preventRabies$cohort.incremental.results.df, ylab="ICERs", xlab="Pprevent3", cex = 0.25, cex.lab=1.25)
plot(cohort_incremental_for_p.dog.bite$p.dog.bite,cohort_incremental_for_p.dog.bite$cohort.incremental.results.df, ylab="ICERs", xlab="Pbite", cex = 0.25, cex.lab=1.25)
plot(cohort_incremental_for_cost.PrEP$cost.PrEP,cohort_incremental_for_cost.PrEP$cohort.incremental.results.df, ylab="ICERs", xlab="CostPrEP", cex = 0.25, cex.lab=1.25)
plot(cohort_incremental_for_p.rabidDog$p.rabidDog,cohort_incremental_for_p.rabidDog$cohort.incremental.results.df, ylab="ICERs", xlab="Prabid", cex = 0.25, cex.lab=1.25)
plot(cohort_incremental_for_p.startPEP$p.startPEP,cohort_incremental_for_p.startPEP$cohort.incremental.results.df, ylab="ICERs", xlab="PstartPEP", cex = 0.25, cex.lab=1.25)
plot(cohort_incremental_for_p.noPrEP.alive$p.noPrEP.alive,cohort_incremental_for_p.noPrEP.alive$cohort.incremental.results.df, ylab="ICERs", xlab="Pprevent1", cex = 0.25, cex.lab=1.25)
# to plot for each DISCOUNTIND RATE = {0.0, 1.5, 3.0}, one need to re-run the Sensitivity_ICER.R using different value of discount_rate  

