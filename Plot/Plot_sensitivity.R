#### Generate individual plots to illustrate the impact of varying key parameters in a one-way sensitivity analysis.
#### This provides insights into how different inputs affect the Incremental Cost-Effectiveness Ratios (ICERs).

# Set the layout to display 6 plots in a 2x3 grid
par(mfrow=c(2,3)) 

# Plot 1: Effect of varying "p.PrEP.preventRabies" on ICER
plot(cohort_incremental_for_p.PrEP.preventRabies$p.PrEP.preventRabies,
     cohort_incremental_for_p.PrEP.preventRabies$cohort.incremental.results.df, 
     ylab="ICERs", 
     xlab="Pprevent3", 
     cex = 0.25, 
     cex.lab=1.25)

# Plot 2: Effect of varying "p.dog.bite" on ICER
plot(cohort_incremental_for_p.dog.bite$p.dog.bite,
     cohort_incremental_for_p.dog.bite$cohort.incremental.results.df, 
     ylab="ICERs", 
     xlab="Pbite", 
     cex = 0.25, 
     cex.lab=1.25)

# Plot 3: Effect of varying "cost.PrEP" on ICER
plot(cohort_incremental_for_cost.PrEP$cost.PrEP,
     cohort_incremental_for_cost.PrEP$cohort.incremental.results.df, 
     ylab="ICERs", 
     xlab="CostPrEP", 
     cex = 0.25, 
     cex.lab=1.25)

# Plot 4: Effect of varying "p.rabidDog" on ICER
plot(cohort_incremental_for_p.rabidDog$p.rabidDog,
     cohort_incremental_for_p.rabidDog$cohort.incremental.results.df, 
     ylab="ICERs", 
     xlab="Prabid", 
     cex = 0.25, 
     cex.lab=1.25)

# Plot 5: Effect of varying "p.startPEP" on ICER
plot(cohort_incremental_for_p.startPEP$p.startPEP,
     cohort_incremental_for_p.startPEP$cohort.incremental.results.df, 
     ylab="ICERs", 
     xlab="PstartPEP", 
     cex = 0.25, 
     cex.lab=1.25)

# Plot 6: Effect of varying "p.noPrEP.alive" on ICER
plot(cohort_incremental_for_p.noPrEP.alive$p.noPrEP.alive,
     cohort_incremental_for_p.noPrEP.alive$cohort.incremental.results.df, 
     ylab="ICERs", 
     xlab="Pprevent1", 
     cex = 0.25, 
     cex.lab=1.25)

#### Note:
# To analyze the impact for different discount rates (e.g., 0.0%, 1.5%, 3.0%), 
# re-run the Sensitivity_ICER.R script with the desired value for the discount_rate variable
