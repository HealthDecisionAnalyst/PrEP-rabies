################# Function for Sensitivity Analysis and ICER Pathway Plot #################
# This function calculates the Incremental Cost-Effectiveness Ratios (ICERs) for two rabies prevention interventions: 
# (1) No PrEP (pre-exposure prophylaxis)
# (2) PrEP with abbreviated PEP (post-exposure prophylaxis).
# It computes expected costs, cases, and quality-adjusted life years (QALYs) for each intervention 
# by evaluating all possible pathways in the decision tree.

# Define the number of samples for sensitivity analysis (used in the plots)
n_samples <- 1000   ### for individual Tornado plot
#### PROBABLISTIC PARAMETERS #####
# Testing a flat distribution for "p.PrEP.preventRabies" between 0 and 0.95
p.PrEP.preventRabies  <- runif(n = n_samples, min = 0.0, max= 0.95)             
# Define fixed parameter values for other model inputs
p.dog.bite <- 0.001; p.rabidDog <- 0.3; p.infected <- 0.19; p.startPEP <- 0.5; p.noPrEP.alive <- 0.94; p.PrEP.alive <- 1; cost.PrEP <-5; cost.PEP1 <- 0; cost.PEP2or3 <- 0
# Run sensitivity analysis for varying "p.PrEP.preventRabies"
cohort_incremental_for_p.PrEP.preventRabies <-sensitivity_ICER()

#### PROBABLISTIC PARAMETERS #####
# Testing a flat distribution for "p.dog.bite" between 0.0001 and 0.01
p.dog.bite <- runif(n = n_samples, min = 0.0001, max= 0.01)                    
# Define fixed parameter values for other model inputs
p.PrEP.preventRabies  <- 0.6; p.rabidDog <- 0.3; p.infected <- 0.19; p.startPEP <- 0.5; p.noPrEP.alive <- 0.94; p.PrEP.alive <- 1; cost.PrEP <-5; cost.PEP1 <- 0; cost.PEP2or3 <- 0
# Run sensitivity analysis for varying "p.dog.bite"
cohort_incremental_for_p.dog.bite <-sensitivity_ICER()

#### PROBABLISTIC PARAMETERS #####
# Testing a flat distribution for "p.rabidDog" between 0.1 and 0.86
p.rabidDog <- runif(n = n_samples, min = 0.1, max= 0.86)                        
# Define fixed parameter values for other model inputs
p.PrEP.preventRabies  <- 0.6; p.dog.bite <- 0.001; p.infected <- 0.19; p.startPEP <- 0.5; p.noPrEP.alive <- 0.94; p.PrEP.alive <- 1; cost.PrEP <-5; cost.PEP1 <- 0; cost.PEP2or3 <- 0
# Run sensitivity analysis for varying "p.rabidDog"
cohort_incremental_for_p.rabidDog <-sensitivity_ICER()

#### PROBABLISTIC PARAMETERS #####
# Testing a flat distribution for "p.startPEP" between 0.1 and 0.9
p.startPEP <- runif(n = n_samples, min = 0.1, max= 0.9)                         
# Define fixed parameter values for other model inputs
p.PrEP.preventRabies  <- 0.6; p.dog.bite <- 0.001; p.infected <- 0.19; p.rabidDog <- 0.3; p.noPrEP.alive <- 0.94; p.PrEP.alive <- 1; cost.PrEP <-5; cost.PEP1 <- 0; cost.PEP2or3 <- 0
# Run sensitivity analysis for varying "p.startPEP"
cohort_incremental_for_p.startPEP <-sensitivity_ICER()

#### PROBABLISTIC PARAMETERS #####
# Testing a flat distribution for "p.noPrEP.alive" between 0.89 and 0.99
p.noPrEP.alive <- runif(n = n_samples, min = 0.89, max= 0.99)                   
# Define fixed parameter values for other model inputs
p.PrEP.preventRabies  <- 0.6; p.dog.bite <- 0.001; p.infected <- 0.19; p.rabidDog <- 0.3; p.startPEP <- 0.5; p.PrEP.alive <- 1; cost.PrEP <-5; cost.PEP1 <- 0; cost.PEP2or3 <- 0
# Run sensitivity analysis for varying "p.noPrEP.alive"
cohort_incremental_for_p.noPrEP.alive <-sensitivity_ICER()

#### PROBABLISTIC PARAMETERS #####
# Testing a flat distribution for "cost.PrEP" between 2 and 45
cost.PrEP <- runif(n = n_samples, min = 2, max= 45)                             
# Define fixed parameter values for other model inputs
p.PrEP.preventRabies  <- 0.6; p.dog.bite <- 0.001; p.infected <- 0.19; p.rabidDog <- 0.3; p.startPEP <- 0.5; p.PrEP.alive <- 1; p.noPrEP.alive <-0.94; cost.PEP1 <- 0; cost.PEP2or3 <- 0
# Run sensitivity analysis for varying "cost.PrEP"
cohort_incremental_for_cost.PrEP <-sensitivity_ICER()

 
