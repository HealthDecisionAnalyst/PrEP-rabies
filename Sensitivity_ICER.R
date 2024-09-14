
################# Function to calculate sensitivity of ICERs and pathway details plot #################
## This function calculates the Incremental Cost-Effectiveness Ratios (ICERs) for two interventions: 
## (1) No PrEP and (2) PrEP with abbreviated PEP, for preventing rabies. It computes expected costs, cases, 
## and QALYs for each intervention by evaluating all decision tree pathways.

# Number of samples  
n_samples <- 1000   ### for individual Tornado plot
#### PROBABLISTIC PARAMETERS #####
p.PrEP.preventRabies  <- runif(n = n_samples, min = 0.0, max= 0.95)             # test flat distribution for "p.PrEP.preventRabies"
p.dog.bite <- 0.001; p.rabidDog <- 0.3; p.infected <- 0.19; p.startPEP <- 0.5; p.noPrEP.alive <- 0.94; p.PrEP.alive <- 1; cost.PrEP <-5; cost.PEP1 <- 0; cost.PEP2or3 <- 0
cohort_incremental_for_p.PrEP.preventRabies <-sensitivity_ICER()

#### PROBABLISTIC PARAMETERS #####
p.dog.bite <- runif(n = n_samples, min = 0.0001, max= 0.01)                     # test flat distribution for "p.dog.bite"
p.PrEP.preventRabies  <- 0.6; p.rabidDog <- 0.3; p.infected <- 0.19; p.startPEP <- 0.5; p.noPrEP.alive <- 0.94; p.PrEP.alive <- 1; cost.PrEP <-5; cost.PEP1 <- 0; cost.PEP2or3 <- 0
cohort_incremental_for_p.dog.bite <-sensitivity_ICER()

#### PROBABLISTIC PARAMETERS #####
p.rabidDog <- runif(n = n_samples, min = 0.1, max= 0.86)                        # test flat distribution for "p.rabidDog"
p.PrEP.preventRabies  <- 0.6; p.dog.bite <- 0.001; p.infected <- 0.19; p.startPEP <- 0.5; p.noPrEP.alive <- 0.94; p.PrEP.alive <- 1; cost.PrEP <-5; cost.PEP1 <- 0; cost.PEP2or3 <- 0
cohort_incremental_for_p.rabidDog <-sensitivity_ICER()

#### PROBABLISTIC PARAMETERS #####
p.startPEP <- runif(n = n_samples, min = 0.1, max= 0.9)                         # test flat distribution for "p.startPEP"
p.PrEP.preventRabies  <- 0.6; p.dog.bite <- 0.001; p.infected <- 0.19; p.rabidDog <- 0.3; p.noPrEP.alive <- 0.94; p.PrEP.alive <- 1; cost.PrEP <-5; cost.PEP1 <- 0; cost.PEP2or3 <- 0
cohort_incremental_for_p.startPEP <-sensitivity_ICER()

#### PROBABLISTIC PARAMETERS #####
p.noPrEP.alive <- runif(n = n_samples, min = 0.89, max= 0.99)                   # test flat distribution for "p.noPrEP.alive"
p.PrEP.preventRabies  <- 0.6; p.dog.bite <- 0.001; p.infected <- 0.19; p.rabidDog <- 0.3; p.startPEP <- 0.5; p.PrEP.alive <- 1; cost.PrEP <-5; cost.PEP1 <- 0; cost.PEP2or3 <- 0
cohort_incremental_for_p.noPrEP.alive <-sensitivity_ICER()

#### PROBABLISTIC PARAMETERS #####
cost.PrEP <- runif(n = n_samples, min = 2, max= 45)                             # test flat distribution for "cost.PrEP"
p.PrEP.preventRabies  <- 0.6; p.dog.bite <- 0.001; p.infected <- 0.19; p.rabidDog <- 0.3; p.startPEP <- 0.5; p.PrEP.alive <- 1; noPrEP.alive <-0.94; cost.PEP1 <- 0; cost.PEP2or3 <- 0
cohort_incremental_for_cost.PrEP <-sensitivity_ICER()

# 
