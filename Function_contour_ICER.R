################# Function to calculate ICERs and pathway details for a 5x5 decision matrix #################
## This function calculates the Incremental Cost-Effectiveness Ratios (ICERs) for two interventions: 
## (1) No PrEP and (2) PrEP with abbreviated PEP, for preventing rabies. It computes expected costs, cases, 
## and QALYs for each intervention by evaluating all decision tree pathways.

contour_ICER <-function(parameters) {
 
  # Number of samples (currently set to 1 for deterministic calculations, extend for probabilistic analysis)
  n_samples <- 1

  #### PROBABILISTIC PARAMETERS ####
  # Extracting input parameters from the 'parameters' argument for modeling rabies prevention.
  # These represent probabilities and costs in the decision tree model.
  p.PrEP.preventRabies <-parameters[1]    # Probability PrEP prevents rabies
  p.dog.bite <-parameters[2]              # Probability of a dog bite
  p.rabidDog <-parameters[3]              # Probability dog is rabid
  p.infected <-parameters[4]              # Probability of getting infected after a bite
  p.startPEP <-parameters[5]              # Probability of starting PEP post-bite
  p.noPrEP.alive <-parameters[6]          # Probability of survival in no PrEP pathway
  p.PrEP.alive <-parameters[7]            # Probability of survival in PrEP pathway
  cost.PrEP <-parameters[8]               # Cost of PrEP
  cost.PEP1 <-parameters[9]               # Cost of first dose of PEP
  cost.PEP2or3 <-parameters[10]           # Cost of subsequent doses of PEP

  ####**** Decision Tree Model ****####
  ## The model simulates rabies prevention over a 5-year time horizon for individuals aged 1 to 16.
  ## Each pathway is a combination of events in the decision tree. 
  ## Probabilities, costs, and cases are calculated for each pathway.
 
  #### No PrEP Scenario (8 pathways), (see Figure "Comparing the PEP-only to the PrEP plus PEP scenarios" @ README.md) ####
  # Pathway 1: Dog Bite -> Rabid Dog -> Infected -> Start PEP -> Alive
  noPrEP.path.1 <- p.dog.bite*15 * p.rabidDog * p.infected * p.startPEP * p.noPrEP.alive  # Probability 
  noPrEP.cost.1 <- cost.PEP1 * 1 + cost.PEP2or3 * 1                                       # Total PEP costs (2 or 3 doses)
  noPrEP.cases.1 <- rep(0,n_samples)                    # Whether death occurs in this pathway,  # i.e. (1 = death, 0 = alive) 

  # Pathway 2: Dog Bite -> Rabid Dog -> Infected -> Start PEP -> Death
  noPrEP.path.2 <- p.dog.bite*15 * p.rabidDog * p.infected * p.startPEP * (1-p.noPrEP.alive)
  noPrEP.cost.2 <- cost.PEP1 * 1 + cost.PEP2or3 * 1                                       # PEP costs
  noPrEP.cases.2 <- rep(1,n_samples)                                                      # Death due to rabies

  ### Repeat similar logic for all remaining pathways:
  # Pathway 3: Dog Bite -> Rabid Dog -> Infected -> No PEP -> Death
  noPrEP.path.3 <- p.dog.bite*15 * p.rabidDog * p.infected * (1-p.startPEP) 
  noPrEP.cost.3 <- rep(0,n_samples)
  noPrEP.cases.3 <- rep(1,n_samples)

  # Pathway 4: Dog Bite -> Rabid Dog -> Not Infected -> Start PEP -> Alive
  noPrEP.path.4 <- p.dog.bite*15 * p.rabidDog * (1-p.infected) * p.startPEP
  noPrEP.cost.4 <- cost.PEP1 * 1 + cost.PEP2or3 * 1
  noPrEP.cases.4 <- rep(0,n_samples)

  # Pathway 5: Dog Bite -> Rabid Dog -> Not Infected -> No PEP
  noPrEP.path.5 <- p.dog.bite*15 * p.rabidDog * (1-p.infected) * (1-p.startPEP)
  noPrEP.cost.5 <- rep(0,n_samples)
  noPrEP.cases.5 <- rep(0,n_samples)
  
  # Pathway 6: Dog Bite -> Non-Rabid Dog -> No Infection -> Start PEP
  noPrEP.path.6 <- p.dog.bite*15 * (1-p.rabidDog) * p.startPEP
  noPrEP.cost.6 <- cost.PEP1 * 1 + cost.PEP2or3 * 1
  noPrEP.cases.6 <- rep(0,n_samples)

  # Pathway 7: Dog Bite -> Non-Rabid Dog -> No Infection -> No PEP
  noPrEP.path.7 <- p.dog.bite*15 * (1-p.rabidDog) * (1-p.startPEP)
  noPrEP.cost.7 <- rep(0,n_samples)
  noPrEP.cases.7 <- rep(0,n_samples)

  # Pathway 8: No Dog Bite
  noPrEP.path.8 <- rep(1-p.dog.bite*15, n_samples)  #to set the size of cost vector = n_samples
  noPrEP.cost.8 <- rep(0,n_samples)
  noPrEP.cases.8 <- rep(0,n_samples)

  # Combine probabilities, costs, and cases across all pathways into dataframes
  noPrEP.probs.dataframe <- data.frame(noPrEP.path.1, noPrEP.path.2, noPrEP.path.3, noPrEP.path.4, noPrEP.path.5, noPrEP.path.6, noPrEP.path.7, noPrEP.path.8)
  noPrEP.costs.dataframe <- data.frame(noPrEP.cost.1, noPrEP.cost.2, noPrEP.cost.3, noPrEP.cost.4, noPrEP.cost.5, noPrEP.cost.6, noPrEP.cost.7, noPrEP.cost.8)
  noPrEP.cases.dataframe <- data.frame(noPrEP.cases.1, noPrEP.cases.2, noPrEP.cases.3, noPrEP.cases.4, noPrEP.cases.5, noPrEP.cases.6, noPrEP.cases.7, noPrEP.cases.8)
  

  #### Calculate expected costs and cases for No PrEP using probabilities and costs across pathways
  noPrEP.costs.df <- data.frame(matrix(ncol = ncol(noPrEP.costs.dataframe), nrow = n_samples))
  noPrEP.cases.df <- data.frame(matrix(ncol = ncol(noPrEP.cases.dataframe), nrow = n_samples))
  noPrEP.costs.total.df <- data.frame(matrix(ncol = 1, nrow = n_samples))
  noPrEP.cases.total.df <- data.frame(matrix(ncol = 1, nrow = n_samples))
  
  for (i in 1:n_samples) {
    noPrEP.costs.df[i,] <-  noPrEP.probs.dataframe[i,] * noPrEP.costs.dataframe[i,]   ## expected vcosts for each pathway (hint: use vector multiplication)
    noPrEP.cases.df[i,] <-  noPrEP.probs.dataframe[i,] * noPrEP.cases.dataframe[i,]   ## expected cases for each pathway (hint: use vector multiplication)
  }

  # Total costs and cases for No PrEP
  noPrEP.costs.total.df <- rowSums(noPrEP.costs.df)      ## total costs (hint: use rowSums()) for each n_samples (simulation)
  noPrEP.cases.total.df <- rowSums(noPrEP.cases.df)      ## total cases (hint: use rowSums()) for each n_samples (simulation)

  # Combine costs and cases into a results dataframe for No PrEP
  noPrEP.results.df <- (data.frame(costs = noPrEP.costs.total.df, cases = noPrEP.cases.total.df))  

  
  #### PrEP Scenario (9 pathways) ####
  ## Similar logic is applied to calculate probabilities, costs, and cases for each of the 9 pathways under the PrEP scenario.
  
  # Pathway 1: Dog Bite -> Rabid Dog -> Infected -> Start PEP -> Alive
  PrEP.path.1 <- p.dog.bite*15 * p.rabidDog * p.infected * p.startPEP * p.PrEP.alive      # Probability 
  PrEP.cost.1 <- cost.PrEP * 1 + cost.PEP1 * 1 + cost.PEP2or3 * 1                         # PrEP + PEP costs
  PrEP.cases.1 <- rep(0,n_samples)                                                        # Alive

  ## Repeat similar logic for the rest of the pathways.
  # Pathway 2: Dog Bite -> Rabid Dog -> Infected -> Start PEP -> Death
  PrEP.path.2 <- p.dog.bite*15 * p.rabidDog * p.infected * p.startPEP * (1-p.PrEP.alive)
  PrEP.cost.2 <- cost.PrEP * 1 + cost.PEP1 * 1 + cost.PEP2or3 * 1
  PrEP.cases.2 <- rep(1,n_samples)
  
  # Pathway 3: Dog Bite -> Rabid Dog -> Infected -> noPEP -> Alive
  PrEP.path.3 <- p.dog.bite*15 * p.rabidDog * p.infected * (1-p.startPEP) * p.PrEP.preventRabies
  PrEP.cost.3 <- cost.PrEP * 1 
  PrEP.cases.3 <- rep(0,n_samples)
  
  # Pathway 4: Dog Bite -> Rabid Dog -> Infected -> noPEP -> Death
  PrEP.path.4 <- p.dog.bite*15 * p.rabidDog * p.infected * (1-p.startPEP) * (1-p.PrEP.preventRabies)
  PrEP.cost.4 <- cost.PrEP * 1 
  PrEP.cases.4 <- rep(1,n_samples)
  
  # Pathway 5: Dog Bite -> RabidDog -> No Infected -> StartPEP
  PrEP.path.5 <- p.dog.bite*15 * p.rabidDog * (1-p.infected) * p.startPEP
  PrEP.cost.5 <- cost.PrEP * 1 + cost.PEP1 * 1 + cost.PEP2or3 * 1
  PrEP.cases.5 <- rep(0,n_samples)
  
  # Pathway 6: Dog Bite -> Rabid Dog -> No Infected -> noPEP
  PrEP.path.6 <- p.dog.bite*15 * p.rabidDog * (1-p.infected) * (1-p.startPEP)
  PrEP.cost.6 <- cost.PrEP * 1
  PrEP.cases.6 <- rep(0,n_samples)
  
  # Pathway 7: Dog Bite -> No Rabid Dog -> No Infected -> StartPEP
  PrEP.path.7 <- p.dog.bite*15 * (1-p.rabidDog) * p.startPEP
  PrEP.cost.7 <- cost.PrEP * 1 + cost.PEP1 * 1 + cost.PEP2or3 * 1
  PrEP.cases.7 <- rep(0,n_samples)
  
  # Pathway 8: Dog Bite -> No Rabid Dog -> No Infected -> no PEP
  PrEP.path.8 <- p.dog.bite*15 * (1-p.rabidDog) * (1-p.startPEP)
  PrEP.cost.8 <- cost.PrEP * 1
  PrEP.cases.8 <- rep(0,n_samples)
  
  # Pathway 9: No Dog Bite
  PrEP.path.9 <- rep(1-p.dog.bite*15, n_samples) # to set the size of cost vector = n_samples
  PrEP.cost.9 <- rep(cost.PrEP * 1,n_samples)    # to set the size of cost vector = n_samples
  PrEP.cases.9 <- rep(0,n_samples)

  # Combine probabilities, costs, and cases across all pathways into dataframes for PrEP
  PrEP.probs.dataframe <- data.frame(PrEP.path.1, PrEP.path.2, PrEP.path.3, PrEP.path.4, PrEP.path.5, PrEP.path.6, PrEP.path.7, PrEP.path.8, PrEP.path.9)
  PrEP.costs.dataframe <- data.frame(PrEP.cost.1, PrEP.cost.2, PrEP.cost.3, PrEP.cost.4, PrEP.cost.5, PrEP.cost.6, PrEP.cost.7, PrEP.cost.8, PrEP.cost.9)
  PrEP.cases.dataframe <- data.frame(PrEP.cases.1, PrEP.cases.2, PrEP.cases.3, PrEP.cases.4, PrEP.cases.5, PrEP.cases.6, PrEP.cases.7, PrEP.cases.8, PrEP.cases.9)
  
  #### Calculate expected costs and cases for PrEP using probabilities and costs across pathways
  PrEP.costs.df <- data.frame(matrix(ncol = ncol(PrEP.costs.dataframe), nrow = n_samples))
  PrEP.cases.df <- data.frame(matrix(ncol = ncol(PrEP.cases.dataframe), nrow = n_samples))
  PrEP.costs.total.df <- data.frame(matrix(ncol = 1, nrow = n_samples))
  PrEP.cases.total.df <- data.frame(matrix(ncol = 1, nrow = n_samples))
  
  for (i in 1:n_samples) {
    PrEP.costs.df[i,] <-  PrEP.probs.dataframe[i,] * PrEP.costs.dataframe[i,]   ## expected costs for each pathway (hint: use vector multiplication)
    PrEP.cases.df[i,] <-  PrEP.probs.dataframe[i,] * PrEP.cases.dataframe[i,]   ## expected cases for each pathway (hint: use vector multiplication)
  }

  # Total costs and cases for PrEP
  PrEP.costs.total.df <- rowSums(PrEP.costs.df)      ## total costs (hint: use rowSums()) for each n_samples (simulation)
  PrEP.cases.total.df <- rowSums(PrEP.cases.df)      ## total cases (hint: use rowSums()) for each n_samples (simulation)
  
  # Combine costs and cases into a results dataframe for PrEP
  PrEP.results.df <- (data.frame(costs = PrEP.costs.total.df, cases = PrEP.cases.total.df))  

  
 
  ####**** Incremental Analysis ****#### 
  # Derive the appropriate parameters assigned above to estimate the cases avoided, 
  # the incremental costs and the cost per rabies-infected death avoided
  
  # Calculate cases avoided with PrEP compared to No PrEP
  cases.avoided.df <- noPrEP.results.df$cases - PrEP.results.df$cases

  # QALYs gained with PrEP using a 50-year time horizon and applying a discount rate

  
  # Gain in QALYs with PrEP+abbreviatedPEP and PEP
  discount_rate <-0.0  ## Discount rate can vary  discount_rate = {0,0.015,0.03}
  QALYs.cases.avoided.discount0.df <- ((cases.avoided.df * 1/(1+discount_rate)^(1:50)) ) # using 50 years
  QALYs.cases.avoided.discount0.df.sum <-sum(QALYs.cases.avoided.discount0.df)
  QALYs.cases.avoided.df <-QALYs.cases.avoided.discount0.df.sum
  
  # Calculate incremental costs (difference in total costs between PrEP and No PrEP)
  incr.costs.df <- PrEP.results.df$costs - noPrEP.results.df$costs 
  # Calculate ICER
  incremental.results.df <- incr.costs.df/QALYs.cases.avoided.df
  return(incremental.results.df)
  print(incremental.results.df)
}

################# END function "contour_ICER" #################

