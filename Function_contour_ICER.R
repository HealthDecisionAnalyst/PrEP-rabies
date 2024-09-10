################# function to get all ICERs of 5x5 (and detail of 5x5 for each contour) #################
# Using function "contour_ICER" to create a vector of each of the probabilities, costs and cases for the pathways

contour_ICER <-function(parameters) {
  
  # Number of samples  
  n_samples <- 1
  
  #### PROBABLISTIC PARAMETERS #####
  # First, assign the appropriate distribution and key values to the parameters below.  
  # Probabilities 
  p.PrEP.preventRabies <-parameters[1]
  p.dog.bite <-parameters[2]
  p.rabidDog <-parameters[3]
  p.infected <-parameters[4]
  p.startPEP <-parameters[5]
  p.noPrEP.alive <-parameters[6]
  p.PrEP.alive <-parameters[7]
  cost.PrEP <-parameters[8]
  cost.PEP1 <-parameters[9]
  cost.PEP2or3 <-parameters[10]
  
  ####**** Decision tree ****####
  ## model run using time horizon 5 years (from age 1 to 16 years)
  
  ## noPrEP ##  with 8 paths (see Figure "Comparing the PEP-only to the PrEP plus PEP scenarios" @ README.md)
  
  # NoPrEP_DogBite_RabidDog_Infected_startPEP1_Alive
  # Pathway probability, based on the decision tree:
  noPrEP.path.1 <- p.dog.bite*15 * p.rabidDog * p.infected * p.startPEP * p.noPrEP.alive
  # Cost associated with this particular decision tree pathway:
  noPrEP.cost.1 <- cost.PEP1 * 1 + cost.PEP2or3 * 1
  # Whether death occurs in this pathway 
  # i.e. (1 = death, 0 = alive)
  noPrEP.cases.1 <- rep(0,n_samples)
  
  # Complete the rest pathways probabilities using the above as a guide
  
  # NoPrEP_DogBite_RabidDog_Infected_startPEP1_Death
  noPrEP.path.2 <- p.dog.bite*15 * p.rabidDog * p.infected * p.startPEP * (1-p.noPrEP.alive)
  noPrEP.cost.2 <- cost.PEP1 * 1 + cost.PEP2or3 * 1
  noPrEP.cases.2 <- rep(1,n_samples)
  
  # NoPrEP_DogBite_RabidDog_Infected_NoPEP1_Death
  noPrEP.path.3 <- p.dog.bite*15 * p.rabidDog * p.infected * (1-p.startPEP) 
  noPrEP.cost.3 <- rep(0,n_samples)
  noPrEP.cases.3 <- rep(1,n_samples)
  
  # NoPrEP_DogBite_RabidDog_NoInfected_startPEP1
  noPrEP.path.4 <- p.dog.bite*15 * p.rabidDog * (1-p.infected) * p.startPEP
  noPrEP.cost.4 <- cost.PEP1 * 1 + cost.PEP2or3 * 1
  noPrEP.cases.4 <- rep(0,n_samples)
  
  # NoPrEP_DogBite_RabidDog_NoInfected_NoPEP1
  noPrEP.path.5 <- p.dog.bite*15 * p.rabidDog * (1-p.infected) * (1-p.startPEP)
  noPrEP.cost.5 <- rep(0,n_samples)
  noPrEP.cases.5 <- rep(0,n_samples)
  
  # NoPrEP_DogBite_NoRabidDog_NoInfected_startPEP1
  noPrEP.path.6 <- p.dog.bite*15 * (1-p.rabidDog) * p.startPEP
  noPrEP.cost.6 <- cost.PEP1 * 1 + cost.PEP2or3 * 1
  noPrEP.cases.6 <- rep(0,n_samples)
  
  # NoPrEP_DogBite_NoRabidDog_NoInfected_noPEP1
  noPrEP.path.7 <- p.dog.bite*15 * (1-p.rabidDog) * (1-p.startPEP)
  noPrEP.cost.7 <- rep(0,n_samples)
  noPrEP.cases.7 <- rep(0,n_samples)
  
  # NoPrEP_NoDogBite
  noPrEP.path.8 <- rep(1-p.dog.bite*15, n_samples)  #to set the size of cost vector = n_samples
  noPrEP.cost.8 <- rep(0,n_samples)
  noPrEP.cases.8 <- rep(0,n_samples)
  
  # NoPrEP arm probabilities and results 
  # Create a vector containing all the pathway probabilities. Then do the same for the costs and cases.
  noPrEP.probs.dataframe <- data.frame(noPrEP.path.1, noPrEP.path.2, noPrEP.path.3, noPrEP.path.4, noPrEP.path.5, noPrEP.path.6, noPrEP.path.7, noPrEP.path.8)
  noPrEP.costs.dataframe <- data.frame(noPrEP.cost.1, noPrEP.cost.2, noPrEP.cost.3, noPrEP.cost.4, noPrEP.cost.5, noPrEP.cost.6, noPrEP.cost.7, noPrEP.cost.8)
  noPrEP.cases.dataframe <- data.frame(noPrEP.cases.1, noPrEP.cases.2, noPrEP.cases.3, noPrEP.cases.4, noPrEP.cases.5, noPrEP.cases.6, noPrEP.cases.7, noPrEP.cases.8)
  
  
  ### create a loop 
  noPrEP.costs.df <- data.frame(matrix(ncol = ncol(noPrEP.costs.dataframe), nrow = n_samples))
  noPrEP.cases.df <- data.frame(matrix(ncol = ncol(noPrEP.cases.dataframe), nrow = n_samples))
  noPrEP.costs.total.df <- data.frame(matrix(ncol = 1, nrow = n_samples))
  noPrEP.cases.total.df <- data.frame(matrix(ncol = 1, nrow = n_samples))
  
  for (i in 1:n_samples) {
    noPrEP.costs.df[i,] <-  noPrEP.probs.dataframe[i,] * noPrEP.costs.dataframe[i,]   ## expected value of costs for each pathway (hint: use vector multiplication)
    noPrEP.cases.df[i,] <-  noPrEP.probs.dataframe[i,] * noPrEP.cases.dataframe[i,]   ## expected value of cases for each pathway (hint: use vector multiplication)
  }
  
  noPrEP.costs.total.df <- rowSums(noPrEP.costs.df)      ## total costs (hint: use rowSums()) for each n_samples (simulation)
  noPrEP.cases.total.df <- rowSums(noPrEP.cases.df)      ## total cases (hint: use rowSums()) for each n_samples (simulation)
  
  # Show and label the results 
  noPrEP.results.df <- (data.frame(costs = noPrEP.costs.total.df, cases = noPrEP.cases.total.df))  
  noPrEP.results.df
  
  
  
  ## PrEP ##   with 9 paths (see Figure "Comparing the PEP-only to the PrEP plus PEP scenarios" @ README.md)
  
  # PrEP_DogBite_RabidDog_Infected_startPEP_Alive
  PrEP.path.1 <- p.dog.bite*15 * p.rabidDog * p.infected * p.startPEP * p.PrEP.alive
  PrEP.cost.1 <- cost.PrEP * 1 + cost.PEP1 * 1 + cost.PEP2or3 * 1
  PrEP.cases.1 <- rep(0,n_samples)
  
  # PrEP_DogBite_RabidDog_Infected_startPEP_Death
  PrEP.path.2 <- p.dog.bite*15 * p.rabidDog * p.infected * p.startPEP * (1-p.PrEP.alive)
  PrEP.cost.2 <- cost.PrEP * 1 + cost.PEP1 * 1 + cost.PEP2or3 * 1
  PrEP.cases.2 <- rep(1,n_samples)
  
  # PrEP_DogBite_RabidDog_Infected_noPEP_Alive
  PrEP.path.3 <- p.dog.bite*15 * p.rabidDog * p.infected * (1-p.startPEP) * p.PrEP.preventRabies
  PrEP.cost.3 <- cost.PrEP * 1 
  PrEP.cases.3 <- rep(0,n_samples)
  
  # PrEP_DogBite_RabidDog_Infected_noPEP_Death
  PrEP.path.4 <- p.dog.bite*15 * p.rabidDog * p.infected * (1-p.startPEP) * (1-p.PrEP.preventRabies)
  PrEP.cost.4 <- cost.PrEP * 1 
  PrEP.cases.4 <- rep(1,n_samples)
  
  # PrEP_DogBite_RabidDog_NoInfected_startPEP
  PrEP.path.5 <- p.dog.bite*15 * p.rabidDog * (1-p.infected) * p.startPEP
  PrEP.cost.5 <- cost.PrEP * 1 + cost.PEP1 * 1 + cost.PEP2or3 * 1
  PrEP.cases.5 <- rep(0,n_samples)
  
  # PrEP_DogBite_RabidDog_NoInfected_noPEP
  PrEP.path.6 <- p.dog.bite*15 * p.rabidDog * (1-p.infected) * (1-p.startPEP)
  PrEP.cost.6 <- cost.PrEP * 1
  PrEP.cases.6 <- rep(0,n_samples)
  
  # PrEP_DogBite_NoRabidDog_NoInfected_startPEP
  PrEP.path.7 <- p.dog.bite*15 * (1-p.rabidDog) * p.startPEP
  PrEP.cost.7 <- cost.PrEP * 1 + cost.PEP1 * 1 + cost.PEP2or3 * 1
  PrEP.cases.7 <- rep(0,n_samples)
  
  # PrEP_DogBite_NoRabidDog_NoInfected_noPEP
  PrEP.path.8 <- p.dog.bite*15 * (1-p.rabidDog) * (1-p.startPEP)
  PrEP.cost.8 <- cost.PrEP * 1
  PrEP.cases.8 <- rep(0,n_samples)
  
  # PrEP_NoDogBite
  PrEP.path.9 <- rep(1-p.dog.bite*15, n_samples) # to set the size of cost vector = n_samples
  PrEP.cost.9 <- rep(cost.PrEP * 1,n_samples)    # to set the size of cost vector = n_samples
  PrEP.cases.9 <- rep(0,n_samples)
  
  # PrEP arm probabilities and results 
  PrEP.probs.dataframe <- data.frame(PrEP.path.1, PrEP.path.2, PrEP.path.3, PrEP.path.4, PrEP.path.5, PrEP.path.6, PrEP.path.7, PrEP.path.8, PrEP.path.9)
  PrEP.costs.dataframe <- data.frame(PrEP.cost.1, PrEP.cost.2, PrEP.cost.3, PrEP.cost.4, PrEP.cost.5, PrEP.cost.6, PrEP.cost.7, PrEP.cost.8, PrEP.cost.9)
  PrEP.cases.dataframe <- data.frame(PrEP.cases.1, PrEP.cases.2, PrEP.cases.3, PrEP.cases.4, PrEP.cases.5, PrEP.cases.6, PrEP.cases.7, PrEP.cases.8, PrEP.cases.9)
  
  
  ### create a loop 
  PrEP.costs.df <- data.frame(matrix(ncol = ncol(PrEP.costs.dataframe), nrow = n_samples))
  PrEP.cases.df <- data.frame(matrix(ncol = ncol(PrEP.cases.dataframe), nrow = n_samples))
  PrEP.costs.total.df <- data.frame(matrix(ncol = 1, nrow = n_samples))
  PrEP.cases.total.df <- data.frame(matrix(ncol = 1, nrow = n_samples))
  
  for (i in 1:n_samples) {
    PrEP.costs.df[i,] <-  PrEP.probs.dataframe[i,] * PrEP.costs.dataframe[i,]   ## expected value of costs for each pathway (hint: use vector multiplication)
    PrEP.cases.df[i,] <-  PrEP.probs.dataframe[i,] * PrEP.cases.dataframe[i,]   ## expected value of cases for each pathway (hint: use vector multiplication)
  }
  
  PrEP.costs.total.df <- rowSums(PrEP.costs.df)      ## total costs (hint: use rowSums()) for each n_samples (simulation)
  PrEP.cases.total.df <- rowSums(PrEP.cases.df)      ## total cases (hint: use rowSums()) for each n_samples (simulation)
  
  # Show and label the results 
  PrEP.results.df <- (data.frame(costs = PrEP.costs.total.df, cases = PrEP.cases.total.df))  
  PrEP.results.df
  
 
####**** Analysis: Incremental results ****#### 
  # Derive the appropriate parameters assigned above to estimate the cases avoided, 
  # the incremental costs and the cost per rabies-infected death avoided
  
  # Reduction in probability of death with PrEP+abbreviatedPEP and PEP
  cases.avoided.df <- noPrEP.results.df$cases - PrEP.results.df$cases
  
  # Gain in QALYs with PrEP+abbreviatedPEP and PEP
  #### add discount rate ####
  # Discount rate = {0,0.015,0.03}
  # re-run this function with 1) discount_rate <-0.0, 2) discount_rate <-0.01, and discount_rate <-0.03
  # code present at line 185-198
  discount_rate <-0.0  # rate = {0,0.015,0.03}
  QALYs.cases.avoided.discount0.df <- ((cases.avoided.df * 1/(1+discount_rate)^(1:50)) ) # using 50 years
  QALYs.cases.avoided.discount0.df.sum <-sum(QALYs.cases.avoided.discount0.df)
  QALYs.cases.avoided.df <-QALYs.cases.avoided.discount0.df.sum
  
  ####discount_rate <-0.015  # rate = {0,0.015,0.03}
  ####QALYs.cases.avoided.discount0015.df <- ((cases.avoided.df * 1/(1+discount_rate)^(1:50)) )
  ####QALYs.cases.avoided.discount0015.df.sum <-sum(QALYs.cases.avoided.discount0015.df)
  ####QALYs.cases.avoided.df <-QALYs.cases.avoided.discount0015.df.sum
  
  ####discount_rate <-0.03  # rate = {0,0.015,0.03}
  ####QALYs.cases.avoided.discount003.df <- ((cases.avoided.df * 1/(1+discount_rate)^(1:50)) )
  ####QALYs.cases.avoided.discount003.df.sum <-sum(QALYs.cases.avoided.discount003.df)
  ####QALYs.cases.avoided.df <-QALYs.cases.avoided.discount003.df.sum
  
  # Incremental cost of PrEP+abbreviatedPEP and PEP
  incr.costs.df <- PrEP.results.df$costs - noPrEP.results.df$costs 
  # Cost per rabies-infected death avoided
  # incremental.results.df <- incr.costs.df/cases.avoided.df
  # Cost per rabies-infected QALYs gain
  incremental.results.df <- incr.costs.df/QALYs.cases.avoided.df
  print(incremental.results.df)
}

################# END function "contour_ICER" #################

