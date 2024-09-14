####**** Function sensitivity_ICER ****####
sensitivity_ICER <-function() {
  
  ####**** Decision tree ****####
  ## model run using time horizon 15 years (from age 1 to 16 years)
  ## noPrEP ##  with 8 paths (see file "DecisionTree_drawing_2023July24")
  
  # NoPrEP_DogBite_RabidDog_Infected_startPEP1_Alive
  # This is the pathway probability, based on the decision tree:
  noPrEP.path.1 <- p.dog.bite*15 * p.rabidDog * p.infected * p.startPEP * p.noPrEP.alive
  
  # This is the cost associated with this particular decision tree pathway:
  noPrEP.cost.1 <- cost.PEP1 * 1 + cost.PEP2or3 * 1
  
  # This is whether death occurs in this pathway 
  # i.e. (1 = death, 0 = alive)
  noPrEP.cases.1 <- rep(0,n_samples)
  
  # Now complete the remainder of the pathway probabilities using the above as a guide
  
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
  #noPrEP.path.8 <- (1-p.dog.bite*15)
  noPrEP.path.8 <- rep(1-p.dog.bite*15, n_samples) # need this command instead of the above line, in order to set the size of cost vector = n_samples
  noPrEP.cost.8 <- rep(0,n_samples)
  noPrEP.cases.8 <- rep(0,n_samples)
  
  
  
  # NoPrEP arm probabilities and results 
  # Now create a vector containing all the pathway probabilities. Then do the same for the costs and cases.
  
  ###noPrEP.probs.vec <- c(noPrEP.path.1, noPrEP.path.2, noPrEP.path.3, noPrEP.path.4, noPrEP.path.5, 
  ###                      noPrEP.path.6, noPrEP.path.7, noPrEP.path.8, noPrEP.path.9, noPrEP.path.10, noPrEP.path.11)
  ###noPrEP.costs.vec <- c(noPrEP.cost.1, noPrEP.cost.2, noPrEP.cost.3, noPrEP.cost.4, noPrEP.cost.5, 
  ###                      noPrEP.cost.6, noPrEP.cost.7, noPrEP.cost.8, noPrEP.cost.9, noPrEP.cost.10, noPrEP.cost.11)
  ###noPrEP.cases.vec <- c(noPrEP.cases.1, noPrEP.cases.2, noPrEP.cases.3, noPrEP.cases.4, noPrEP.cases.5, 
  ###                       noPrEP.cases.6, noPrEP.cases.7, noPrEP.cases.8, noPrEP.cases.9, noPrEP.cases.10, noPrEP.cost.11)
  
  
  noPrEP.probs.dataframe <- data.frame(noPrEP.path.1, noPrEP.path.2, noPrEP.path.3, noPrEP.path.4, noPrEP.path.5, 
                                       noPrEP.path.6, noPrEP.path.7, noPrEP.path.8)
  noPrEP.costs.dataframe <- data.frame(noPrEP.cost.1, noPrEP.cost.2, noPrEP.cost.3, noPrEP.cost.4, noPrEP.cost.5, 
                                       noPrEP.cost.6, noPrEP.cost.7, noPrEP.cost.8)
  noPrEP.cases.dataframe <- data.frame(noPrEP.cases.1, noPrEP.cases.2, noPrEP.cases.3, noPrEP.cases.4, noPrEP.cases.5, 
                                       noPrEP.cases.6, noPrEP.cases.7, noPrEP.cases.8)
  
  
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
  
  # Here we can show and label the results 
  noPrEP.results.df <- (data.frame(costs = noPrEP.costs.total.df, cases = noPrEP.cases.total.df))  
  noPrEP.results.df
  
  
  
  
  
  ## PrEP ##  9 paths
  
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
  #PrEP.path.9 <- (1-p.dog.bite*15)
  PrEP.path.9 <- rep(1-p.dog.bite*15, n_samples) # need this command instead of the above line, in order to set the size of cost vector = n_samples
  #PrEP.cost.9 <- cost.PrEP * 1
  PrEP.cost.9 <- rep(cost.PrEP * 1,n_samples)   # need this command instead of the above line, in order to set the size of cost vector = n_samples
  PrEP.cases.9 <- rep(0,n_samples)
  
  
  # PrEP arm probabilities and results 
  PrEP.probs.dataframe <- data.frame(PrEP.path.1, PrEP.path.2, PrEP.path.3, PrEP.path.4, PrEP.path.5, PrEP.path.6, PrEP.path.7, PrEP.path.8,
                                     PrEP.path.9)
  PrEP.costs.dataframe <- data.frame(PrEP.cost.1, PrEP.cost.2, PrEP.cost.3, PrEP.cost.4, PrEP.cost.5, PrEP.cost.6, PrEP.cost.7, PrEP.cost.8,
                                     PrEP.cost.9)
  PrEP.cases.dataframe <- data.frame(PrEP.cases.1, PrEP.cases.2, PrEP.cases.3, PrEP.cases.4, PrEP.cases.5, PrEP.cases.6, PrEP.cases.7, PrEP.cases.8,
                                     PrEP.cases.9)
  
  
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
  
  # Here we can show and label the results 
  PrEP.results.df <- (data.frame(costs = PrEP.costs.total.df, cases = PrEP.cases.total.df))  
  PrEP.results.df
  
  # Create a vector of each of the probabilities, costs and cases for the pathways
  ####**** Analysis: Incremental results ****#### 
  ####*
  # Now derive the appropriate parameters assigned above to estimate the cases avoided, 
  #  the incremental costs and the cost per rabies-infected death avoided
  
  # Reduction in probability of death with PrEP+abbreviatedPEP and PEP
  cases.avoided.df <- noPrEP.results.df$cases - PrEP.results.df$cases
  
  # Incremental cost of  PrEP+abbreviatedPEP and PEP
  incr.costs.df <- PrEP.results.df$costs - noPrEP.results.df$costs 
  
  # Cost per rabies-infected death avoided
  incremental.results.df <- incr.costs.df/cases.avoided.df
  
  ####**** Analysis: Incremental results for cohort size population (assume = 1)****#### 
  
  ## QALYs from 1 to 16 years (total year = 15 years) (if either with PEP or PrEP arm), 
  ## assume those who will be death avoided due to using PrEP, we can save QALYs =  50 years each 
  
  QALYs.cases.avoided.df <- (cases.avoided.df * 1) * 50
  
  ##################. add discount rate ##################  
  discount_rate <-0.0  # rate = {0,0.015,0.03}
  #QALYs.cases.avoided.discount0.df <- ((cases.avoided.df * 1/(1+discount_rate)^(1:50)) )
  #QALYs.cases.avoided.discount0.df.sum <-sum(QALYs.cases.avoided.discount0.df)
  
  ### create a empty variable "QALYs.cases.avoided.discount0.df"
  QALYs.cases.avoided.discount0.df <- data.frame(matrix(ncol = 1, nrow = n_samples))
  for (i in 1:n_samples) {
    QALYs.cases.avoided.discount0.df[i] <-  sum(cases.avoided.df[i] * 1/(1+discount_rate)^(1:50) )  ## expected value of costs for each pathway (hint: use vector multiplication)
  }
  QALYs.cases.avoided.discount0.df.sum <- sum(QALYs.cases.avoided.discount0.df)      ## total costs (hint: use rowSums()) for each n_samples (simulation)
  
  # Incremental cost of  PrEP+PEP and PEP for cohort size 500,000 population
  cohort.incr.costs.df <- (incr.costs.df) * 1
  
  # Cost per rabies-infected death avoided
  cohort.incremental.results.df <- cohort.incr.costs.df/QALYs.cases.avoided.df
  
  # Incremental cost of  PrEP+PEP and PEP for cohort size 500,000 population
  cohort.incr.costs.df <- (incr.costs.df) * 1
  cohort.incremental.results.discount0.df <- as.vector(cohort.incr.costs.df)/unlist(as.vector(QALYs.cases.avoided.discount0.df[1,]))
  #cohort.incremental.results.discount0015.df <- as.vector(cohort.incr.costs.df)/unlist(as.vector(QALYs.cases.avoided.discount0015.df[1,]))
  #cohort.incremental.results.discount003.df <- as.vector(cohort.incr.costs.df)/unlist(as.vector(QALYs.cases.avoided.discount003.df[1,]))
  
  cohort_incremental <-data.frame(p.startPEP,p.rabidDog,p.PrEP.preventRabies,p.noPrEP.alive,p.dog.bite,cost.PrEP,cohort.incremental.results.df)
  return(cohort_incremental)
}

####  END Function sensitivity_ICER #####
