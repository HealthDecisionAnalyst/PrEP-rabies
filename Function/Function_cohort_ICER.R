
################# Function to calculate ICERs and pathway details for Tornado plot #################
## This function calculates the Incremental Cost-Effectiveness Ratios (ICERs) for two interventions: 
## (1) No PrEP and (2) PrEP with abbreviated PEP, for preventing rabies. It computes expected costs, cases, 
## and QALYs for each intervention by evaluating all decision tree pathways.

cohort_ICER <-function(parameters) {
  
  # Number of samples (currently set to 1 for deterministic calculations, extend for probabilistic analysis)
  n_samples <- 1000
  
  #### PROBABLISTIC PARAMETERS #####
  ## First, assign the appropriate distribution and key values to the parameters below.  
  ## Probabilities 
  
  ## Beta(r, n) , where r=alpha and n= alpha+beta)
  ## alpha = mean * (alpha+beta)
  ## alpha+beta  = { (mean * (1-mean)) / (standard error)^2 } -1
  p.PrEP.preventRabies  <- runif(n = n_samples, min = 0.0, max= 0.95)                # test flat distribution
  
  #mn.p.dog.bite <- 0.01                                                      ## mean of Probability of dog bite, bite incidence (SV3.1)
  #se.p.dog.bite <- 0.03                                                      ## standard error of Probability of dog bite (assume at 0.03, as [min,max] = [0.0004,0.04]) 
  #shape2.p.dog.bite <- (mn.p.dog.bite*(1-mn.p.dog.bite)/(se.p.dog.bite^2))-1 ## alpha+beta value (or shape2) for Probability of dog bite
  #shape1.p.dog.bite <- (mn.p.dog.bite* shape2.p.dog.bite)                    ## alpha value (or shape1) for Probability of dog bite
  #p.dog.bite <- rbeta(n = n_samples, shape1 = shape1.p.dog.bite , shape2 = shape2.p.dog.bite)          # Beta distribution draw for Probability of dog bite
  p.dog.bite <- runif(n = n_samples, min = 0.0001, max= 0.01)                # test flat distribution
  
  #mn.p.rabidDog <- 0.05                                                      ## mean of Probability of dog bite rabid (SV3.2)
  #se.p.rabidDog <- 0.05                                                      ## standard error of Probability of dog bite (assume at 0.05, as [min,max] = [0.001,0.1]) 
  #shape2.p.rabidDog <- (mn.p.rabidDog*(1-mn.p.rabidDog)/(se.p.rabidDog^2))-1 ## alpha+beta value (or shape2) for Probability of dog bit
  #shape1.p.rabidDog <- (mn.p.rabidDog* shape2.p.rabidDog)                    ## alpha value (or shape1) for Probability of dog bit
  #p.rabidDog <- rbeta(n = n_samples, shape1 = shape1.p.rabidDog  , shape2 = shape2.p.rabidDog)         # Beta distribution draw for Probability of dog bite rabid
  p.rabidDog <- runif(n = n_samples, min = 0.1, max= 0.86)                # test flat distribution 
  
  #mn.p.infected <- 0.2                                                       ## mean of Probability of dog bite rabid, human infected (SV3.3)
  #se.p.infected <- 0.05                                                      ## standard error of Probability of dog bite rabid, human infected (assume at 0.2, as [min,max] = [0.01,0.4]) 
  #shape2.p.infected <- (mn.p.infected*(1-mn.p.infected)/(se.p.infected^2))-1 ## alpha+beta value (or shape2) for Probability of dog bite rabid, human infected
  #shape1.p.infected <- (mn.p.infected* shape2.p.infected)                    ## alpha value (or shape1) for Probability of dog bite rabid, human infected
  #p.infected <- rbeta(n = n_samples, shape1 = shape1.p.infected , shape2 = shape2.p.infected)          # Beta distribution draw for Probability of dog bite rabid, human infected
  p.infected <- runif(n = n_samples, min = 0.01, max= 0.3)                # test flat distribution 
  
  #mn.p.startPEP <- 0.9                                                       ## mean of Probability of dog bite rabid, human infected, start PEP  (only 1 PEP for those do PrEP or noPrEP)
  #se.p.startPEP <- 0.05                                                      ## standard error of Probability of dog bite rabid, human infected, start PEP  (only 1 PEP for those do PrEP or noPrEP) (assume at 0.05, as [min,max] = [0.0,1.0]) 
  #shape2.p.startPEP <- (mn.p.startPEP*(1-mn.p.startPEP)/(se.p.startPEP^2))-1 ## alpha+beta value (or shape2) for Probability of dog bite rabid, human infected, start PEP  (only 1 PEP for those do PrEP or noPrEP)
  #shape1.p.startPEP <- (mn.p.startPEP* shape2.p.startPEP)                    ## alpha value (or shape1) for Probability of dog bite rabid, human infected, start PEP  (only 1 PEP for those do PrEP or noPrEP)
  #p.startPEP <- rbeta(n = n_samples, shape1 = shape1.p.startPEP , shape2 = shape2.p.startPEP)          # Beta distribution draw for Probability of dog bite rabid, human infected, start PEP  (only 1 PEP for those do PrEP or noPrEP)
  
  #mn.p.startPEP1 <- 0.9                                                       ## mean of Probability of dog bite rabid, human infected, start PEP1  (2 or 3 PEP for those do noPrEP) (SV4.1)
  #se.p.startPEP1 <- 0.05                                                      ## standard error of Probability of dog bite rabid, human infected, start PEP1  (2 or 3 PEP for those do noPrEP) (assume at 0.05, as [min,max] = [0.0,1.0]) 
  #shape2.p.startPEP1 <- (mn.p.startPEP1*(1-mn.p.startPEP1)/(se.p.startPEP1^2))-1 ## alpha+beta value (or shape2) for Probability of dog bite rabid, human infected, start PEP1  (2 or 3 PEP for those do noPrEP)
  #shape1.p.startPEP1 <- (mn.p.startPEP1* shape2.p.startPEP1)                    ## alpha value (or shape1) for Probability of dog bite rabid, human infected, start PEP1  (2 or 3 PEP for those do noPrEP)
  #p.startPEP1 <- rbeta(n = n_samples, shape1 = shape1.p.startPEP1 , shape2 = shape2.p.startPEP1 )         # Beta distribution draw for Probability of dog bite rabid, human infected, start PEP1 (2 or 3 PEP for those do noPrEP)
  p.startPEP <- runif(n = n_samples, min = 0.1, max= 0.9)                         # test flat distribution 
  
  #mn.p.completePEP2or3 <- 0.8                                                   ## mean of Probability of dog bite rabid, human infected, start PEP1, complete PEP 2or3 (2 or 3 PEP for those do noPrEP)
  #se.p.completePEP2or3 <- 0.05                                                  ## standard error of Probability of dog bite rabid, human infected, start PEP1, complete PEP 2or3 (2 or 3 PEP for those do noPrEP)(assume at 0.05, as [min,max] = [0.0,1.0]) 
  #shape2.p.completePEP2or3 <- (mn.p.completePEP2or3*(1-mn.p.completePEP2or3)/(se.p.completePEP2or3^2))-1 ## alpha+beta value (or shape2) for Probability of dog bite rabid, human infected, start PEP1, complete PEP 2or3 (2 or 3 PEP for those do noPrEP)
  #shape1.p.completePEP2or3 <- (mn.p.completePEP2or3* shape2.p.completePEP2or3)                    ## alpha value (or shape1) for Probability of dog bite rabid, human infected, start PEP1, complete PEP 2or3 (2 or 3 PEP for those do noPrEP)
  #p.completePEP2or3 <- rbeta(n = n_samples, shape1 = shape1.p.completePEP2or3 , shape2 = shape2.p.completePEP2or3)   # Beta distribution draw for Probability of dog bite rabid, human infected, start PEP1, complete PEP 2or3 (2 or 3 PEP for those do noPrEP)
  #p.completePEP2or3 <- runif(n = n_samples, min = 0.4, max= 1)                # test flat distribution 
  
  #mn.p.noPrEP.alive <- 0.5                                                       ## mean of Probability of dog bite rabid, human infected, start PEP1, not complete PEP 2or3, alive (SV4.2)
  #se.p.noPrEP.alive <- 0.3                                                       ## standard error of Probability of dog bite rabid, human infected, start PEP1, not complete PEP 2or3, alive (assume at 0.3, as [min,max] = [0.2,0.9]) 
  #shape2.p.noPrEP.alive <- (mn.p.noPrEP.alive*(1-mn.p.noPrEP.alive)/(se.p.noPrEP.alive^2))-1 ## alpha+beta value (or shape2) for Probability of dog bite rabid, human infected, start PEP1,not complete PEP 2or3, alive
  #shape1.p.noPrEP.alive <- (mn.p.noPrEP.alive* shape2.p.noPrEP.alive)                    ## alpha value (or shape1) for Probability of dog bite rabid, human infected, start PEP1, not complete PEP 2or3, alive 
  #p.noPrEP.alive <- rbeta(n = n_samples, shape1 = shape1.p.noPrEP.alive , shape2 = shape2.p.noPrEP.alive)      # Beta distribution draw for Probability of dog bite rabid, human infected, start PEP1, not complete PEP 2or3, alive
  p.noPrEP.alive <- runif(n = n_samples, min = 0.89, max= 0.99)                # test flat distribution 
  
  #mn.p.PrEP.alive <- 0.5                                                       ## mean of Probability of dog bite rabid, human infected, not complete PEP 2or3, alive (SV4.4)
  #se.p.PrEP.alive <- 0.3                                                       ## standard error of Probability of dog bite rabid, human infected, no PEP, alive (assume at 0.3, as [min,max] = [0.2,0.9]) 
  #shape2.p.PrEP.alive <- (mn.p.PrEP.alive*(1-mn.p.PrEP.alive)/(se.p.PrEP.alive^2))-1 ## alpha+beta value (or shape2) for Probability of dog bite rabid, human infected, no PEP, alive
  #shape1.p.PrEP.alive <- (mn.p.PrEP.alive* shape2.p.PrEP.alive)                  ## alpha value (or shape1) for Probability of dog bite rabid, human infected, no PEP, alive
  #p.PrEP.alive <- rbeta(n = n_samples, shape1 = shape1.p.PrEP.alive , shape2 = shape2.p.PrEP.alive)        # Beta distribution draw for Probability of dog bite rabid, human infected, no PEP, alive
  p.PrEP.alive <- runif(n = n_samples, min = 1, max= 1)                # test flat distribution 
  
  ## Costs
  #mn.cost.PrEP <- 3 ## mean cost of PrEP
  #se.cost.PrEP <- 2 ## standard error of cost of PrEP (assume at 2, as [min,max] = [1,5]) 
  #a.cost.PrEP <- (mn.cost.PrEP/se.cost.PrEP)^2 ## alpha value for cost of PrEP
  #b.cost.PrEP <- (se.cost.PrEP^2)/mn.cost.PrEP ## beta value for cost of PrEP
  #cost.PrEP <- rgamma(n = n_samples,shape=a.cost.PrEP,scale=b.cost.PrEP) ## Gamma distribution draw for cost of PrEP
  ## cost.PrEP = (SV2) = cost of PrEP/person $US (as in Excel file "230705 Model variables") 
  ## This variable is treated as fix deterministic for  5 values {1,2,4,8,16}
  cost.PrEP <- runif(n = n_samples, min = 2, max= 45)                # test flat distribution 
  
  #mn.cost.PEP <- 20 ## mean cost of only 1 PEP for those do PrEP 
  #se.cost.PEP <- 10 ## standard error of cost of only 1 PEP for those do PrEP  (assume at 10, as [min,max] = [5,30]) 
  #a.cost.PEP <- (mn.cost.PEP/se.cost.PEP)^2 ## alpha value for cost of only 1 PEP for those do PrEP 
  #b.cost.PEP <- (se.cost.PEP^2)/mn.cost.PEP ## beta value for cost of only 1 PEP for those do PrEP 
  #cost.PEP <- rgamma(n = n_samples,shape=a.cost.PEP,scale=b.cost.PEP) ## Gamma distribution draw for cost of only 1 PEP for those do PrEP 
  
  # mn.cost.PEP1 <- 100 ## mean cost of first PEP for those do PrEP ordo noPrEP (combined direct and indirect costs: 50+50)
  # se.cost.PEP1 <- 30 ## standard error of cost of first PEP for those do PrEP ordo noPrEP  (assume at 15, as [min,max] = [15+15,90+90]) 
  # a.cost.PEP1 <- (mn.cost.PEP1/se.cost.PEP1)^2 ## alpha value for cost of first PEP for those do PrEP or do noPrEP 
  # b.cost.PEP1 <- (se.cost.PEP1^2)/mn.cost.PEP1 ## beta value for cost of first PEP for those do PrEP or do noPrEP 
  # cost.PEP1 <- rgamma(n = n_samples,shape=a.cost.PEP1,scale=b.cost.PEP1) ## Gamma distribution draw for cost of first PEP for those do PrEP or do noPrEP 
  cost.PEP1 <- 0
  
  # mn.cost.PEP2or3 <- 100 ## mean cost of two or three-dose PEP only for those do noPrEP (combined direct and indirect costs: 1x(50+50))
  # se.cost.PEP2or3 <- 30 ## standard error of cost of two or three-dose PEP only for those do noPrEP
  # a.cost.PEP2or3 <- (mn.cost.PEP2or3/se.cost.PEP2or3)^2 ## alpha value for cost of two or three-dose PEP only for those do noPrEP
  # b.cost.PEP2or3 <- (se.cost.PEP2or3^2)/mn.cost.PEP2or3 ## beta value for cost of two or three-dose PEP only for those do noPrEP
  # cost.PEP2or3 <- rgamma(n = n_samples,shape=a.cost.PEP2or3,scale=b.cost.PEP2or3) ## Gamma distribution draw for cost of two or three-dose PEP only for those do noPrEP
  cost.PEP2or3 <- 0
  
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
  # Incremental cost of PrEP+PEP and PEP for cohort size  
  cohort.incr.costs.df <- (incr.costs.df) * 1
  # Cost per rabies-infected death avoided
  cohort.incremental.results.df <- cohort.incr.costs.df/QALYs.cases.avoided.df
  
  cohort_incremental <- data.frame(p.startPEP,p.rabidDog,p.PrEP.preventRabies,p.noPrEP.alive,p.dog.bite,cost.PrEP,cohort.incremental.results.df)
  return(cohort_incremental)
}

################# END function "cohort_ICER" #################


