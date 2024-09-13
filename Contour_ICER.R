####**** This script sets up a series (25) of inner 5x5 matrices elements for outer matrix (R1C1, R1C2, ..., R5C5) size 5x5
## each inner matrix representing different risk levels for rabies and PEP access (Post-Exposure Prophylaxis).
## The "contour_ICER" function will be applied to each inner matrix, calculating the ICER (Incremental Cost-Effectiveness Ratio)
## for every combination of rabies risk and PEP access

## The inner matrices are structured as follows:
## - Columns represent different rabies risk levels (e.g., 0.6, 1.6, 5.7, 11.4, 19.0)
## - Rows represent different PEP access (e.g., 0.9, 0.7, 0.5, 0.3, 0.1)
## The goal is to iterate over each matrix and its respective cell values, 
## computing the ICER for all "rabies risk" and "PEP access" combinations.

## The outer matrix representing different "cost of rabies PrEP per person" and "efficacy of PrEP in preventing rabies in the absence of PEP"
## The outer matrices are structured as follows:
## - Columns represent "efficacy of PrEP in preventing rabies in the absence of PEP" (e.g., 0, 0.3, 0.6, 0.8, 0.95)
## - Rows represent "cost of rabies PrEP per person" (e.g., 2, 3, 5, 15, 45)
## The goal is to iterate over each matrix and its respective cell values, 



# created initial inner matrices size 5x5 for [Rabies risk X PEP access]
# [R1xC1]
R1C1 <-matrix(0, nrow = 5, ncol=5, byrow=TRUE)  
# All initial inner matrices created as a copy of [R1xC1]
R1C2 <-R1C1; R1C3 <-R1C1; R1C4 <-R1C1; R1C5 <-R1C1
R2C1 <-R1C1; R2C2 <-R1C1; R2C3 <-R1C1; R2C4 <-R1C1; R2C5 <-R1C1
R3C1 <-R1C1; R3C2 <-R1C1; R3C3 <-R1C1; R3C4 <-R1C1; R3C5 <-R1C1
R4C1 <-R1C1; R4C2 <-R1C1; R4C3 <-R1C1; R4C4 <-R1C1; R4C5 <-R1C1
R5C1 <-R1C1; R5C2 <-R1C1; R5C3 <-R1C1; R5C4 <-R1C1; R5C5 <-R1C1



##### step1 call function contour_ICER
## step2 using set of parameters 

### Define fixed parts of the parameters for R1C1,R1C2,R1C3,R1C4,R1C5
fixed_params <- c(0, 0.0001, 0.3, 0.19, 0.94, 1, 45, 0, 0)
parameters <- c(0, 0.0001, 0.3, 0.19, 0.9, 0.94, 1, 45, 0, 0) ; R1C1[1,1] <-contour_ICER(parameters)

phi_values <- c(0.9, 0.7, 0.5, 0.3, 0.1)
theta_values <- c(0.0001, 0.001, 0.003, 0.01)

# R1C1 loop
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R1C1[i, j] <- contour_ICER(parameters)
  }
}

# R1C2 loop (changing first parameter to 0.3)
fixed_params[1] <- 0.3
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R1C2[i, j] <- contour_ICER(parameters)
  }
}

# R1C3 loop (changing first parameter to 0.6)
fixed_params[1] <- 0.6
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R1C3[i, j] <- contour_ICER(parameters)
  }
}

# R1C4 loop (changing first parameter to 0.8)
fixed_params[1] <- 0.8
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R1C4[i, j] <- contour_ICER(parameters)
  }
}

# R1C5 loop (changing first parameter to 0.95)
fixed_params[1] <- 0.95
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R1C5[i, j] <- contour_ICER(parameters)
  }
}

### Define fixed parts of the parameters for R2C1,R2C2,R2C3,R2C4,R2C5
fixed_params <- c(0, 0.0001, 0.3, 0.19, 0.94, 1, 15, 0, 0)
parameters <- c(0, 0.0001, 0.3, 0.19, 0.9, 0.94, 1, 15, 0, 0) ; R1C1[1,1] <-contour_ICER(parameters)

phi_values <- c(0.9, 0.7, 0.5, 0.3, 0.1)
theta_values <- c(0.0001, 0.001, 0.003, 0.01)

# R2C1 loop
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R2C1[i, j] <- contour_ICER(parameters)
  }
}

# R2C2 loop (changing first parameter to 0.3)
fixed_params[1] <- 0.3
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R2C2[i, j] <- contour_ICER(parameters)
  }
}

# R2C3 loop (changing first parameter to 0.6)
fixed_params[1] <- 0.6
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R2C3[i, j] <- contour_ICER(parameters)
  }
}

# R2C4 loop (changing first parameter to 0.8)
fixed_params[1] <- 0.8
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R2C4[i, j] <- contour_ICER(parameters)
  }
}

# R2C5 loop (changing first parameter to 0.95)
fixed_params[1] <- 0.95
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R2C5[i, j] <- contour_ICER(parameters)
  }
}

### Define fixed parts of the parameters for R3C1,R3C2,R3C3,R3C4,R3C5
fixed_params <- c(0, 0.0001, 0.3, 0.19, 0.94, 1, 5, 0, 0)
parameters <- c(0, 0.0001, 0.3, 0.19, 0.9, 0.94, 1, 5, 0, 0) ; R1C1[1,1] <-contour_ICER(parameters)

phi_values <- c(0.9, 0.7, 0.5, 0.3, 0.1)
theta_values <- c(0.0001, 0.001, 0.003, 0.01)

# R3C1 loop
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R3C1[i, j] <- contour_ICER(parameters)
  }
}

# R3C2 loop (changing first parameter to 0.3)
fixed_params[1] <- 0.3
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R3C2[i, j] <- contour_ICER(parameters)
  }
}

# R3C3 loop (changing first parameter to 0.6)
fixed_params[1] <- 0.6
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R3C3[i, j] <- contour_ICER(parameters)
  }
}

# R3C4 loop (changing first parameter to 0.8)
fixed_params[1] <- 0.8
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R3C4[i, j] <- contour_ICER(parameters)
  }
}

# R3C5 loop (changing first parameter to 0.95)
fixed_params[1] <- 0.95
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R3C5[i, j] <- contour_ICER(parameters)
  }
}

### Define fixed parts of the parameters for R4C1,R4C2,R4C3,R4C4,R4C5
fixed_params <- c(0, 0.0001, 0.3, 0.19, 0.94, 1, 2, 0, 0)
parameters <- c(0, 0.0001, 0.3, 0.19, 0.9, 0.94, 1, 2, 0, 0) ; R1C1[1,1] <-contour_ICER(parameters)

phi_values <- c(0.9, 0.7, 0.5, 0.3, 0.1)
theta_values <- c(0.0001, 0.001, 0.003, 0.01)

# R4C1 loop
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R4C1[i, j] <- contour_ICER(parameters)
  }
}

# R4C2 loop (changing first parameter to 0.3)
fixed_params[1] <- 0.3
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R4C2[i, j] <- contour_ICER(parameters)
  }
}

# R4C3 loop (changing first parameter to 0.6)
fixed_params[1] <- 0.6
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R4C3[i, j] <- contour_ICER(parameters)
  }
}

# R4C4 loop (changing first parameter to 0.8)
fixed_params[1] <- 0.8
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R4C4[i, j] <- contour_ICER(parameters)
  }
}

# R4C5 loop (changing first parameter to 0.95)
fixed_params[1] <- 0.95
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R4C5[i, j] <- contour_ICER(parameters)
  }
}

### Define fixed parts of the parameters for R5C1,R5C2,R5C3,R5C4,R5C5
fixed_params <- c(0, 0.0001, 0.3, 0.19, 0.94, 1, 1, 0, 0)
parameters <- c(0, 0.0001, 0.3, 0.19, 0.9, 0.94, 1, 1, 0, 0) ; R1C1[1,1] <-contour_ICER(parameters)

phi_values <- c(0.9, 0.7, 0.5, 0.3, 0.1)
theta_values <- c(0.0001, 0.001, 0.003, 0.01)

# R5C1 loop
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R5C1[i, j] <- contour_ICER(parameters)
  }
}

# R5C2 loop (changing first parameter to 0.3)
fixed_params[1] <- 0.3
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R5C2[i, j] <- contour_ICER(parameters)
  }
}

# R5C3 loop (changing first parameter to 0.6)
fixed_params[1] <- 0.6
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R5C3[i, j] <- contour_ICER(parameters)
  }
}

# R5C4 loop (changing first parameter to 0.8)
fixed_params[1] <- 0.8
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R5C4[i, j] <- contour_ICER(parameters)
  }
}

# R5C5 loop (changing first parameter to 0.95)
fixed_params[1] <- 0.95
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]
    R5C5[i, j] <- contour_ICER(parameters)
  }
}

########
# Define common column names (risk of Rabies), row names (prob start PEP), and ICER names for INNER MATRICES
col_names <- c("0.6", "1.6", "5.7", "11.4", "19.0")
row_names <- c("0.9", "0.7", "0.5", "0.3", "0.1")
icer_names <- rep("ICER", 25)

# List of 25 matrices, each representing a different combination of rabies risk and PEP access,
# where column names are rabies risk levels and row names are PEP access
matrix_list <- list(R1C1, R1C2, R1C3, R1C4, R1C5, 
                    R2C1, R2C2, R2C3, R2C4, R2C5,
                    R3C1, R3C2, R3C3, R3C4, R3C5,
                    R4C1, R4C2, R4C3, R4C4, R4C5,
                    R5C1, R5C2, R5C3, R5C4, R5C5)

# Apply the same column names (rabies risk), row names (PEP access), and ICER labels to each matrix in the list using a loop
for (i in 1:length(matrix_list)) {
  colnames(matrix_list[[i]]) <- col_names
  rownames(matrix_list[[i]]) <- row_names
  names(matrix_list[[i]]) <- icer_names
}

# After modifying the matrices, reassign them to their respective variables (e.g., R1C1, R1C2, ...)
# so that they can be used in further calculations or visualizations
R1C1 <- matrix_list[[1]] ;R1C2 <- matrix_list[[2]]; R1C3 <- matrix_list[[3]]; R1C4 <- matrix_list[[4]]; R1C5 <- matrix_list[[5]]
R2C1 <- matrix_list[[6]] ;R2C2 <- matrix_list[[7]]; R2C3 <- matrix_list[[8]]; R2C4 <- matrix_list[[9]]; R2C5 <- matrix_list[[10]]
R3C1 <- matrix_list[[11]];R3C2 <- matrix_list[[12]];R3C3 <- matrix_list[[13]];R3C4 <- matrix_list[[14]];R3C5 <- matrix_list[[15]]
R4C1 <- matrix_list[[16]];R4C2 <- matrix_list[[17]];R4C3 <- matrix_list[[18]];R4C4 <- matrix_list[[19]];R4C5 <- matrix_list[[20]]
R5C1 <- matrix_list[[21]];R5C2 <- matrix_list[[22]];R5C3 <- matrix_list[[23]];R5C4 <- matrix_list[[24]];R5C5 <- matrix_list[[25]]







