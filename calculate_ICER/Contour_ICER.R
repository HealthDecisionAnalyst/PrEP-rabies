#### This script creates a series of 5x5 inner matrices (R1C1, R1C2, ..., R5C5) representing combinations of rabies risk and PEP (Post-Exposure Prophylaxis) access probabilities. 
## These matrices are embedded within an outer 5x5 matrix, each corresponding to different "cost of rabies PrEP per person" and "efficacy of PrEP in preventing rabies without PEP" combinations.

## The script generates and fills 25 inner 5x5 matrices, where:
## Inner matrix structure:
##  - Rows represent different levels of PEP access (0.9, 0.7, 0.5, 0.3, 0.1)
##  - Columns represent rabies risk levels (0.6, 1.6, 5.7, 11.4, 19.0)
## The "contour_ICER" function is applied to calculate the Incremental Cost-Effectiveness Ratio (ICER) for each matrix cell, representing all combinations of rabies risk and PEP access
## Outer matrix structure:
## - Rows: "Cost of rabies PrEP per person" (2, 3, 5, 15, 45)
## - Columns: "Efficacy of PrEP in preventing rabies without PEP" (0, 0.3, 0.6, 0.8, 0.95)
## - Each outer matrix corresponds to a set of ICER calculations for the respective rabies risk and PEP access levels

# Initial setup of 5x5 inner matrices for each [Rabies risk X PEP access] combination
# Creating a base matrix (R1C1) and replicating it for other matrices
R1C1 <-matrix(0, nrow = 5, ncol=5, byrow=TRUE)  
# All initial inner matrices created as a copy of [R1xC1]
R1C2 <-R1C1; R1C3 <-R1C1; R1C4 <-R1C1; R1C5 <-R1C1
R2C1 <-R1C1; R2C2 <-R1C1; R2C3 <-R1C1; R2C4 <-R1C1; R2C5 <-R1C1
R3C1 <-R1C1; R3C2 <-R1C1; R3C3 <-R1C1; R3C4 <-R1C1; R3C5 <-R1C1
R4C1 <-R1C1; R4C2 <-R1C1; R4C3 <-R1C1; R4C4 <-R1C1; R4C5 <-R1C1
R5C1 <-R1C1; R5C2 <-R1C1; R5C3 <-R1C1; R5C4 <-R1C1; R5C5 <-R1C1


##### Source the function for generating contour plotting
source("Function_contour_ICER.R")

##### Set up parameters for contour plotting
### Define fixed parameters for outer matrices (R1C1 to R5C5)
fixed_params <- c(0, 0.0001, 0.3, 0.19, 0.94, 1, 45, 0, 0)

### Parameters for the R1C1 inner matrix
parameters <- c(0, 0.0001, 0.3, 0.19, 0.9, 0.94, 1, 45, 0, 0) ;R1C1[1,1] <-contour_ICER(parameters)
# PEP access probabilities and rabies risk values for matrix loops
phi_values <- c(0.9, 0.7, 0.5, 0.3, 0.1)         # Rows: PEP access
theta_values <- c(0.0001, 0.001, 0.003, 0.01)    # Columns: Rabies risk levels

# Loop to fill the R1C1 matrix with ICER values
for (j in 1:length(theta_values)) {
  for (i in 1:length(phi_values)) {
    parameters <- c(fixed_params[1:4], phi_values[i], fixed_params[5:9])
    parameters[2] <- theta_values[j]             # Update rabies risk (theta)
    R1C1[i, j] <- contour_ICER(parameters)
  }
}

# Repeat the process for other inner matrices by updating specific parameters
# Example: R1C2 loop (changing first parameter to 0.3)
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

### Fixed parameters for R2C1,R2C2,R2C3,R2C4,R2C5
fixed_params <- c(0, 0.0001, 0.3, 0.19, 0.94, 1, 15, 0, 0)
# Parameters for R2C1
parameters <- c(0, 0.0001, 0.3, 0.19, 0.9, 0.94, 1, 15, 0, 0) ; R2C1[1,1] <-contour_ICER(parameters)

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

### Fixed parameters for R3C1,R3C2,R3C3,R3C4,R3C5
fixed_params <- c(0, 0.0001, 0.3, 0.19, 0.94, 1, 5, 0, 0)
# Parameters for R3C1
parameters <- c(0, 0.0001, 0.3, 0.19, 0.9, 0.94, 1, 5, 0, 0) ; R3C1[1,1] <-contour_ICER(parameters)

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

## Fixed parameters for R4C1,R4C2,R4C3,R4C4,R4C5
fixed_params <- c(0, 0.0001, 0.3, 0.19, 0.94, 1, 2, 0, 0)
# Parameters for R4C1
parameters <- c(0, 0.0001, 0.3, 0.19, 0.9, 0.94, 1, 2, 0, 0) ; R4C1[1,1] <-contour_ICER(parameters)

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

## Fixed parameters for R5C1,R5C2,R5C3,R5C4,R5C5
fixed_params <- c(0, 0.0001, 0.3, 0.19, 0.94, 1, 1, 0, 0)
# Parameters for R5C1
parameters <- c(0, 0.0001, 0.3, 0.19, 0.9, 0.94, 1, 1, 0, 0) ; R5C1[1,1] <-contour_ICER(parameters)

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

### Define column names (risk of Rabies), row names (prob start PEP), and ICER names for INNER MATRICES
col_names <- c("0.6", "1.6", "5.7", "11.4", "19.0")
row_names <- c("0.9", "0.7", "0.5", "0.3", "0.1")
icer_names <- rep("ICER", 25)

# List of 25 matrices representing each combination of rabies risk and PEP access,
# where column names are rabies risk levels and row names are PEP access
matrix_list <- list(R1C1, R1C2, R1C3, R1C4, R1C5, 
                    R2C1, R2C2, R2C3, R2C4, R2C5,
                    R3C1, R3C2, R3C3, R3C4, R3C5,
                    R4C1, R4C2, R4C3, R4C4, R4C5,
                    R5C1, R5C2, R5C3, R5C4, R5C5)

# Apply row/column names and ICER labels to each matrix
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







