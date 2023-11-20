
source(paste0(here::here(),"/0_config.R"))

# Example usage of the function
sample_size <- 100000
data <- generate_dgp_data(sample_size) # A generated based on W1 and W2
data_fixed_A1 <- generate_dgp_data(sample_size, fixed_A = 1) # A set to 1 for all observations
data_fixed_A0 <- generate_dgp_data(sample_size, fixed_A = 0) # A set to 0 for all observations

# View the first few rows of each dataset
head(data)
head(data_fixed_A1)
head(data_fixed_A0)

trueRD=mean(data_fixed_A1$Y) - mean(data_fixed_A0$Y) 
trueRR=mean(data_fixed_A1$Y) / mean(data_fixed_A0$Y) 

trueRD
trueRR
