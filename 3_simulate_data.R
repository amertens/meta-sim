
rm(list=ls())
source(paste0(here::here(),"/0_config.R"))

data_samples=readRDS(paste0(here::here(), "/data/data_samples.R"))

#parameters
n_simulated_data=100


#simulate through 3 approaches:
#1) scramble Y and bootstrap

# Assuming 'data' is your original dataset with 'Y', 'A', 'W1', 'W2' columns

# Function to scramble Y and bootstrap
scramble_Y <- function(data, n_bootstraps) {
  data_list=list()
  for(i in 1:n_bootstraps){
    data$Y<-sample(data$Y, nrow(data), replace = FALSE, prob = NULL)
    data <- data[sample(1:nrow(data), nrow(data), replace = TRUE, prob = NULL),] 
    rownames(data) = NULL
    data_list[[i]] <- data
  }

  return(data_list)
}

# Example usage
set.seed(123)  # For reproducibility
bootstrap_sim_df<-list()
for(i in 1:length(data_samples)){
  bootstrap_sim_df[[i]]<- scramble_Y(data_samples[[i]], n_bootstraps=n_simulated_data)
}


#2) Fit regression with set A-Y relationship and use lava to predict from it




gen_parameteric_data <- function(data){
  n=nrow(data)
  W1 <- rnorm(n, mean(data$W1), sd(data$W1))
  W2 <- rnorm(n, mean(data$W2), sd(data$W2))
  A <- rbinom(n, 1, prob = plogis(mean(data$A)))
  Y <- rep(NA,n)
  for(i in 1:n){
    Y[i] <- rbinom(1, 1, prob = plogis(-0.5*data$A[i] + data$W1[i] + data$W2[i]))   
  }
  df=data.frame(Y,A,W1,W2)
  return(df)
}

#true RR: 0.6065307
set.seed(1234)
parametric_sim_list <- list()
for(i in 1:length(data_samples)){
  sim_list<-list()
  for(j in 1:length(n_simulated_data)){
    sim_list[[j]] <- gen_parameteric_data(data_samples[[i]])
  }
  parametric_sim_list[[i]] <- sim_list
}



#3) SL plasmode simulation
library=c("SL.mean", "SL.glm", "SL.glmnet","SL.randomForest")
library=c("SL.glm")

SL_sim_df <- data_samples

sim_SL <- function(SL_sim_df, n_df, library){

  SL_model <- SuperLearner(Y = SL_sim_df$Y, X = SL_sim_df[, -1], SL.library = library, family = binomial())
  sim_df <- SL_sim_df
  sim_df_list <- list()
  for(i in 1:length(n_df)){
    predictions <- predict(SL_model, newdata = SL_sim_df)
    simulated_Y <- rbinom(nrow(SL_sim_df), 1, as.numeric(predictions$pred))
    sim_df$Y <- simulated_Y
    sim_df_list[[i]] <- sim_df
  }
  return(sim_df_list)
}

SL_sim_df <- list()
for(i in 1:length(data_samples)){
  SL_sim_df[[i]] <- sim_SL(data_samples[[i]], n_df=n_simulated_data, library=library)
}




#NOTE! Need to update to make many data.frames per simulated data

save(SL_sim_df, parametric_sim_list, bootstrap_sim_df, file=paste0(here::here(), "/data/sim_data.R"))
