

#parameters
source(paste0(here::here(),"/0_config.R"))

minN=100
maxN=10000
N_datasets=10

set.seed(12345)
sample_sizes <- floor(runif(N_datasets, minN, maxN))
sample_seeds <- floor(runif(N_datasets, 1, 1000000))


data_samples=list()

for(i in 1:N_datasets){
  data_samples[[i]] <- generate_dgp_data(n=sample_sizes[i], fixed_A = NULL, seed=sample_seeds[i])
}

saveRDS(data_samples, file=paste0(here::here(), "/data/data_samples.R"))
