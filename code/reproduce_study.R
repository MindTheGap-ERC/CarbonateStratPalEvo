## set seed for repeatability
set.seed(1)

## load utility functions
source("code/utils.R")

## import ADMs from matlab
process_adms = FALSE
if(process_adms){
  source("code/process_adm_from_matlab.R")
}

## run simulations and tests
run_sim_and_test = FALSE
if(run_sim_and_test){
  #! Warning, will take hours of computation time
  source("code/simulate_and_test_modes_of_evolution.R")
}

## make plots
make_plots = TRUE
if(make_plots){
  source("code/make_plots.R")
}


