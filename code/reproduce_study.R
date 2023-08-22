## set seed for repeatability
set.seed(1)

## load utility functions
source("code/utils.R")

## download data from osf
download_data = FALSE
if(download_data){
  source("code/download_data_from_osf.R")
}

## import ADMs from matlab
process_adms = FALSE
if(process_adms){
  source("code/process_adm_from_matlab.R")
}

## run simulations and tests
run_sim_and_test = FALSE
if(run_sim_and_test){
  #! Warning, will take hours to compute
  source("code/simulate_and_test_modes_of_evolution.R")
}

## make plots
make_plots = TRUE
if(make_plots){
  source("code/make_plots.R")
}


