#### load utility functions ####
source("code/utils.R")

#### download matlab data from osf ####
# requires files:
# none
# downloads files:
# - data/matlab_outputs/scenarioA_matlab_outputs.mat
# - data/matlab_outputs/scenarioB_matlab_outputs.mat
# - data/matlab_outputs/scenarioA_and_B_matlab_to_R.mat
# file size: < 200 Mb
download_data = FALSE
if(download_data){
  source("code/download_data_from_osf.R")
}

#### import ADMs from matlab ####
# requires files:
# - data/matlab_outputs/scenarioA_matlab_outputs.mat
# - data/matlab_outputs/scenarioB_matlab_outputs.mat
# - data/matlab_outputs/scenarioA_and_B_matlab_to_R.mat
# generates files:
# - ageDepthModelsScenariosAandB.RData
# file size: < 10 Mb
process_adms = FALSE
if(process_adms){
  source("code/process_adm_from_matlab.R")
}

#### run simulations and tests ####
# requires files:
# - ageDepthModelsScenariosAandB.RData
# generates files:
# - results_modes_of_evolution.RData
# file size: < 40 Mb
# computation time: hours to days!
run_sim_and_test = FALSE
if(run_sim_and_test){
  #! Warning, will take hours to compute
  source("code/simulate_and_test_modes_of_evolution.R")
}

#### make plots ####
# requires files:
# - results_modes_of_evolution.RData
# generates files:
# - multiple .pdf files in figs/R/
# file size: combined a few Mb
# computation time:
# - few minutes
make_plots = TRUE
if(make_plots){
  source("code/make_plots.R")
}


