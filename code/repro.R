## set seed for repeatibility
set.seed(1)

## load utility functions
source("code/utils.R")

## import ADMs from matlab
source("code/process_adm_from_matlab.R")

run_sim_and_test = FALSE

if(run_sim_and_test){
  #! Warning, will take hours of computation time
  source("code/simulate_and_test_modes_of_evolution.R")
}

make_plots = FALSE
if(make_plots){
  source("code/make_plots.R")
}



#### determine incompletenss at locatinos 
completeness_mat = matrix(
  data = NA,
  nrow = length(scenarioNames),
  ncol = length(examinedBasinPositions),
  dimnames = list(
    "scenarios" = scenarioNames,
    "basin_pos" = examinedBasinPositions
  )
)
for (dist in examinedBasinPositions){
  for (scenario in scenarioNames){
    completeness_mat[scenario, dist] = length(unique(ageDepthModels[[scenario]][[dist]]$height))/length(ageDepthModels[[scenario]][[dist]]$height)
  }
}

