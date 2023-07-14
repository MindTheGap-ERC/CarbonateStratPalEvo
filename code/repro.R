

set.seed(42)
run_sim_and_test = TRUE

if(run_sim_and_test){
  source("code/simulate_and_test_modes_of_evolution.R")
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

