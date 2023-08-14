load("data/R_outputs/ageDepthModelsScenariosAandB.Rdata")
load("data/R_outputs/results_modes_of_evolution.Rdata")


hiatus_list = list()
for (scenario in scenarioNames){
  res_list = list()
  all_dist = names(ageDepthModels[[scenario]])[1:150]
  for (dista in all_dist){
    adm = ageDepthModels[[scenario]][[dista]]
    hiatus_position = unique(adm$height[duplicated(adm$height)])
    hiat_time = c()
    for (i in  seq_along(hiatus_position)){
      hiat_time[i] = diff(range(adm$time[adm$height == hiatus_position[i]]))
    }
    res_list[[dista]] = list(hiatus_position = hiatus_position,
                            hiatus_duration = hiat_time)
  }
  hiatus_list[[scenario]] = res_list
}

compl_matrix = matrix(
  data = NA,
  nrow = length(scenarioNames),
  ncol = length(examinedBasinPositions),
  dimnames = list("scenario" = scenarioNames,
                  "basin_position" = examinedBasinPositions)
)

for (scenario in scenarioNames){
  for (pos in examinedBasinPositions){
    time_interval = diff(range(ageDepthModels[[scenario]][[pos]]$time))
    compl_matrix[scenario, pos] = 1 - sum(hiatus_list[[scenario]][[pos]]$hiatus_duration)/(time_interval)
  }
}

save("hiatus_list", file = "./data/R_outputs/hiatus_info.RData")


