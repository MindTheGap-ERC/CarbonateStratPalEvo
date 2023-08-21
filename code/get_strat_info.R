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

hiat_measures = list()
for (scenario in scenarioNames){
  compl = rep(NA, length(all_dist))
  median_duration_myr = rep(NA, length(all_dist))
  first_quartile_duration_myr = rep(NA, length(all_dist))
  third_quartile_duration_myr = rep(NA, length(all_dist))
  max_duration_myr = rep(NA, length(all_dist))
  for (i in seq_along(all_dist)){
    compl[i] = get_completeness(pos = all_dist[i], scenario = scenario)
    hiat_distr = get_hiatus_distribution(pos = all_dist[i], scenario = scenario)
    median_duration_myr[i] = median(hiat_distr)
    first_quartile_duration_myr[i] = quantile(hiat_distr, 0.25)
    third_quartile_duration_myr[i] = quantile(hiat_distr,0.75)
    max_duration_myr[i] = max(hiat_distr)
    
  }
  li = list()
 li[["completeness"]] = compl
  li[["median_duration_myr"]] = median_duration_myr
  li[["first_quartile_duration_myr"]] = first_quartile_duration_myr
  li[["third_quartile_duration_myr"]] = third_quartile_duration_myr
  li[["max_duration_myr"]] = max_duration_myr
  hiat_measures[[scenario]] = li
}


save("hiatus_list","compl_matrix","all_dist", "hiat_distr", file = "./data/R_outputs/hiatus_info.RData")


