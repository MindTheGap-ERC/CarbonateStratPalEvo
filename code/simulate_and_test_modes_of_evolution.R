#### Import Age-Depth models ####
load("data/R_outputs/ageDepthModelsScenariosAandB.Rdata")

#### load utility functions ####
source("code/utils.R")

#### Modes of evolution that will be simulated ####
# name is the name used for output
# mode is the string passed to the subroutines, either "BD" or "Stasis"
# params are the parameters handed over to the simulation of the traits
EvoModes <- list()
EvoModes[[1]] <- list(
  name = "stasis",
  mode = "Stasis",
  params = c(0, 1)
) # stasis with mean 0, variance 1
EvoModes[[2]] <- list(
  name = "Brownian motion",
  mode = "BD",
  params = c(0, 1)
) # BM with mean zero and variance 1
EvoModes[[3]] <- list(
  name = "weak Brownian drift",
  mode = "BD",
  params = c(5, 1)
) # BD with drift 5 and variance 1
EvoModes[[4]] <- list(
  name = "strong Brownian drift",
  mode = "BD",
  params = c(10, 1)
) # BD with drift 10 and variance 1

# extract names of the simulated evolutionary modes
simulatedEvoModes <- sapply(EvoModes, function(x) x$name)


#### Global Test Options ####
scenarioNames <- names(ageDepthModels)
specimensPerSample <- 100 # no of specimens found at one sampling site
interPopVar <- 0.1 # variance in traits at one sampling location around the simulated mean trait value
noOfTests <- 100 # no of tests performed per basin position

examinedBasinPositions <- c("2 km", "6 km", "8 km", "10 km", "12 km") # distance from shore in km where the tests will be performed
distanceBetweenSamples <- 1 # m between sampling sites in the section

maxTimes = list() # duration of scenarios respectively in Ma
for (scenario in scenarioNames){
  maxTimes[[scenario]] = max(ageDepthModels[[scenario]][[examinedBasinPositions[1]]][["time"]])
}

#### time series length info ####
ts_length_mat = matrix(
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
    sampleLocations <- seq(
      from = distanceBetweenSamples, # determine sampling locations
      to = max(ageDepthModels[[scenario]][[dist]]$height),
      by = distanceBetweenSamples
    )
    ts_length_mat[scenario, dist] = length(sampleLocations)
  }
}

# time series length for benchmarks in time domain
noOfSamplingLoc_time = c("5", "10", "15", "20", "25", "35", "50", "100", "200")

# time series length for time domain, incl. direct comparisons
noOfSamplingLoc <- as.character(unique(sort(c(as.vector(ts_length_mat),as.numeric(noOfSamplingLoc_time))))) # how long is the time series? Times sampled are given by seq(0,maxTimes[[X]],length.out=noOfSamplingLoc[i])


#### Completeness and hiatus duration ####
hiatus_list = list()
all_dist = head(names(ageDepthModels[[scenario]]),-1)
for (scenario in scenarioNames){
  res_list = list()
  
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


#### Stratigraphic Domain Tests ####
#!WARNING this will take a few hours to calculate !
testResultsStrat <- list()
for (scenario in scenarioNames) {
  scenarioRes <- list()
  for (dist in examinedBasinPositions) {
    # get Age depth models
    ADMHeight <- ageDepthModels[[scenario]][[dist]]$height
    ADMTime <- ageDepthModels[[scenario]][[dist]]$time

    distFromShoreRes <- list()
    distFromShoreRes$distanceFromShore <- ageDepthModels[[scenario]][[dist]]$distanceToShore

    for (j in seq_along(EvoModes)) {
      print(paste("Testing ", EvoModes[[j]]$name, " ", as.character(dist), " offshore in scenario ", scenario, sep = ""))
      distFromShoreRes[[EvoModes[[j]]$name]] <- testModesOfEvolStrat(
        distBetweenSamples = distanceBetweenSamples,
        specimensPerSample = specimensPerSample,
        interPopVar = interPopVar,
        noOfTests = noOfTests,
        ADMHeight = ADMHeight,
        ADMTime = ADMTime,
        inputMode = EvoModes[[j]]$mode,
        EvoModes[[j]]$params[1],
        EvoModes[[j]]$params[2]
      )
    }
    scenarioRes[[paste(as.character(dist), sep = "")]] <- distFromShoreRes
  }
  testResultsStrat[[scenario]] <- scenarioRes
}


#### Time domain tests ####
testResultsTime <- list()
for (scenario in scenarioNames) {
  scenarioTimeRes <- list()

  for (noSamp in noOfSamplingLoc) {
    noOfSamplingLocRes <- list()
    noOfSamplingLocRes$noOfSamplingLocations <- noSamp

    for (j in 1:length(EvoModes)) {
      print(paste("Testing ", EvoModes[[j]]$name, " with ", noSamp, " sampling locations, equidistantly spread over ", maxTimes[[scenario]], " Ma", sep = ""))
      noOfSamplingLocRes[[EvoModes[[j]]$name]] <- testModesOfEvolTime(
        maxTime = maxTimes[[scenario]],
        noOfSamplingLocations = as.numeric(noSamp),
        specimensPerSample = specimensPerSample,
        interPopVar = interPopVar,
        noOfTests = noOfTests,
        inputMode = EvoModes[[j]]$mode,
        EvoModes[[j]]$params[1],
        EvoModes[[j]]$params[2]
      )
    }
    scenarioTimeRes[[noSamp]] <- noOfSamplingLocRes
  }
  testResultsTime[[scenario]] <- scenarioTimeRes
}


#### Save Results ####

# names of the modes of evolution for which the model comparison was run
testedEvoModes=dimnames(testResultsTime[[scenarioNames[1]]][[noOfSamplingLoc[1]]][[simulatedEvoModes[1]]][[1]]$testRes$modelFits)[[1]]

save.image(file = "data/R_outputs/results_modes_of_evolution.Rdata")

