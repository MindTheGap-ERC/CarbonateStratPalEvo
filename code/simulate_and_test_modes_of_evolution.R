#### Import Age-Depth models ####
load("data/R_outputs/ageDepthModelsScenariosAandB.Rdata")

#### Modes of evolution that will be simulated for ####
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
scenarioNames <- c("A", "B")
specimensPerSample <- 100 # no of specimens found at one sampling site
interPopVar <- 0.1 # variance in traits at one sampling location around the simulated mean trait value
noOfTests <- 100 # no of tests performed per basin position

examinedBasinPositions <- c("2 km", "6 km", "8 km", "10 km", "12 km") # distance from shore in km where the tests will be performed
distanceBetweenSamples <- 1 # m between sampling sites in the section

maxTimes = list() # duration of scenarios respectively in Ma
for (scenario in scenarioNames){
  maxTimes[[scenario]] = max(ageDepthModels[[scenario]][[examinedBasinPositions[1]]][["time"]])
} 

## time series length info
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
        EvoModes[[j]]$params[1], EvoModes[[j]]$params[2]
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

