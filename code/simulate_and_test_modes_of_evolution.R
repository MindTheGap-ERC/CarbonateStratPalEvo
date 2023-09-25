## Run tests & simulations

#### Load age-depth models ####
load("data/R_outputs/ageDepthModelsScenariosAandB.Rdata")
load("data/R_outputs/parameters_for_tests.Rdata")

#### load utility functions ####
source("code/utils.R")

#### Seed run ####

{
  set.seed(sim_seed)
  sim_seed_used = sim_seed
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
      # run tests for specified scenario & distance from shore
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

    for (j in seq_along(EvoModes)) {
      print(paste("Testing ", EvoModes[[j]]$name, " with ", noSamp, " sampling locations, equidistantly spread over ", maxTimes[[scenario]], " Ma", sep = ""))
      # run tests for specified time duration (maxTimes[[scenario]]) & number of samples taken
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

save(testResultsStrat,
     testResultsTime,
     testedEvoModes,
     sim_seed_used,
     file = "data/R_outputs/results_modes_of_evolution.Rdata")

