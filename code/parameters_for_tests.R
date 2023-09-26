## Define parameters used for the tests

#### Import Age-Depth models ####
load("data/R_outputs/ageDepthModelsScenariosAandB.Rdata")

#### load utility functions ####
source("code/utils.R")

#### seed for the simulation & test run ####
sim_seed = 1

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

specimensPerSample <- 100 # no of specimens found at one sampling site
interPopVar <- 0.1 # variance in traits at one sampling location around the simulated mean trait value
noOfTests <- 100 # no of tests performed per basin position

examinedBasinPositions <- c("2 km", "6 km", "8 km", "10 km", "12 km") # distance from shore in km where the tests will be performed
distanceBetweenSamples <- 1 # m between sampling sites in the section

#### time series length ####
#length of stratigraphic series at different distances from shore

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
    sampleLocations <- get_sample_locations(scenario = scenario,
                                            distance_from_shore_km = dist,
                                            distanceBetweenSamples = distanceBetweenSamples)
    ts_length_mat[scenario, dist] = length(sampleLocations)
  }
}

# time series length for benchmarks in time domain
noOfSamplingLoc_time = c("5", "10", "15", "20", "25", "35", "50", "100", "200")

# time series length for time domain, incl. direct comparisons
noOfSamplingLoc <- as.character(unique(sort(c(as.vector(ts_length_mat),as.numeric(noOfSamplingLoc_time))))) # how long is the time series? Times sampled are given by seq(0,maxTimes[[X]],length.out=noOfSamplingLoc[i])


save(sim_seed,
     EvoModes,
     simulatedEvoModes,
     specimensPerSample,
     interPopVar,
     noOfTests,
     examinedBasinPositions,
     distanceBetweenSamples,
     ts_length_mat,
     noOfSamplingLoc_time,
     noOfSamplingLoc,
     file = "data/R_outputs/parameters_for_tests.Rdata")
