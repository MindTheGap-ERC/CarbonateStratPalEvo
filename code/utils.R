## Utility functions

myNormStasis <- function(t,
                         mean = 0,
                         sd = 1) {
#' 
#' @title simulate evol. stasis of lineage
#' 
#' @param t: numeric vector - where is the lineage sampled?
#' @param mean: mean trait value
#' @param sd : standard deviatino of trait value
#' 
#' @note stasis is simulated aS iid normal distribution with specified mean and sd
#' 
#' @return a list with elements "time" and "traitValues"
#' 
  traitValues <- rnorm(
    n = length(t),
    mean = mean,
    sd = sd
  )
  traitList <- list(
    time = t,
    traitValue = traitValues
  )
  return(traitList)
}

myBD <- function(t,
                 mu = 0,
                 sigma = 1) {
  #' 
  #' @title simulate lineage evolving according to Brownian drift and special cases (Brownian motion)
  #' 
  #' @param t: numeric vector - where is the lineage sampled?
  #' @param mu: how large is the directionsality?
  #' @param sigma : numeric. strength of random fluctuations
  #' 
  #' @return a list with elements "time" and "traitValues"
  #' 
  variance <- diff(t)
  # Calculates the deviation from the square root of variance
  standardDeviation <- sqrt(variance)
  # Generates a random number according to the deviation calculated earlier.
  increments <- rnorm(
    n = length(t) - 1,
    mean = 0,
    sd = standardDeviation
  )
  # Generates the values of BM by adding all the random numbers together.
  brownMotion <- cumsum(c(0, increments))
  
  ## PART 2: Calculate the Brownian drift based on the Brownian motion
  # Calculates the drift
  traitValues <- mu * t + sigma * brownMotion
  
  traitList <- list(
    time = t,
    traitValue = traitValues
  )
  ## output
  return(traitList)
}
  
simulateTraitEvo <- function(t,
                             mode,
                             ...) {
  
  #' @title simulate trait evolution of a lienage
  #' 
  #' @param t: time where the lineage is sampled
  #' @param mode: string, either "BD" (brownian drift) or "Stasis"(stasis)\
  #' @param ... : model parameters to be handed over to the functions myNormStasis and myBD
  #' 
  #' @note wrapper around myNormStasis & myBD
  #' 
  #' @return : a list with elements "time"and "traitValues"

  if (mode == "BD") {
    outVal <- myBD(t, ...)
    return(outVal)
  }
  if (mode == "Stasis") {
    outVal <- myNormStasis(t, ...)
    return(outVal)
  } else {
    stop("unknown mode of evolution")
  }
}
  

getSampleAge <- function(sampleLocations,
                         ADMHeight,
                         ADMTime) {
  
  #' 
  #' @title for sample locations, determine their time of deposition
  #' 
  #' @param sampleLocations : numeric vector. strat positions where samples are take
  #' @param ADMHeight height tie points of the age-depth mode
  #' @param ADMTime time tie points of the age-depth model
  #' 
  #' @return a vector of times
  #' 
  adjustADMHeight <- ADMHeight[!duplicated(ADMHeight)] # Adjust height by removing duplicates
  adjustADMTime <- ADMTime[!duplicated(ADMHeight)] # Adjust time by removing values where height is duplicated
  
  # Transforming the sampling locations into the time domain
  transVals <- DAIME::pointtransform(
    points = sampleLocations,
    xdep = adjustADMHeight,
    ydep = adjustADMTime,
    direction = "height to time",
    depositionmodel = "age model"
  )
  
  sampleAge <- transVals$time
  return(sampleAge)
}
  
  

getTraitValInStrat <- function(distBetweenSamples,
                               ADMHeight,
                               ADMTime,
                               mode,
                               ...) {
  
  #' 
  #' @title simulate trait evolution in strat domain
  #' 
  #' @param distBetweenSamples numeric, how frequent are samples taken
  #' @param ADMHeight stratigraphic tiep oints of the ADM
  #' @param ADMTime tie points in time of the ADM
  #' @param mode "Stasis" or "BD". mode of evolution to be simulated. handed over to simulateTraitEvo
  #' @param ... model parameters for simulateTraitEvo
  #' 

  stopifnot(distBetweenSamples <= max(ADMHeight)) # check whether section is too short to be sampled at all
  sampleLocations <- seq(
    from = distBetweenSamples, # determine sampling locations
    to = max(ADMHeight),
    by = distBetweenSamples
  )
  sampleTimes <- getSampleAge(
    sampleLocations = sampleLocations, # determine ages of sampling locations
    ADMHeight = ADMHeight,
    ADMTime = ADMTime
  )
  traitInfo <- simulateTraitEvo(
    t = sampleTimes, # simulate trait values at the age that corresponds to the sampling locations
    mode = mode,
    ...
  )
  traitInfo$height <- sampleLocations # assign location info to simulated trait values
  return(traitInfo)
}
  
testModesOfEvolStrat <- function(distBetweenSamples,
                                 specimensPerSample,
                                 interPopVar,
                                 noOfTests,
                                 ADMHeight,
                                 ADMTime,
                                 inputMode,
                                 ...) {
  
  #' 
  #' @title identify modes of evolution in stratigraphic domain
  #' 
  #' @param distBetweenSamples numeric, how frequently are sampels take
  #' @param specimensPersample how many specimens are collected at each sampling locatioin
  #' @param interPopVar sd of traits found at one sampling location
  #' @noOfTests integer, how many lineages are simulated and tested
  #' @param ADMHeight stratigraphic tiep oints of the ADM
  #' @param ADMTime tie points in time of the ADM
  #' @param inputMode "Stasis" or "BD". mode of evolution to be simulated. handed over to simulateTraitEvo
  #' @param ... model parameters for simulateTraitEvo
  

  testResultList <- vector(mode = "list", length = noOfTests)
  for (i in 1:noOfTests) {
    traitValInStrat <- getTraitValInStrat(
      distBetweenSamples,
      ADMHeight,
      ADMTime,
      mode = inputMode,
      ...
    )
    ## define paleots object, see https://cran.r-project.org/web/packages/paleoTS/vignettes/paleoTS_basics.html
    fossilTimeSeries <- paleoTS::as.paleoTS(
      mm = traitValInStrat$traitValue, # mean value is the simulated trait values
      vv = rep(interPopVar, length.out = length(traitValInStrat$traitValue)), # variance around mean is interPopVar
      nn = rep(specimensPerSample, length.out = length(traitValInStrat$traitValue)), # no of specimens per site is specimensPerSample
      tt = traitValInStrat$height
    )
    # strat position
    # fit models to paleots object
    w.grw <- paleoTS::fitSimple(fossilTimeSeries, model = "GRW") # General Random walk (Brownian drift)
    w.urw <- paleoTS::fitSimple(fossilTimeSeries, model = "URW") # Unbiased Random Walk (Brownian motion)
    w.stat <- paleoTS::fitSimple(fossilTimeSeries, model = "Stasis") # Stasis
    w.ou <- paleoTS::fitSimple(fossilTimeSeries, model = "OU") # Ornstein-Uhlenbeck model
    
    # This compares the models, the most likely model has the highest akaike.wt and the smallest AICc values.
    compared <- paleoTS::compareModels(w.grw, w.urw, w.stat, w.ou, silent = TRUE, sort = FALSE)
    
    # store test results and input data
    testRes <- list(
      testRes = compared, # results of model comparison
      inputData = traitValInStrat, # raw data used as input
      paleots = fossilTimeSeries
    ) # paleots object
    testResultList[[i]] <- testRes
    print(paste(as.character(i), "/", as.character(noOfTests), " tests performed", sep = ""))
  }
  print("done")
  return(testResultList)
}
  

testModesOfEvolTime <- function(maxTime,
                                noOfSamplingLocations,
                                specimensPerSample,
                                interPopVar,
                                noOfTests,
                                inputMode,
                                ...) {
  #' @title identify modes of evolution in time domain
  #' 
  #' @param maxTime : numeric, for how long should the trait evol be simulated
  #' @param noOfSamplingLocations : numeric, how ofthen should the lineage be sampled
  #' @param specimensPerSample : integer > 1. how many specimens are observed at each sampling time?
  #' @param interPopVar : numeric > 0, standard deviation of traits within a sampled population
  #' @param noOfTests : integer , how many lineages are simulated and tested?
  #' @param inputMode : character, either "BD" or "Stasis". Handed over to simulateTraitEvo
  #' @param ... : parameters handed over to simulateTraitEvo
  #' 
  #' @note calls simulateTraitEvo to simulate trait evo along lineages, then runs
  #' standard tests from the paleoTS package on the results
  #' 
  #' @return a lost with test results. see code book for description of its structure
  #' 
  testResultList <- vector(mode = "list", length = noOfTests)
  for (i in seq_len(noOfTests)) {
    # simulate trait values in time
    sampling_times = seq(
      from = 0,
      to = maxTime,
      length.out = noOfSamplingLocations)
    
    traitValInTime <- simulateTraitEvo(
      t = sampling_times, 
      mode = inputMode,
      ...
    )
    # turn simulations into paleoTS object
    fossilTimeSeries <- paleoTS::as.paleoTS(
      mm = traitValInTime$traitValue,
      vv = rep(x = interPopVar, length.out = length(traitValInTime$traitValue)),
      nn = rep(x = specimensPerSample, length.out = length(traitValInTime$traitValue)),
      tt = traitValInTime$time
    )
    
    w.grw <- paleoTS::fitSimple(
      y = fossilTimeSeries,
      model = "GRW"
    ) # General Random walk (Brownian drift)
    w.urw <- paleoTS::fitSimple(
      y = fossilTimeSeries,
      model = "URW"
    ) # Unbiased Random Walk (Brownian motion)
    w.stat <- paleoTS::fitSimple(
      y = fossilTimeSeries,
      model = "Stasis"
    ) # Stasis
    w.ou <- paleoTS::fitSimple(
      y = fossilTimeSeries,
      model = "OU"
    ) # Ornstein-Uhlenbeck model
    
    # This compares the models, the most likely model has the highest akaike.wt and the smallest AICc values.
    compared <- paleoTS::compareModels(w.grw,
                                       w.urw,
                                       w.stat,
                                       w.ou,
                                       silent = TRUE,
                                       sort = FALSE
    )
    
    # store test results and input data
    testResultList[[i]] <- list(
      testRes = compared,
      inputData = traitValInTime,
      paleots = fossilTimeSeries
    )
    print(paste(as.character(i), "/", as.character(noOfTests), " tests with ", noOfSamplingLocations, " sampling locations performed", sep = ""))
  }
  print("done")
  return(testResultList)
}


get_completeness = function(pos, scenario){
  #'
  #'@title Get strat completeness at specific position in basin
  #'
  #'@param: String, "A" or "B". Scenario of interest
  #'@param: String, element of all_dist
  #'
  #'@returns numeric, strat. completeness as fraction
  #'
  #' @example  
  #' load("data/R_outputs/ageDepthModelsScenariosAandB.Rdata")
  #' get_completeness(pos = "2 km", scenario = "A")
  #' 
  
  time_interval = diff(range(ageDepthModels[[scenario]][[pos]]$time))
  completeness =  1 - sum(hiatus_list[[scenario]][[pos]]$hiatus_duration)/(time_interval)
  
  return(completeness)
}

get_hiatus_distribution = function(pos, scenario){
  #'
  #'@title get distribution of hiatus durations in myr
  #'
  #'@note requires age-depth model data in the workspace, so will only run after 
  #'load("data/R_outputs/ageDepthModelsScenariosAandB.Rdata") is executed at least once
  #'
  #'@param, String, "A", or "B", scenario of interest
  #'@param string, element of all_dist. from shore, e.g. "2 km"
  #'
  #'@example
  #'load("data/R_outputs/ageDepthModelsScenariosAandB.Rdata")
  #'get_hiatus_distribution(pos = "2 km', scenario = "A")
  #'
  #'@returns numeric vector of hiatus durations in specified basin and dist from shore
  #'
  #'
  hiat_distr = hiatus_list[[scenario]][[pos]]$hiatus_duration
  
  return(hiat_distr)
}



get_sample_locations = function(scenario, distance_from_shore_km, distanceBetweenSamples){
  #' 
  #' @title determine sampling positions 
  #' 
  #' @param scenario: "A" or "B"
  #' @param distance_from_shore_km : element of all_dist
  #' @param distanceBetweenSamples : numeric, distance between samples taken in m
  #' 
  #' needs age-depth models to be loaded, meaning 
  #' load("data/R_outputs/ageDepthModelsScenariosAandB.Rdata")
  #' need to be executed before the function can be used
  #'
  #' @example get_sample_locations(scenario = "A", distance_from_shore_km = "0.2 km", distanceBetweenSamples = 2) 
  #' 
  #' @returns a vector of stratigraphic positions where samples are taken
  
  sample_pos = seq(from = distanceBetweenSamples,
                   to = max(ageDepthModels[[scenario]][[distance_from_shore_km]]$height),
                   by = distanceBetweenSamples)
  
  return(sample_pos)
}

get_sample_times = function(scenario, pos){
  
  #'
  #' @title determine timing of samples taken as specified basin position
  #' 
  #' @param scenario : "A" or "B"
  #' @param pos : distance from shore, elem,ent of all_dist
  #' 
  #' @example  get_sample_times(scenario = "A", pos = "2 km")
  #' 
  #' @note needs both age-depth models & test parameters to be loaded, meaning 
  #' load("data/R_outputs/ageDepthModelsScenariosAandB.Rdata")
  #' and 
  #' load("data/R_outputs/parameters_for_tests.Rdata") need to be executed before 
  #' the function can be used
  #' 
  #' @return a vector, containing the time at which the samples were formed
  #' 
  sample_height = seq(distanceBetweenSamples, max(ageDepthModels[[scenario]][[pos]]$height), by = distanceBetweenSamples)
  times = approx(x = ageDepthModels[[scenario]][[pos]]$height, ageDepthModels[[scenario]][[pos]]$time, xout = sample_height,ties = mean)$y
  return(times)
}
