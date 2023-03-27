#### load required packages ####

#### Import Age-Depth models ####
load("data/R_outputs/age_depth_models.Rdata")

#### Random Seed ####
## Fix seed for repeatibility & debugging
# replace 42 by NULL if no fixed seed is required
set.seed(42)

#### Modes of evolution that will be tested for ####
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
) # B<M with mean zero and variance 1
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
noOfTests <- 1 # no of tests performed per basin position

## for tests on the strat domain only
examinedBasinPositions <- c("2 km", "6 km", "8 km", "10 km", "12 km") # distance from shore in km where the tests will be performed
distanceBetweenSamples <- 1 # m between sampling sites in the section

## For tests in time domain only:
maxTimes <- list(
  A = 2,
  B = 2.58
) # duration of scenarios A & B respectively in Ma
noOfSamplingLoc <- c("5", "10", "15", "20", "25", "35", "50", "100", "200") # how long is the time series? Times sampled are given by seq(0,maxTimes[[X]],length.out=noOfSamplingLoc[i])


#### Auxiliary Functions ####

{
  ## 1
  ## simulate stasis as independent identically distributed random variables with
  ## prescribed mean and standard deviation
  myNormStasis <- function(t,
                           mean = 0,
                           sd = 1) {
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
  ## 2
  ## simulate Brownian motion and drift
  ## based on the method that increments of a BM are normally distributed
  # The function that simulates Brownian motion with t=times, mu=mean and sigma= sd
  myBD <- function(t,
                   mu = 0,
                   sigma = 1) {
    ## PART 1: Brownian motion
    # Gets the variance of BM between values from t.
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

  ## 3
  ## wrapper around the myBM and myNormStasis functions for unified syntax
  simulateTraitEvo <- function(t,
                               mode,
                               ...) {
    # use mode="BD" to simulate brownian drift and motion
    # use mode = "Stasis" to simulate stasis
    # ... is where the model parameters go
    # !NOTE the model parameters must be named as expected by myNormStasis and myBD
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

  ## 4
  ## for a vector of sampling locations, determine the sample ages based on an age depth model
  getSampleAge <- function(sampleLocations,
                           ADMHeight,
                           ADMTime) {
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


  ## 5
  ## simulate trait values in the stratigraphic domain
  getTraitValInStrat <- function(distBetweenSamples,
                                 ADMHeight,
                                 ADMTime,
                                 mode,
                                 ...) {
    # distanceBetweenSamples: distance between samples, sampling is assumed to be equidistant in the section
    # ADMHeight, ADMTime, vectors describing the Age-depth model
    # mode either "BD" or "Stasis", specifies which mode of evolution is simulated
    # ... is where the model parameters for the mode of evolution go
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

  ## 6
  ## Fully automated test for mode of evolution in stratigraphic domain
  testModesOfEvolStrat <- function(distBetweenSamples,
                                   specimensPerSample,
                                   interPopVar,
                                   noOfTests,
                                   ADMHeight,
                                   ADMTime,
                                   inputMode,
                                   ...) {
    # distBetweenSamples specifies the distance between sampling locations in strat domain
    # specimensPerSample: no of specimens found at each sampling loaction
    # interPopVar: variance of traits around the simulated mean trait value at the sampling locations
    # noOfTests: number of test runs to execute
    # ADMHeight, ADMTime: vectors that specify the age-depth model
    # inputMode: simulated mode of Evolution, either "BD" or "Stasis"
    # ...: model parameters for the simulation of the trait
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

  ## 7
  ## Fully automated test for mode of evolution in the time domain
  testModesOfEvolTime <- function(maxTime,
                                  noOfSamplingLocations,
                                  specimensPerSample,
                                  interPopVar,
                                  noOfTests,
                                  inputMode,
                                  ...) {
    # maxTime: maximum time for trait sim
    # noOfSampling locations: how many points are in the time series. sampling locations are  given by seq(0,maxTime,length.out=noOfSamplingLocations)
    # specimensPerSample: no of specimens found at each sampling locction
    # interPopVar: variance of traits around the simulated mean trait value at the sampling locations
    # noOfTests: number of test runs to execute
    # inputMode: simulated mode of Evolution, either "BD" or "Stasis"
    # ... model parameters for the simulation of the trait
    testResultList <- vector(mode = "list", length = noOfTests)
    for (i in 1:noOfTests) {
      # simulate trait values in time
      traitValInTime <- simulateTraitEvo(
        t = seq(
          from = 0,
          to = maxTime,
          length.out = noOfSamplingLocations
        ), # simulate trait values at the age that corresponds to the sampling locations
        mode = inputMode,
        ...
      )
      # define paleoTS object
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
}

#### Run tests in Stratigraphic Domain ####

#!WARNING this will take a few hours to calculate !
testResultsStrat <- list()
for (scenario in scenarioNames) {
  scenarioRes <- list()
  for (dist in examinedBasinPositions) { ## for all examined transsects
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


#### Run tests in the time domain ####
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

save(testResultsStrat,
     testResultsTime,
     EvoModes,
     maxTimes,
     scenarioNames,
     specimensPerSample,
     interPopVar,
     noOfTests,examinedBasinPositions,
     distanceBetweenSamples,
     noOfSamplingLoc,
     simulatedEvoModes,
     testedEvoModes,
     file = "data/R_outputs/results_modes_of_evolution.Rdata")


#### Plot Age-Depth models ####
# for (scenario in scenarioNames){
#   for (dist in examinedBasinPositions){
#     jpeg(filename = paste("0 ADM scenario ",scenario, ", ",dist, " offshore.jpg",sep=""))
#     plot(x=ageDepthModels[[scenario]][[dist]]$time,
#          y=ageDepthModels[[scenario]][[dist]]$height,
#          xlab = "Time [Ma]",
#          ylab = "Height [m]",
#          type = "l",
#          main = paste("Scenario ", scenario,", ", dist, " offshore", sep = ""))
#     dev.off()
#   }
# }