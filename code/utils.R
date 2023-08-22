#### Utility functions for CarboStratPalEvo

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

get_completeness = function(pos, scenario){
  #'
  #'@title Get strat completeness at specific position in basin
  #'
  #'@param: String, "A" or "B". Scenario of interest
  #'@param: String, element of all_dist
  #'
  #'@returns numeric, strat. completeness as fraction
  #'
  #' @usage get_completeness(pos = "2 km", scenario = "A")
  #' 
  
  time_interval = diff(range(ageDepthModels[[scenario]][[pos]]$time))
  completeness =  1 - sum(hiatus_list[[scenario]][[pos]]$hiatus_duration)/(time_interval)
  
  return(completeness)
}

get_hiatus_distribution = function(pos, scenario){
  #'
  #'@title get distribution of hiatus durations in myr
  #'
  #'@param, String, "A", or "B", scenario of interest
  #'@param string, element of all_dist
  #'
  #'@returns numeric vector of hiatus durations in specified basin and dist from shore
  #'
  #'@usage get_hiatus_distribution(pos = "2 km", scenario = "A")
  #'
  hiat_distr = hiatus_list[[scenario]][[pos]]$hiatus_duration
  
  return(hiat_distr)
}





multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}