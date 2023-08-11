#### Get utility functions
source("code/utils.R")

#### fix random seed for debugging
set.seed(1)

#### Load required packages
require(DAIME)
require(paleoTS)
require(grid)
require("ggplot2")
require("RColorBrewer")
require("ggrepel")  
require("gridExtra")

#### load data ####
load("data/R_outputs/ageDepthModelsScenariosAandB.Rdata")
load("data/R_outputs/results_modes_of_evolution.Rdata")

#### Helper functions ####
get_AIC_scenario = function(basin, simulated_mode){
  
  #' 
  #' @title Get AIC vals from model output for plotting
  #' 
  #' @param basin: "A" or "B": the scenario of interest. 
  #' @param simulated_mode: "stasis", "Brownian motion", "weak Brownian drift", 
  #' or "strong Brownian drift". True (= simulated) mode of evolution for which AIC is supposed to be extracted
  #' 
  #' @return An object that can be passed to ggplot to generate boxplots of AIC in strat. domain

  stopifnot(basin %in% scenarioNames)
  stopifnot(simulated_mode %in% simulatedEvoModes)
#Creating an array ready for all the input data:
  AkaikeWtArrayStrat <- array( 
    data = NA,
    dim = c(length(examinedBasinPositions), length(testedEvoModes), noOfTests),
    dimnames = list(
      "basin_positions" = examinedBasinPositions,
      "tested_evo_modes" = testedEvoModes,
      "test" = NULL
    )
  )

#This part fills the just created array with the Akaike data:

    for (dist in examinedBasinPositions) {
        for (resMode in testedEvoModes) {
          for (i in 1:noOfTests) {
            AkaikeWtArrayStrat[dist, resMode, i] <- testResultsStrat[[basin]][[dist]][[simulated_mode]][[i]]$testRes$modelFits[resMode, "Akaike.wt"]
          }
        }
    }

  
#This part takes only the required Akaike data from the array and puts it in a format accepted by ggplot:  
  df=data.frame() 
  for (pos in examinedBasinPositions){
    for (testedEvoMode in testedEvoModes){
      df=rbind(df,data.frame(position=as.factor(rep(pos,noOfTests)),
                             testedEvoMode=rep(testedEvoMode,noOfTests),
                             AIC=AkaikeWtArrayStrat[pos,testedEvoMode,]))
    }
  }
  
  return(df) #The return, ready for input in ggplot.
}

 
get_AIC_no_of_sampl_loc= function(no_of_sampl_loc, simulated_mode, scenario){
  #' 
  #' @title get AIC from test results for specified number of sampling locations
  #' 
  #' @param no_of_sampl_loc : string, element of noOfSamplingLoc
  #' @param simulated_mode: "stasis", "Brownian motion", "weak Brownian drift", 
  #' or "strong Brownian drift". True (= simulated) mode of evolution for which AIC is supposed to be extracted
  #' 
  #' @return some data structure that can be used by the function get_AIC_time
  #' 
  stopifnot(no_of_sampl_loc %in% noOfSamplingLoc) # check if data from sampl. location is available
}

get_AIC_time = function(basin, simulated_mode){
  #' 
  #' @title Get AIC vals from model output for plotting
  #' 
  #' @param basin: "A" or "B": the scenario of interest. Named 'basin' instead of 'scenario due to the second already being used.
  #' @param simulated_mode: "stasis", "Brownian motion", "weak Brownian drift", 
  #' or "strong Brownian drift". True (= simulated) mode of evolution for which AIC is supposed to be extracted
  #' 
  #' @description combine outputs from the function get_AIC_no_of_sampl_loc
  #' 
  #' @return An object that can be passed to ggplot to generate boxplots of AIC in time domain
  #' 
  #' Notes to self(Joël):
  #' noOfSamplingLoc= All investigated lengths of time series
  #' ts_length_mat = The lengths of the time series in all locations in all basins, table form.
  #' no_of_sampl_loc is created in get_AIC_time for all locations and then used to get the correct AIC values.
  #
  
  
  ## Your code here
}

# the idea is that we can use the code as follows:
# data = get_AIC_time()
# ggplot(data,your arguments her)

#### Figure 4 ####
ts_lengths = as.character(ts_length_mat["A",])
names(ts_lengths) = colnames(ts_length_mat)

#### Figure 5 ####
ts_lengths = as.character(ts_length_mat["B",])
names(ts_lengths) = colnames(ts_length_mat)

#### Figure 6 ####
ts_lengths = noOfSamplingLoc_time
