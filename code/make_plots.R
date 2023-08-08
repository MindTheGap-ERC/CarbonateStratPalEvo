#### Get utility functions
source("code/utils.R")

#### fix random seed for debugging
set.seed(1)

#### load data ####
load("data/R_outputs/ageDepthModelsScenariosAandB.Rdata")
load("data/R_outputs/results_modes_of_evolution.Rdata")

#### Helper functions ####
get_AIC_scenario = function(scenario, simulated_mode){
  #' 
  #' @title Get AIC vals from model outpur for plotting
  #' 
  #' @param scenario: "A" or "B": the scenario of interest
  #' @param simulated_mode: "stasis", "Brownian motion", "weak Brownian drift", 
  #' or "strong Brownian drift". True (= simulated) mode of evolution for which AIC is supposed to be extracted
  #' 
  #' @return An opject that can be passed to ggplot to generate boxplots of AIC in strat domain
}

get_AIC_time = function(times, simulated_mode){
  #' 
  #' @title Get AIC vals from model output for plotting
  #' 
  #' @param times: char vector, subset of noOfSamplingLoc: number of sampling points
  #' @param simulated_mode: "stasis", "Brownian motion", "weak Brownian drift", 
  #' or "strong Brownian drift". True (= simulated) mode of evolution for which AIC is supposed to be extracted
  #' 
  #' @description combine outputs from the the function get_AIC_no_of_sampl_loc
  #' 
  #' @return An object that can be passed to ggplot to generate boxplots of AIC in time domain
}

get_AIC_no_of_sampl_loc= function(no_of_sampl_loc, simulated_mode){
  #' 
  #' @title get AIC from test results for specified number of sampling locatinos
  #' 
  #' @param no_of_sampl_loc : string, element of noOfSamplingLoc
  #' @param simulated_mode: "stasis", "Brownian motion", "weak Brownian drift", 
  #' or "strong Brownian drift". True (= simulated) mode of evolution for which AIC is supposed to be extracted
  #' 
  #' @return some data structure that can be used by the function get_AIC_time
  #' 
  stopifnot(no_of_sampl_loc %in% noOfSamplingLoc) # check if data from sampl. location is available
}

#### Figure 4 ####
ts_lengths = as.character(ts_length_mat["A",])
names(ts_lengths) = colnames(ts_length_mat)

#### Figure 5 ####
ts_lengths = as.character(ts_length_mat["B",])
names(ts_lengths) = colnames(ts_length_mat)

#### Figure 6 ####
ts_lengths = noOfSamplingLoc_time
