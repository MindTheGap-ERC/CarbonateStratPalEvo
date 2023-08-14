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
load("data/R_outputs/hiatus_info.Rdata")

#### Helper functions ####
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


get_AIC_no_of_sampl_loc= function(no_of_sampl_loc, basin, simulated_mode){
  #' 
  #' @title get AIC from test results for specified number of sampling locations
  #' 
  #' @param no_of_sampl_loc : string, element of noOfSamplingLoc
  #' @param simulated_mode: "stasis", "Brownian motion", "weak Brownian drift", 
  #' or "strong Brownian drift". True (= simulated) mode of evolution for which AIC is supposed to be extracted
  #' @param basin: String, "A" or "B"
  #' 
  #' @return some data structure that can be used by the function get_AIC_time
  #' 
  stopifnot(no_of_sampl_loc %in% noOfSamplingLoc) # check if data from sampl. location is available
  stopifnot(basin %in% scenarioNames)
  stopifnot(simulated_mode %in% simulatedEvoModes)

#Creating an array ready for all the input data:  
  AkaikeWtArrayTime <- array(
    data = NA,
    dim = c(length(no_of_sampl_loc), length(testedEvoModes), noOfTests),
    dimnames = list(
      "number_sampling_loc"= no_of_sampl_loc,
      "tested_evo_modes" = testedEvoModes,
      "test" = NULL
    )
  )
  #This part fills the just created array with the Akaike data:
  for (p in no_of_sampl_loc) {
        for (resMode in testedEvoModes) {
          for (i in 1:noOfTests) {
            AkaikeWtArrayTime[p, resMode, i] <- testResultsTime[[basin]][[p]][[simulated_mode]][[i]]$testRes$modelFits[resMode, "Akaike.wt"]
          }
      }
    }
  
  return(AkaikeWtArrayTime)#The return, ready for input in get_AIC_time.
} 


get_AIC_time = function(no_of_sampl_loc, basin, simulated_mode){
  #' 
  #' @title Get AIC vals from model output for plotting
  #' 
  #'  @param no_of_sampl_loc : string, element of noOfSamplingLoc
  #' @param simulated_mode: "stasis", "Brownian motion", "weak Brownian drift", 
  #' or "strong Brownian drift". True (= simulated) mode of evolution for which AIC is supposed to be extracted
  #' @param basin: String, "A" or "B"
  #' 
  #' @description combine outputs from the function get_AIC_no_of_sampl_loc
  #' 
  #' @return An object that can be passed to ggplot to generate boxplots of AIC in time domain
  #' 
  
  #This part fills the just created array with the Akaike data:
  Akaike_time=get_AIC_no_of_sampl_loc(no_of_sampl_loc, basin, simulated_mode)
  
  #This part takes only the required Akaike data from the array and puts it in a format accepted by ggplot:  
  df=data.frame()
  for (nsp in no_of_sampl_loc){
    for (testedEvoMode in testedEvoModes){
      df=rbind(df,data.frame(nsp=as.factor(rep(nsp,length(Akaike_time[1,1,]))),
                             testedEvoMode=rep(testedEvoMode,length(Akaike_time[1,1,])),
                             AIC=Akaike_time[nsp,testedEvoMode,]))
      
    }
  }
  
  return(df) #The return, ready for input in ggplot.
  
}


Plot_Strat = function(basin, simulated_mode, label){
  #' 
  #' @title plots AIC values in boxplots for the stratigraphic domain
  #'
  #' @param simulated_mode: "stasis", "Brownian motion", "weak Brownian drift", 
  #' or "strong Brownian drift". True (= simulated) mode of evolution for which AIC is supposed to be extracted
  #' @param basin: String, "A" or "B"
  #' @param label: the label you want attached to the plot, String,"A" to "Z"
  #' 
  #' @description Plots AIC values collected by get_AIC_scenario.
  #' 
  #' @return A graph, ready to be combined with other graphs in the combined plot.
  #' 
  
  plotS1=ggplot2::ggplot(get_AIC_scenario(basin,simulated_mode), aes(x=position, y=AIC, fill=testedEvoMode)) + 
    geom_boxplot(outlier.shape = NA,lwd=0.1) +
    labs(tag = label)+
    scale_fill_brewer(palette="Spectral")+
    theme(legend.position="none", plot.title =element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),text = element_text(size = 6))+
    labs(title = paste(simulated_mode), y="AIC weight", x="Distance from Shore (km)")+
    scale_x_discrete( labels=c("2 km"="2","6 km"="6","8 km"="8","10 km"="10","12 km"="12"))+
    scale_y_continuous(limits=c(0,1))
  
  return(plotS1)
}

Plot_time = function(no_of_sampl_loc, basin, simulated_mode, label){
  #' 
  #' @title plots AIC values in boxplots for the stratigraphic domain
  #' 
  #'@param no_of_sampl_loc : string, element of noOfSamplingLoc
  #' @param simulated_mode: "stasis", "Brownian motion", "weak Brownian drift", 
  #' or "strong Brownian drift". True (= simulated) mode of evolution for which AIC is supposed to be extracted
  #' @param basin: String, "A" or "B"
  #' @param label: the label you want attached to the plot, String,"A" to "Z"
  #' 
  #' @description Plots AIC values collected by get_AIC_time.
  #' 
  #' @return A graph, ready to be combined with other graphs in the combined plot.
  #' 
  
  
  plotT1=ggplot2::ggplot(get_AIC_time(no_of_sampl_loc, basin, simulated_mode), aes(x=nsp, y=AIC, fill=testedEvoMode)) + 
    geom_boxplot(outlier.shape = NA,lwd=0.1) +
    labs(tag = label)+
    scale_fill_brewer(palette="Spectral")+
    theme(legend.position="none", plot.title =element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98), text = element_text(size = 8))+
    labs(title = paste(simulated_mode), y="AIC weight", x="Number of Sampling Points")+
    scale_y_continuous(limits=c(0,1))
  
  return(plotT1)
}

get_only_legend = function(plot) {
  #' 
  #' @title Get the legend for a set of plots
  #' 
  #' @param plot : a plot made in PlotLegend
  #' 
  #' @description creates a seperate legend required for the combined plot
  #' 
  #' @return An plot that can be used in the combined plot
  #' 
    # get tabular interpretation of plot
    plot_table <- ggplot_gtable(ggplot_build(plot)) 
    
    #  Mark only legend in plot
    legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
    
    # extract legend
    legend <- plot_table$grobs[[legend_plot]]
    
    # return legend
    return(legend) 
}

#Code to generate the reusable Legend
{
  PlotLegend=ggplot2::ggplot(get_AIC_time(noOfSamplingLoc_time, "A", "stasis"), aes(x=nsp, y=AIC, fill=testedEvoMode)) + 
  geom_boxplot(outlier.shape = NA,lwd=0.1)+ 
  scale_fill_brewer(palette="Spectral",name = "Tested Mode")+
  theme(plot.title =element_text(size = 10),text = element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),
        legend.text = element_text(size = 5),legend.title = element_text(size = 5), axis.title.y=element_blank())+
  theme(legend.position = "bottom" )

Legend=get_only_legend(PlotLegend)
}

#### Figure 4 ####
#Setting ts_lengths to the correct lengths
ts_lengths = as.character(ts_length_mat["A",])
names(ts_lengths) = colnames(ts_length_mat)

#The stratigraphic plots:
Plot4_1=Plot_Strat("A","stasis","A")
Plot4_2=Plot_Strat("A","Brownian motion","B")
Plot4_3=Plot_Strat("A","weak Brownian drift","C")
Plot4_4=Plot_Strat("A","strong Brownian drift","D")

#The time plots:
Plot4_5=Plot_time(ts_lengths,"A","stasis","E")
Plot4_6=Plot_time(ts_lengths,"A","Brownian motion","F")
Plot4_7=Plot_time(ts_lengths,"A","weak Brownian drift","G")
Plot4_8=Plot_time(ts_lengths,"A","strong Brownian drift","H")

#Combining all the plots:
combined_plot=grid.arrange(Plot4_1,Plot4_2,Plot4_3,Plot4_4,Plot4_5,Plot4_6,Plot4_7,Plot4_8, ncol=4)

#Printing the plots plus the legend to .pdf
{
  pdf(file = paste("figs/R/fig4_raw.pdf"), width=6.5, height = 3.25)
  
  grid.arrange(combined_plot, Legend, nrow = 2, heights = c(10, 1))  #The multiplot
  dev.off()
}

#### Figure 5 ####
ts_lengths = as.character(ts_length_mat["B",])
names(ts_lengths) = colnames(ts_length_mat)

#The stratigraphic plots:
Plot5_1=Plot_Strat("B","stasis","A")
Plot5_2=Plot_Strat("B","Brownian motion","B")
Plot5_3=Plot_Strat("B","weak Brownian drift","C")
Plot5_4=Plot_Strat("B","strong Brownian drift","D")

#The time plots:
Plot5_5=Plot_time(ts_lengths,"B","stasis","E")
Plot5_6=Plot_time(ts_lengths,"B","Brownian motion","F")
Plot5_7=Plot_time(ts_lengths,"B","weak Brownian drift","G")
Plot5_8=Plot_time(ts_lengths,"B","strong Brownian drift","H")

#Combining all the plots:
combined_plot=grid.arrange(Plot5_1,Plot5_2,Plot5_3,Plot5_4,Plot5_5,Plot5_6,Plot5_7,Plot5_8, ncol=4)

#Printing the plots plus the legend to .pdf
{
  pdf(file = paste("figs/R/fig5_raw.pdf"), width=6.5, height = 3.25)
  
  grid.arrange(combined_plot, Legend, nrow = 2, heights = c(10, 1))  #The multiplot
  dev.off()
}

#### Figure 6 ####
ts_lengths = noOfSamplingLoc_time
Plot6_1=Plot_time(ts_lengths,"A","stasis","A")
Plot6_2=Plot_time(ts_lengths,"A","Brownian motion","B")
Plot6_3=Plot_time(ts_lengths,"A","weak Brownian drift","C")
Plot6_4=Plot_time(ts_lengths,"A","strong Brownian drift","D")

combined_plot=grid.arrange(Plot6_1,Plot6_2,Plot6_3,Plot6_4, ncol=2)

{
  pdf(file = paste("figs/R/fig6_raw.pdf"), width=6.5, height = 3.25)
  grid.arrange(combined_plot, Legend, nrow = 2, heights = c(10, 1))  #The multiplot
}

