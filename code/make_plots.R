#### Get utility functions
source("code/utils.R")

#### fix random seed for repeatability
set.seed(1)

#### Load required packages
require("DAIME")
require("paleoTS")
require("grid")
require("ggplot2")
require("RColorBrewer")
require("ggrepel")
require("gridExtra")

#### load data ####
#load("data/R_outputs/ageDepthModelsScenariosAandB.Rdata")
load("data/R_outputs/results_modes_of_evolution.Rdata")
#load("data/R_outputs/hiatus_info.Rdata")

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

  #Loads in the correct line thicknesses for the graph:
  if(simulated_mode=="stasis"){
    highlight=rep(c(0.1,0.1,0.5,0.1),5)
  }else if(simulated_mode=="Brownian motion"){
    highlight=rep(c(0.1,0.1,0.1,0.5),5)
  }else if(simulated_mode=="strong Brownian drift"){
    highlight=rep(c(0.5,0.1,0.1,0.1),5)
  }else if(simulated_mode=="weak Brownian drift"){
    highlight=rep(c(0.5,0.1,0.1,0.1),5)
  }
  
  plotS1=ggplot2::ggplot(get_AIC_scenario(basin,simulated_mode), aes(x=position, y=AIC, fill=testedEvoMode)) + 
    geom_boxplot(outlier.shape = NA,lwd=highlight) +
    geom_hline(yintercept = 0.9, linetype = "dashed", linewidth=0.1)+
    labs(tag = label)+
    scale_fill_brewer(palette="Spectral")+
    theme(legend.position="none", plot.title =element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),
          text = element_text(size = 6))+
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

  #Running the function to extract the amount of sample locations:
  plot_data=get_AIC_time(no_of_sampl_loc, basin, simulated_mode)
  Mult_high=length(no_of_sampl_loc)
 
  #Loads in the correct line thicknesses for the graph:
  if(simulated_mode=="stasis"){
    highlight=rep(c(0.1,0.1,0.5,0.1),Mult_high)
  }else if(simulated_mode=="Brownian motion"){
    highlight=rep(c(0.1,0.1,0.1,0.5),Mult_high)
  }else if(simulated_mode=="strong Brownian drift"){
    highlight=rep(c(0.5,0.1,0.1,0.1),Mult_high)
  }else if(simulated_mode=="weak Brownian drift"){
    highlight=rep(c(0.5,0.1,0.1,0.1),Mult_high)
  }
  
  #Plots the plot:
  plotT1= ggplot2::ggplot(plot_data, aes(x=nsp, y=AIC, fill=testedEvoMode)) + 
    geom_boxplot(outlier.shape = NA,linewidth=highlight) +
    geom_hline(yintercept = 0.9, linetype = "dashed", linewidth=0.1)+
    labs(tag = label)+
    scale_fill_brewer(palette="Spectral")+ 
    theme(legend.position="none", plot.title =element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98), 
          text = element_text(size = 6))+
    labs(title = paste(simulated_mode), y="AIC weight", x="Number of Sampling Points")+
    scale_y_continuous(limits=c(0,1))
    
  plotT1
  
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

#### Comparison modes of evolution ####


make_evo_mode_plot = function(name, scenario, no_of_lineages = 3, plot_seed = 1){
  #' 
  #' @title create plots of evo modes in time domain
  #' 
  #' @param name: name of simulated evo mode
  #' @param scenario: "A" or "B"
  #' @param no_of_lineages: integer
  #' 
  #' @return an object to generate ggplot
  #' 
  set.seed(plot_seed)
  max_time = maxTimes[[scenario]]
  evo_index = which(name == simulatedEvoModes)
  mode = EvoModes[[evo_index]]$mode
  params = EvoModes[[evo_index]]$params
  time_res_myr = 0.001
  times = seq(0, max_time, by = time_res_myr)
  df = data.frame()
  for (i in seq_len(no_of_lineages)){
    trait_sim = simulateTraitEvo(t = times, mode = mode, params[1], params[2])
    df_2 = data.frame(t = trait_sim$time, val = trait_sim$traitValue, run = rep(as.character(i), length(time)))
    df = rbind(df, df_2)
  }
  ret_plot = ggplot2::ggplot(df, aes(x = t, y = val, color = run)) +
    geom_line()+
    theme_bw()
  
  return(ret_plot)
}

make_evo_mode_comparison_time_domain = function(scenario, no_of_lineages = 3){
  
  #' 
  #' @title compare all simulated evo modes in time domain
  #' 
  #' @param scenario: "A" or "B"
  #' @param no_of_lineages: integer
  #' 
  plot_list = list()
  for (i in seq_along(simulatedEvoModes)){
    plot_list[[i]] = make_evo_mode_plot(name = simulatedEvoModes[[i]], scenario, no_of_lineages)
  }
  file_name = paste("figs/R/evo_mode_time_domain_scenario_", scenario, "_raw.pdf", sep = "")
  pdf(file = file_name , width=6.5, height = 3.25)
  
  combined_plot = grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]])
  dev.off()
}

sapply(scenarioNames, function(x) make_evo_mode_comparison_time_domain(scenario = x, no_of_lineages = 3))


#### test_results_scenario_A and B_raw.pdf ####





#Figure 
make_test_res_strat_plot = function(scenario){
  #Setting ts_lengths to the correct lengths
  ts_lengths = as.character(ts_length_mat[scenario,])
  names(ts_lengths) = colnames(ts_length_mat)
  
  #The stratigraphic plots:
  Plot4_1=Plot_Strat(scenario,"stasis","A")
  Plot4_2=Plot_Strat(scenario,"Brownian motion","B")
  Plot4_3=Plot_Strat(scenario,"weak Brownian drift","C")
  Plot4_4=Plot_Strat(scenario,"strong Brownian drift","D")
  
  #The time plots:
  Plot4_5=Plot_time(ts_lengths,scenario,"stasis","E")
  Plot4_6=Plot_time(ts_lengths,scenario,"Brownian motion","F")
  Plot4_7=Plot_time(ts_lengths,scenario,"weak Brownian drift","G")
  Plot4_8=Plot_time(ts_lengths,scenario,"strong Brownian drift","H")
  
  #Combining all the plots:
  combined_plot=grid.arrange(Plot4_1,Plot4_2,Plot4_3,Plot4_4,Plot4_5,Plot4_6,Plot4_7,Plot4_8, ncol=4)
  
  #Printing the plots plus the legend to .pdf

  file_name = paste("figs/R/test_results_scenario_", scenario, "_raw.pdf", sep = "")
  pdf(file = file_name , width=6.5, height = 3.25)
  
  grid.arrange(combined_plot, Legend, nrow = 2, heights = c(10, 1))  #The multiplot
  dev.off()

}

for (scenario in scenarioNames){
  make_test_res_strat_plot(scenario)
}


#### test_results_time_domain_raw.pdf ####
make_test_res_time_plot = function(scenario){

  #Setting the correct sampling points:
  ts_lengths = noOfSamplingLoc_time
  #The plots:
  Plot6_1=Plot_time(ts_lengths,scenario,"stasis","A")
  Plot6_2=Plot_time(ts_lengths,scenario,"Brownian motion","B")
  Plot6_3=Plot_time(ts_lengths,scenario,"weak Brownian drift","C")
  Plot6_4=Plot_time(ts_lengths,scenario,"strong Brownian drift","D")
  
  #Combining all the plots:
  combined_plot=grid.arrange(Plot6_1,Plot6_2,Plot6_3,Plot6_4, ncol=2)
  
  #Printing the plots plus the legend to .pdf

  file_name = paste("figs/R/test_results_time_domain_", as.character(scenario), "_raw.pdf", sep = "")
  pdf(file = file_name, width = 6.5, height = 3.25)
  grid.arrange(combined_plot, Legend, nrow = 2, heights = c(10, 1))  #The multiplot
  dev.off()
}

for (scenario in scenarioNames){
  make_test_res_time_plot(scenario)
}

#### completeness_and_hiat_duration_raw.pdf ####
## TODO:
# 1. Merge plots with completeness and hiatus duration distribution. Use the left
# y axis for completeness and the right for hiatus duration
# 2. make uniform y axis scale for hiatus duration between scenarios. This is so
# the plots for different scenarios are comparable with each other
# 3. Adjust x axis. should be real distance from shore (in km) instead of the index as here
# 4. add legend for hiatus duration & use different line types for hiatus duration.
# should be "first quartile", "median", "third quartile", and "maximum"


#Getting all the input data for the graph:
make_hiat_plot = function(scenario){
  hiatus=hiat_measures[[scenario]]$completeness*100
  hiatus_max=hiat_measures[[scenario]]$max_duration_myr
  hiatus_median=hiat_measures[[scenario]]$median_duration_myr
  hiatus_1st_quart=hiat_measures[[scenario]]$first_quartile_duration_myr
  hiatus_3rd_quart=hiat_measures[[scenario]]$third_quartile_duration_myr
  xaxis=(1:150)
  
  df=data.frame(x=xaxis,y1=hiatus,y2=hiatus_max,y3=hiatus_median,y4=hiatus_1st_quart,y5=hiatus_3rd_quart)
  title = paste0("Completeness over scenario ", scenario, sep = "")
  #The plot for basin 
  plot=ggplot(data = df, aes(x=1:150))+
    geom_line(aes(y=y1), size=1)+
    geom_line(aes(y=y2*130), colour = "red")+
    geom_line(aes(y=y3*130), colour = "blue")+
    geom_line(aes(y=y4*130), colour = "lightblue",linetype = "dashed")+
    geom_line(aes(y=y5*130), colour = "brown",linetype = "dashed")+
    labs(tag = "A")+
    scale_y_continuous(
      limits=c(0,100),
      name = "Completeness",
      sec.axis = sec_axis(~.*130, name="Hiatus duration (Myr)", breaks= seq(0, 12500, by = 2500),labels = c(seq(0, 12500, by = 2500)/1000000))
    )+
    scale_x_continuous(
      breaks= seq(0, 150, by = 25),labels = c(seq(0, 150, by = 25)/10)
    )+
    ggtitle(title)+ #for the title
    xlab("Distance from shore (km)")+ # for the x axis label
    ylab("Completenes")+ # for the y axis label
    theme_bw()+ #Makes the background white.
    theme(text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),legend.position="none",plot.title = element_text(size=9)) #Changes text size
  
  return(plot)
}

make_completeness_and_hiat_plot = function(){
  combined_plot=grid.arrange(make_hiat_plot("A"),make_hiat_plot("B"),ncol=2)
  pdf(file = paste("figs/R/completeness_and_hiat_duration_raw.pdf"), width=6.5, height = 3.25)
  grid.arrange(combined_plot, ncol = 1, heights = c(10, 1))  #The multiplot
  dev.off()
}

make_completeness_and_hiat_plot()


#### Skewness and kurtosis of hiatus duration ####
require("moments")

skew_list = list()
for (scenario in scenarioNames){
  skew_list[[scenario]] = sapply(seq_len(length(all_dist)), function(x) skewness(get_hiatus_distribution(pos = all_dist[x],scenario)) )
}

plot(NULL, xlim = c(1,150), ylim = c(0,11), main = "Skewness")
lines(1:150, skew_list[["A"]], col = "red")
lines(1:150, skew_list[["B"]], col = "black")
legend("topleft", col = c("red", "black"), legend = c("A", "B"), lty = 1)

kurt_list = list()
for (scenario in scenarioNames){
  kurt_list[[scenario]] = sapply(seq_len(length(all_dist)), function(x) kurtosis(get_hiatus_distribution(pos = all_dist[x],scenario)) )
}

plot(NULL, xlim = c(1,150), ylim = c(0,20), main = "Kurtosis")
lines(1:150, kurt_list[["A"]], col = "red")
lines(1:150, kurt_list[["B"]], col = "black")
legend("topleft", col = c("red", "black"), legend = c("A", "B"), lty = 1)

#### equal_completeness_comparison.pdf ####

#Inputs for the correct simulated mode of evolution:
make_pairwise_comparison_plot = function(scenario_1, scenario_2, dist_1, dist_2, mode, no_of_lineages = 3, plot_seed = 1){
  set.seed(plot_seed)
  # function generating a plot that compares the same mode of evolution at differnet places in the basin and plots the completeness

  mode = simulatedEvoModes[4]
  
  stopifnot(mode %in% simulatedEvoModes)
  evo_index = which(mode == simulatedEvoModes )
  evo_fullname = EvoModes[[evo_index]]$name
  evo_mode = EvoModes[[evo_index]]$mode
  evo_params = EvoModes[[evo_index]]$params
  
  sample_loc_1 = get_sample_locations(scenario = scenario_1,
                                      distance_from_shore_km = dist_1,
                                      distanceBetweenSamples = distanceBetweenSamples)
  
  sample_loc_2 = get_sample_locations(scenario = scenario_2,
                                      distance_from_shore_km = dist_2,
                                      distanceBetweenSamples = distanceBetweenSamples)
  
  get_sample_times = function(scenario, pos){
    sample_height = seq(distanceBetweenSamples, max(ageDepthModels[[scenario]][[pos]]$height), by = distanceBetweenSamples)
    times = approx(x = ageDepthModels[[scenario]][[pos]]$height, ageDepthModels[[scenario]][[pos]]$time, xout = sample_height,ties = mean)$y
  }
  
  sample_times_1 = get_sample_times(scenario_1, dist_1)
  sample_times_2 = get_sample_times(scenario_2, dist_2)
  sim_times = seq(0, max(sapply(c(scenario_1, scenario_2), function(x) maxTimes[[x]])), by = 0.001)
  
  relevant_times = sort(unique(c(sample_times_1, sample_times_2, sim_times)))
  
  
  lin_list = list()
  for (i in seq_len(no_of_lineages)){
    lin_list[[i]] = simulateTraitEvo(relevant_times, evo_mode, evo_params[1], evo_params[2])
  }
  
  make_time_dom_subplot = function(){
    df = data.frame()
    for (i in seq_len(no_of_lineages)){
      df_temp = data.frame(h = sim_times,
                           val = approx(x = lin_list[[i]]$time,
                                        y = lin_list[[i]]$traitValue,
                                        xout = sim_times)$y,
                           lineage = rep(as.character(i, length(sim_times))))
      df = rbind(df, df_temp)
    }
    
    out_plot = ggplot(data = df, aes(x = h, y = val, col = lineage)) +
      geom_line() + 
      theme_bw() +
      theme(legend.position = "none")
    return(out_plot)
  }
  
  make_strat_dom_subplot_1 = function(){
    completeness = get_completeness(pos = dist_2, scenario = scenario_2)
    comp = signif(completeness, digits = 4) * 100
    df = data.frame()
    for (i in seq_len(no_of_lineages)){
      df_temp = data.frame(h = sample_loc_1,
                           val = approx(x = lin_list[[i]]$time,
                                        y = lin_list[[i]]$traitValue,
                                        xout = sample_times_1)$y,
                           lineage = rep(as.character(i, length(sample_loc_1))))
      df = rbind(df, df_temp)
    }
    out_plot = ggplot(data = df, aes(x = h, y = val, col = lineage)) +
      geom_line() +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(paste0(comp))
    return(out_plot)
  }
  
  make_strat_dom_subplot_2 = function(){
    df = data.frame()
    for (i in seq_len(no_of_lineages)){
      df_temp = data.frame(h = sample_loc_2,
                           val = approx(x = lin_list[[i]]$time,
                                        y = lin_list[[i]]$traitValue,
                                        xout = sample_times_2)$y,
                           lineage = rep(as.character(i, length(sample_loc_2))))
      df = rbind(df, df_temp)
    }
    
    completeness = get_completeness(pos = dist_2, scenario = scenario_2)
    comp = signif(completeness, digits = 4) * 100
    out_plot = ggplot(data = df, aes(x = h, y = val, col = lineage)) +
      geom_line() +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(paste0(comp))
    return(out_plot)
  }
  
  
   time_subplot = make_time_dom_subplot()
  
   strat_plot_1 = make_strat_dom_subplot_1()
   strat_plot_2 = make_strat_dom_subplot_2()
   
   file_name = paste("figs/R/pairwise_comp", scenario_1, dist_1, scenario_2, dist_2, mode, ".pdf", sep = "")
   pdf(file = file_name, width=6.5, height = 7.5)
   grid.arrange(time_subplot, strat_plot_1, strat_plot_2, nrow = 1)
   dev.off()
   

}

make_pairwise_comparison_plot(scenario_1 = "A",
                              scenario_2 = "B",
                              dist_1 = "4 km",
                              dist_2 = "6 km",
                              mode = "strong Brownian drift",
                              no_of_lineages = 3,
                              plot_seed = 1)




  
#### spatial_variability_scen_A_sBD.pdf #### 
    

    
  make_spat_comparison_plot = function(scenario, mode, no_of_lineages = 5, plot_seed = 1){
    set.seed(plot_seed)
    stopifnot(mode %in% simulatedEvoModes)
    evo_index = which(mode == simulatedEvoModes )
    evo_fullname = EvoModes[[evo_index]]$name
    evo_mode = EvoModes[[evo_index]]$mode
    evo_params = EvoModes[[evo_index]]$params
    
    get_sample_times = function(scenario, pos){
      sample_height = seq(distanceBetweenSamples, max(ageDepthModels[[scenario]][[pos]]$height), by = distanceBetweenSamples)
      times = approx(x = ageDepthModels[[scenario]][[pos]]$height, ageDepthModels[[scenario]][[pos]]$time, xout = sample_height,ties = mean)$y
    }
    
    sim_times = seq(0, maxTimes[[scenario]], 0.001)
    sample_times = list()
    sample_heights = list()
    for (i in seq_along(examinedBasinPositions)){
      sample_heights[[i]] = get_sample_locations(scenario = scenario,
                                           distance_from_shore_km = examinedBasinPositions[i],
                                           distanceBetweenSamples = distanceBetweenSamples)
      sample_times[[i]] = get_sample_times(scenario = scenario,
                                           pos = examinedBasinPositions[i])
    }
    relevant_times = sort(unique(c(sim_times, unlist(sample_times))))
    
    trait_val_list = list()
    for (i in seq_len(no_of_lineages)){
      trait_val_list[[i]] = simulateTraitEvo(t = relevant_times,
                                             mode = evo_mode,
                                             evo_params[1],
                                             evo_params[2])
    }
    
    make_time_domain_subplot = function(){
      df = data.frame()
      for (i in seq_len(no_of_lineages)){
        df_temp = data.frame(t = sim_times,
                             val = approx(x = trait_val_list[[i]]$time,
                                          y = trait_val_list[[i]]$traitValue,
                                          xout = sim_times)$y,
                             lineage = rep(as.character(i), length(sim_times)))
       df = rbind(df, df_temp)
      }
      
      out_plot = ggplot( data = df, aes(x = t, y = val, col = lineage)) + 
        geom_line()
      
      return(out_plot)
    }
  
  
    make_pos_subplot = function(pos){
      df = data.frame()
      for (i in seq_len(no_of_lineages)){
        df_temp = data.frame(h = sample_heights[[i]],
                             val = approx(x = trait_val_list[[i]]$time,
                                          y = trait_val_list[[i]]$traitValue,
                                          xout = sample_times[[i]])$y,
                              lineage = rep(as.character(i), length(sample_times[[i]])))
        df = rbind(df, df_temp)
      }
      outplot = ggplot( data = df, aes(x = h, y = val, col = lineage)) + 
        geom_line()
      return(outplot)
    }
    

  
    make_adm_subplot = function(scenario, distances_from_shore_km){
      #' 
      #' @title plot adms at differnet positions in basin
      #' 
      #' @param scenario: "A" or "B"
      #' @param distances_from_shore_km: vector, subset of all_dist
      #' 
      #' @return a ggplot object
      df = data.frame()
      for (dist in distances_from_shore_km){
        df_temp = data.frame(t = ageDepthModels[[scenario]][[dist]]$time,
                             h = replace(ageDepthModels[[scenario]][[dist]]$height,
                                         duplicated(ageDepthModels[[scenario]][[dist]]$height),
                                         NA),
                             distance = rep(dist, length(ageDepthModels[[scenario]][[dist]]$time)))
        df = rbind(df, df_temp)
      }
      ret_plot = ggplot2::ggplot(df, aes(x = t, y = h, col = distance)) +
        geom_line() + 
        theme_bw()
      return(ret_plot)
    }
    
    
    adm_subplot = make_adm_subplot(scenario, examinedBasinPositions)
    time_domain_subplot = make_time_domain_subplot()
    plot_2km = make_pos_subplot("2 km")
    plot_6km = make_pos_subplot("6 km")
    plot_8km = make_pos_subplot("8 km")
    plot_10km = make_pos_subplot("10 km")
    plot_12km = make_pos_subplot("12 km")
    
    file_name = paste("figs/R/spatial_variability_scen_", scenario, mode, ".pdf", sep = "")
    pdf(file = file_name, width=6.5, height = 7.5)
    grid.arrange(adm_subplot, time_domain_subplot, plot_2km, plot_6km, plot_8km, plot_10km, plot_12km)
    dev.off()
    
    # merge all plots here
    
  }
  
  for (scenario in scenarioNames){
    for (mode in simulatedEvoModes){
      make_spat_comparison_plot(scenario = scenario,
                                mode = mode,
                                no_of_lineages = 3,
                                plot_seed = 1)
    }
  }    

      

    
#### variable_pres_of_different modes #### 
    
make_var_pres_of_modes_plot = function(pos, scenario, no_of_lineages = 3, plot_seed = 1){
  set.seed(plot_seed)
  get_sample_times = function(scenario, pos){
    sample_height = seq(distanceBetweenSamples, max(ageDepthModels[[scenario]][[pos]]$height), by = distanceBetweenSamples)
    times = approx(x = ageDepthModels[[scenario]][[pos]]$height, ageDepthModels[[scenario]][[pos]]$time, xout = sample_height,ties = mean)$y
    return(times)
  }
  sample_height = get_sample_locations(scenario = scenario, distance_from_shore_km = pos, distanceBetweenSamples = distanceBetweenSamples)
  sample_times = get_sample_times(scenario, pos)
  sim_times = seq(0, maxTimes[[scenario]], by = 0.001)
  relevant_times = sort(unique(c(sample_times, sim_times)))
  
  compare_evo_modes_time_strat_plot = function(name, no_of_lineages = 3){
    evo_index = which(name == simulatedEvoModes)
    mode = EvoModes[[evo_index]]$mode
    params = EvoModes[[evo_index]]$params
    trait_list = list()
    for ( i in seq_len(no_of_lineages)){
      trait_list[[i]] = simulateTraitEvo(t = relevant_times, mode = mode, params[1], params[2])
    }
    
    plot_list = list()
    
    strat_df = data.frame()
    for (i in seq_len(no_of_lineages)){
      df_temp = data.frame(h = sample_height,
                           val  = approx(x = trait_list[[i]]$time, y = trait_list[[i]]$traitValue, xout = sample_times)$y,
                           lineage = rep(as.character(i), length(sample_height)))
      strat_df = rbind(strat_df,df_temp)
    }
    
    plot_list[["strat"]] = ggplot2::ggplot(data = strat_df, aes(x = h, y = val, col = lineage)) +
      geom_line() +
      theme_bw() +
      theme(legend.position = "none")
    
    time_df = data.frame()
    for (i in seq_len(no_of_lineages)){
      df_temp = data.frame(t = sim_times,
                           val  = approx(x = trait_list[[i]]$time, y = trait_list[[i]]$traitValue, xout = sim_times)$y,
                           lineage = rep(as.character(i), length(sim_times)))
      time_df = rbind(time_df,df_temp)
    }
    
    plot_list[["time"]] = ggplot2::ggplot(data = time_df, aes(x = t, y = val, col = lineage)) +
      geom_line() +
      theme_bw() + 
      theme(legend.position = "none")
    
    return(plot_list)
  }
  
  comb_list = list()
  for (name in simulatedEvoModes){
    comb_list[[name]] = compare_evo_modes_time_strat_plot(name, no_of_lineages = 3)
  }
  
  file_name = paste("figs/R/comparison_pres_of_modes", scenario," " , pos, "_raw.pdf", sep = "")
  pdf(file = file_name , width=6.5, height = 3.25)
  combined_plot = grid.arrange(comb_list$stasis$strat,
                               comb_list$`Brownian motion`$strat,
                               comb_list$`weak Brownian drift`$strat,
                               comb_list$`strong Brownian drift`$strat,
                               comb_list$stasis$time,
                               comb_list$`Brownian motion`$time,
                               comb_list$`weak Brownian drift`$time,
                               comb_list$`strong Brownian drift`$time,
                               nrow = 2)
  
  dev.off()
  
}
    
for (scenario in scenarioNames) {
  for (pos in examinedBasinPositions) {
    make_var_pres_of_modes_plot(pos = pos, scenario = scenario, no_of_lineages = 3, plot_seed = 1)
  }
}
    