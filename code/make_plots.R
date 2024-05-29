#### Get utility functions
source("code/utils.R")

#### Load required packages

require("ggplot2")


#### load data ####

load("data/R_outputs/results_modes_of_evolution.Rdata")
load("data/R_outputs/ageDepthModelsScenariosAandB.Rdata")
load("data/R_outputs/parameters_for_tests.Rdata")


#### Constants ####

strat_label = "Stratigraphic Height [m]"
time_label = "Time [Myr]"
time_domain = "Time Domain"
trait_label = "Trait Value"
AICw_label = "AIC weight"
sampling_points_label = "Time Series Length"
distance_from_shore_label = "Distance from Shore [km]"
axis_label_size = 6
tick_size = 6
evo_mode_lwds = c(0.05,0.5,0.5,0.5)
title_size = 10
adm_palette = RColorBrewer::brewer.pal(name = "PuOr", n = 9)[-c(4:7)]
lineage_palette = "Set1"
hiat_palette = c("black", rev(RColorBrewer::brewer.pal(name = "OrRd", n = 5)[-1]))
default_width_fp_in = 170 / 25.4 # width for  full page fig in inch
default_width_hp_in = 85 / 25.4 # width for  half page fig in inch

AIC_palette = "Spectral"
max_dist_plot_km = max_rel_distance_km
legend_text_size = 6

#### Helper functions ####
get_AIC_scenario = function(basin, simulated_mode){
  
  #' 
  #' @title Get AIC vals from model output for plotting
  #' 
  #' @param basin: "A" or "B": the scenario of interest. 
  #' @param simulated_mode: "stasis", "Brownian motion", or "Brownian drift". True (= simulated) mode of evolution for which AIC is supposed to be extracted
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
  #' @param simulated_mode: "stasis", "Brownian motion", or "Brownian drift". True (= simulated) mode of evolution for which AIC is supposed to be extracted
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
  #' @param simulated_mode: "stasis", "Brownian motion", or "Brownian drift". True (= simulated) mode of evolution for which AIC is supposed to be extracted
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


plot_AIC_strat = function(basin, simulated_mode, label){
  #' 
  #' @title plots AIC values in boxplots for the stratigraphic domain
  #'
  #' @param simulated_mode: "stasis", "Brownian motion", or "Brownian drift", 
  #' True (= simulated) mode of evolution for which AIC is supposed to be extracted
  #' @param basin: String, "A" or "B"
  #' @param label: the label you want attached to the plot, String,"A" to "Z"
  #' 
  #' @description Plots AIC values collected by get_AIC_scenario.
  #' 
  #' @return A graph, ready to be combined with other graphs in the combined plot.
  #' 

  #Loads in the correct line thicknesses for the graph:
  if(simulated_mode=="stasis"){
    highlight=rep(c(0.1,0.5,0.1),5)
  }else if(simulated_mode=="Brownian motion"){
    highlight=rep(c(0.1,0.1,0.5),5)
  }else if(simulated_mode=="Brownian drift"){
    highlight=rep(c(0.5,0.1,0.1),5)
  }
  
  plotS1=ggplot2::ggplot(get_AIC_scenario(basin,simulated_mode), aes(x=position, y=AIC, fill=testedEvoMode)) + 
    geom_boxplot(outlier.shape = NA,lwd=highlight) +
    labs(tag = label)+
    scale_fill_brewer(palette=AIC_palette)+
    theme(legend.position="none", plot.title =element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),
          text = element_text(size = 6))+
    labs(title = stringr::str_to_title(simulated_mode), y=AICw_label, x=distance_from_shore_label)+
    scale_x_discrete( labels=c("2 km"="2","6 km"="6","8 km"="8","10 km"="10","12 km"="12"))+
    scale_y_continuous(limits=c(0,1))
  
  return(plotS1)
}

plot_AIC_time = function(no_of_sampl_loc, basin, simulated_mode, label){
  #' 
  #' @title plots AIC values in boxplots for the stratigraphic domain
  #' 
  #'@param no_of_sampl_loc : string, element of noOfSamplingLoc
  #' @param simulated_mode: "stasis", "Brownian motion", "Brownian drift", 
  #'  True (= simulated) mode of evolution for which AIC is supposed to be extracted
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
    highlight=rep(c(0.1,0.5,0.1),Mult_high)
  }else if(simulated_mode=="Brownian motion"){
    highlight=rep(c(0.1,0.1,0.5),Mult_high)
  }else if(simulated_mode=="Brownian drift"){
    highlight=rep(c(0.5,0.1,0.1),Mult_high)
  }
  
  #Plots the plot:
  plotT1= ggplot2::ggplot(plot_data, aes(x=nsp, y=AIC, fill=testedEvoMode)) + 
    geom_boxplot(outlier.shape = NA,linewidth=highlight) +
    labs(tag = label)+
    scale_fill_brewer(palette=AIC_palette)+ 
    theme(legend.position="none", plot.title =element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98), 
          text = element_text(size = 6))+
    labs(title = stringr::str_to_title(simulated_mode), y=AICw_label, x=sampling_points_label)+
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
dev.off()
}

#### Comparison modes of evolution in time domain ####

plot_comparison_evo_modes_time_domain = function(scenario, no_of_lineages = 3, plot_seed = 1){
  
  #' 
  #' @title compare all simulated evo modes in time domain
  #' 
  #' @param scenario: "A" or "B"
  #' @param no_of_lineages: integer
  #' @param plot_seed: seed for random number generator
  #' 
  make_evo_mode_plot = function(name, scenario, label, no_of_lineages, plot_seed){
    #' 
    #' @title create plots of evo modes in time domain
    #' 
    #' @param name: name of simulated evo mode
    #' @param scenario: "A" or "B"
    #' @param label: label to be shown at top left corner
    #' @param no_of_lineages: integer
    #' @param plot_seed seed for the simulations
    #' 
    #' @return an object to generate plot
    #' 
    set.seed(plot_seed)
    max_time = maxTimes[[scenario]]
    evo_index = which(name == simulatedEvoModes)
    mode = EvoModes[[evo_index]]$mode
    params = EvoModes[[evo_index]]$params
    times = seq(0, max_time, by = time_res_myr)
    df = data.frame()
    for (i in seq_len(no_of_lineages)){
      trait_sim = simulateTraitEvo(t = times, mode = mode, params[1], params[2])
      df_2 = data.frame(t = trait_sim$time, val = trait_sim$traitValue, run = rep(as.character(i), length(time)))
      df = rbind(df, df_2)
    }
    ret_plot = ggplot2::ggplot(df, aes(x = t, y = val, color = run)) +
      geom_line(lwd = evo_mode_lwds[evo_index])+
      theme_bw() +
      xlab(time_label) +
      ylab(trait_label) +
      theme(legend.position = "none") +
      labs(tag = label) +
      scale_color_brewer(palette=lineage_palette) +
      theme(plot.title = element_text(size = title_size)) +
      theme(axis.title = element_text(size = axis_label_size)) +
      theme(axis.text = element_text(size = tick_size)) +
      ggtitle(stringr::str_to_title(name))
    
    return(ret_plot)
  }
  
  plot_list = list()
  for (i in seq_along(simulatedEvoModes)){
    plot_list[[i]] = make_evo_mode_plot(name = simulatedEvoModes[[i]], scenario, LETTERS[i], no_of_lineages, plot_seed)
  }
  file_name = paste("figs/R/comparison_evo_modes_time_domain_scenario_", scenario, "_raw.pdf", sep = "")
  pdf(file = file_name , width=default_width_fp_in)
  
  combined_plot = gridExtra::grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]])
  dev.off()
}

sapply(scenarioNames, function(x) plot_comparison_evo_modes_time_domain(scenario = x, no_of_lineages = 3, plot_seed = 1))


#### plot AIC weights for scenario A & B  ####

make_test_res_strat_plot = function(scenario){
  #Setting ts_lengths to the correct lengths
  ts_lengths = as.character(ts_length_mat[scenario,])
  names(ts_lengths) = colnames(ts_length_mat)
  
  #The stratigraphic plots:
  Plot4_1=plot_AIC_strat(scenario,"stasis","A")
  Plot4_2=plot_AIC_strat(scenario,"Brownian motion","B")
  Plot4_3=plot_AIC_strat(scenario,"Brownian drift","C")
  
  #The time plots:
  Plot4_5=plot_AIC_time(ts_lengths,scenario,"stasis","D")
  Plot4_6=plot_AIC_time(ts_lengths,scenario,"Brownian motion","E")
  Plot4_7=plot_AIC_time(ts_lengths,scenario,"Brownian drift","F")

  
  #Combining all the plots:
  combined_plot=gridExtra::grid.arrange(Plot4_1,Plot4_2,Plot4_3,Plot4_5,Plot4_6,Plot4_7, ncol=3)
  
  #Printing the plots plus the legend to .pdf

  file_name = paste("figs/R/test_results_scenario_", scenario, "_raw.pdf", sep = "")
  pdf(file = file_name , width=default_width_fp_in, height = 3.25)
  
  gridExtra::grid.arrange(combined_plot, Legend, nrow = 2, heights = c(10, 1))  #The multiplot
  dev.off()

}


sapply(scenarioNames, function(scenario) make_test_res_strat_plot(scenario))

##### Evidence ratio: example from one case #####
# Extracted from Scenario A, position at 
A_BW <- get_AIC_scenario("A","Brownian motion")
A_BW <- A_BW[A_BW == "6 km",]
A_BW <- as.data.frame(cbind(A_BW[1:100,3], A_BW[101:200,3]))
colnames(A_BW) <- c("GRW","URW")
A_BW$ratio <- A_BW$URW/A_BW$GRW

ggplot2::ggplot(A_BW, aes(ratio)) + 
  geom_histogram(color="blue", fill="white")+
  labs(x = "AICc(URW)/AICc(GRW)", 
       y = "Count")+
  geom_vline(aes(xintercept=1),
             color="red", linetype="dashed", size=1)

#### plot AIC weights of tests in time domain ####
make_test_res_time_plot = function(scenario){

  #Setting the correct sampling points:
  ts_lengths = noOfSamplingLoc_time
  #The plots:
  Plot6_1=plot_AIC_time(ts_lengths,scenario,"stasis","A")
  Plot6_2=plot_AIC_time(ts_lengths,scenario,"Brownian motion","B")
  Plot6_3=plot_AIC_time(ts_lengths,scenario,"Brownian drift","C")
  
  #Combining all the plots:
  combined_plot=gridExtra::grid.arrange(Plot6_1,Plot6_2,Plot6_3, ncol=1)
  
  #Printing the plots plus the legend to .pdf

  file_name = paste("figs/R/test_results_time_domain_", as.character(scenario), "_raw.pdf", sep = "")
  pdf(file = file_name, width = default_width_fp_in, height = 6)
  gridExtra::grid.arrange(combined_plot, Legend, nrow = 2, heights = c(10, 1))  #The multiplot
  dev.off()
}


sapply(scenarioNames, function(scenario) make_test_res_time_plot(scenario))


#### Plot completeness and hiatus duration ####

#Getting all the input data for the graph:
make_hiat_plot = function(scenario, y_resc, label){
  compl=hiat_measures[[scenario]]$completeness*100
  hiatus_max=hiat_measures[[scenario]]$max_duration_myr
  hiatus_median=hiat_measures[[scenario]]$median_duration_myr
  hiatus_1st_quart=hiat_measures[[scenario]]$first_quartile_duration_myr
  hiatus_3rd_quart=hiat_measures[[scenario]]$third_quartile_duration_myr

  df = data.frame(x = rep(all_dist_raw, 5),
                    y = c(compl,
                          hiatus_max/ y_resc * 100,
                          hiatus_median / y_resc * 100,
                          hiatus_1st_quart / y_resc * 100,
                          hiatus_3rd_quart / y_resc * 100 ),
                    type = factor(c(rep("Completeness", length(all_dist_raw)), 
                                    rep("Maximum hiatus duration", length(all_dist_raw)),
                                    rep("Median hiatus duration", length(all_dist_raw)),
                                    rep("1st quartile of hiatus durations", length(all_dist_raw)),
                                    rep("3rd quartile of hiatus durations", length(all_dist_raw))),
                                  levels = c("Completeness",
                                             "Maximum hiatus duration",  
                                             "3rd quartile of hiatus durations",
                                             "Median hiatus duration",
                                             "1st quartile of hiatus durations"
                                            )))
  title = paste0("Completeness in scenario ", scenario, sep = "")
  #The plot for basin 
  plot=ggplot(data = df, aes(x=x, y = y, col = type)) +
    geom_line() +
    labs(tag = label)+
    scale_y_continuous(
      limits=c(0,100),
      name = "Completeness [%]",
      sec.axis = sec_axis(~./y_resc * 100, name="Hiatus duration [Myr]", breaks= seq(0, 100 * 100 / y_resc, length.out = 4), labels = seq(0, y_resc, length.out = 4))
    )+
    ggtitle(title)+ #for the title
    xlab(distance_from_shore_label)+ # for the x axis label
    ylab("Completeness [%]")+ # for the y axis label
    theme_bw()+ #Makes the background white.
    theme(text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98)) +
    theme(legend.position = c(0.3, 0.85)) +
    theme(legend.key.size = unit(0.3, "cm")) +
    theme(legend.title = element_blank()) +
    xlim(c(0, max_dist_plot_km)) +
    scale_color_manual(values = hiat_palette) +
    theme(legend.text = element_text(size = legend_text_size))
  
  return(plot)
}

make_completeness_and_hiat_plot = function(){
  y_resc = max(sapply(scenarioNames, function(scenario) max(hiat_measures[[scenario]]$max_duration_myr)))
  y_resc = ceiling(y_resc * 100)/ 100
  pdf(file = paste("figs/R/completeness_and_hiat_duration_raw.pdf"), width=default_width_fp_in, height = 3.25)
  gridExtra::grid.arrange(make_hiat_plot("A", y_resc, "A"),make_hiat_plot("B", y_resc, "B"),ncol=2)
  dev.off()
}

make_completeness_and_hiat_plot()




#### Pairwise comparison ####

plot_pairwise_comparison = function(scenario_1, scenario_2, dist_1, dist_2, mode, no_of_lineages = 3, plot_seed = 1){
  set.seed(plot_seed)

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
  
  ymax =  max(sapply(seq_len(no_of_lineages), function(i) max(lin_list[[i]]$traitValue)))
  ymin =  min(sapply(seq_len(no_of_lineages), function(i) min(lin_list[[i]]$traitValue)))
  
  make_time_dom_subplot = function(label, header){
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
      theme(legend.position = "none") +
      xlab(time_label) +
      ylab(trait_label) +
      ggtitle(header) +
      labs(tag = label) +
      ylim(c(ymin, ymax)) +
      theme(plot.title = element_text(size = title_size)) +
      theme(axis.title = element_text(size = axis_label_size)) +
      theme(axis.text = element_text(size = tick_size)) +
      scale_color_brewer(palette=lineage_palette)
    return(out_plot)
  }
  
  make_strat_dom_subplot_1 = function(label){
    completeness = get_completeness(pos = dist_1, scenario = scenario_1)
    comp = signif(completeness, digits = 3) * 100
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
      ggtitle(paste0(comp, " % Completeness")) +
      labs(tag = label) +
      xlab(strat_label) + 
      ylab(trait_label) +
      ylim(c(ymin, ymax)) + 
      theme(plot.title = element_text(size = title_size)) +
      theme(axis.title = element_text(size = axis_label_size)) +
      theme(axis.text = element_text(size = tick_size)) +
      scale_color_brewer(palette=lineage_palette)
    return(out_plot)
  }
  
  make_strat_dom_subplot_2 = function(label){
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
    comp = signif(completeness, digits = 3) * 100
    out_plot = ggplot(data = df, aes(x = h, y = val, col = lineage)) +
      geom_line() +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(paste0(comp, " % Completeness")) +
      labs(tag = label) +
      xlab(strat_label) +
      ylab(trait_label) +
      ylim(c(ymin, ymax)) +
      theme(plot.title = element_text(size = title_size)) +
      theme(axis.title = element_text(size = axis_label_size)) +
      theme(axis.text = element_text(size = tick_size)) +
      scale_color_brewer(palette=lineage_palette)
    return(out_plot)
  }
  
  
   time_subplot = make_time_dom_subplot(label = "A", header = "Time Domain")
  
   strat_plot_1 = make_strat_dom_subplot_1(label = "B")
   strat_plot_2 = make_strat_dom_subplot_2(label = "C")
   
   file_name = paste("figs/R/pairwise_comp", scenario_1, dist_1, scenario_2, dist_2, mode, ".pdf", sep = "")
   pdf(file = file_name, width=default_width_fp_in, height = 3)
   gridExtra::grid.arrange(time_subplot, strat_plot_1, strat_plot_2, nrow = 1)
   dev.off()
   

}

plot_pairwise_comparison(scenario_1 = "A",
                        scenario_2 = "B",
                        dist_1 = "2 km",
                        dist_2 = "6 km",
                        mode = "Brownian drift",
                        no_of_lineages = 3,
                        plot_seed = 1)




  
#### Spatial variability in preservation across basin #### 
### Work on the arrangement here! 1strow ADM, followed by 2 cols of trait sims
# check with joiurnal

    
  plot_spat_comparison = function(scenario, mode, no_of_lineages = 3, plot_seed = 1){
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
    
    sim_times = seq(0, maxTimes[[scenario]], time_res_myr)
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
    
    make_time_domain_subplot = function(label){
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
        geom_line(lwd = evo_mode_lwds[evo_index]) +
        theme_bw() +
        xlab(time_label) +
        ylab(trait_label) +
        ggtitle(time_domain) +
        theme(legend.position = "none")  +
        labs(tag = label) +
        theme(axis.title = element_text(size = axis_label_size)) +
        theme(axis.text = element_text(size = tick_size)) +
        scale_color_brewer(palette=lineage_palette) +
        theme(plot.title = element_text(size=10))

      
      return(out_plot)
    }
  
  
    make_pos_subplot = function(pos, label){
      df = data.frame()
      for (i in seq_len(no_of_lineages)){
        df_temp = data.frame(h = sample_heights[[which(pos == examinedBasinPositions)]],
                             val = approx(x = trait_val_list[[i]]$time,
                                          y = trait_val_list[[i]]$traitValue,
                                          xout = sample_times[[which(pos == examinedBasinPositions)]])$y,
                              lineage = rep(as.character(i), length(sample_times[[which(pos == examinedBasinPositions)]])))
        df = rbind(df, df_temp)
      }
      outplot = ggplot( data = df, aes(x = h, y = val, col = lineage)) + 
        geom_line(lwd = evo_mode_lwds[evo_index]) +
        theme_bw() +
        xlab(strat_label) +
        ylab(trait_label) +
        ggtitle(paste0(pos, " Offshore")) +
        theme(legend.position = "none") +
        labs(tag = label) +
        theme(axis.title = element_text(size = axis_label_size)) +
        theme(axis.text = element_text(size = tick_size)) +
        scale_color_brewer(palette=lineage_palette) +
        theme(plot.title = element_text(size=10))
      return(outplot)
    }
    

  
    make_adm_subplot = function(scenario, distances_from_shore_km, label){
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
     suppressWarnings( { ret_plot = ggplot2::ggplot(df, aes(x = t, y = h, col = distance)) +
        geom_line() + 
        theme_bw() + 
        xlab(time_label) +
        ylab(strat_label) +
       labs(tag = label) +
       theme(axis.title = element_text(size = axis_label_size)) +
       theme(axis.text = element_text(size = tick_size)) +
       theme(legend.position = c(0.21, 0.765)) +
       theme(legend.key.size = unit(0.1, "cm")) +
       theme(legend.box.margin = margin()) +
       theme(legend.title = element_text(size = 10)) +
       theme(legend.text = element_text(size = 8)) +
       scale_color_manual(values=adm_palette)} ) +
       theme(plot.title = element_text(size=10))
      return(ret_plot)
    }
    
    
    adm_subplot = make_adm_subplot(scenario, examinedBasinPositions, label = "A")
    time_domain_subplot = make_time_domain_subplot(label = "B")
    plot_2km = make_pos_subplot("2 km", label = "C")
    plot_6km = make_pos_subplot("6 km", label = "D")
    plot_8km = make_pos_subplot("8 km", label = "E")
    plot_10km = make_pos_subplot("10 km", label = "F")
    plot_12km = make_pos_subplot("12 km", label  = "G")
    
    file_name = paste("figs/R/spatial_variability_scen_", scenario, "_", mode, ".pdf", sep = "")
    pdf(file = file_name, width=default_width_fp_in, height = 7.5)
    gridExtra::grid.arrange(adm_subplot, time_domain_subplot, plot_2km, plot_6km, plot_8km, plot_10km, plot_12km)
    dev.off()
    
    # merge all plots here
    
  }
  
  for (scenario in scenarioNames){
    for (mode in simulatedEvoModes){
      plot_spat_comparison(scenario = scenario,
                                mode = mode,
                                no_of_lineages = 3,
                                plot_seed = 1)
    }
  }    

      

    
####  Variable preservation of different modes of evolution #### 
    
make_var_pres_of_modes_plot = function(pos, scenario, no_of_lineages = 3, plot_seed = 1){
  #'
  #'@title plot variable preservation of modes
  #'
  #'@description plots preservation of evo modes at 
  #'
  #'@param pos: distance from shore, e.g. "2 km"
  #'@param scenario: "A" or "B"
  #'@param no_of_lineages: integer
  #'@param plot_seed: seed for random number generator
  #'
  #'
  set.seed(plot_seed)

  sample_height = get_sample_locations(scenario = scenario,
                                       distance_from_shore_km = pos,
                                       distanceBetweenSamples = distanceBetweenSamples)
  sample_times = get_sample_times(scenario, pos)
  sim_times = seq(0, maxTimes[[scenario]], by = time_res_myr)
  relevant_times = sort(unique(c(sample_times, sim_times)))
  
  compare_evo_modes_time_strat_plot = function(name, labels, lwd, no_of_lineages = 3){
    evo_index = which(name == simulatedEvoModes)
    mode = EvoModes[[evo_index]]$mode
    params = EvoModes[[evo_index]]$params
    trait_list = list()
    lwd = evo_mode_lwds[evo_index]
    for ( i in seq_len(no_of_lineages)){
      trait_list[[i]] = simulateTraitEvo(t = relevant_times, mode = mode, params[1], params[2])
    }
    
    plot_list = list()
    
    strat_df = data.frame()
    for (i in seq_len(no_of_lineages)){
      df_temp = data.frame(h = sample_height,
                           val  = approx(x = trait_list[[i]]$time,
                                         y = trait_list[[i]]$traitValue,
                                         xout = sample_times)$y,
                           lineage = rep(as.character(i), length(sample_height)))
      strat_df = rbind(strat_df,df_temp)
    }
    
    plot_list[["strat"]] = ggplot2::ggplot(data = strat_df, aes(x = h, y = val, col = lineage)) +
      geom_line(lwd = lwd) +
      theme_bw() +
      theme(legend.position = "none") +
      xlab(strat_label) +
      ylab(trait_label) +
      labs(tag = labels[1]) +
      theme(axis.title = element_text(size = axis_label_size)) +
      theme(axis.text = element_text(size = tick_size)) +
      scale_color_brewer(palette=lineage_palette)
    
    time_df = data.frame()
    for (i in seq_len(no_of_lineages)){
      df_temp = data.frame(t = sim_times,
                           val  = approx(x = trait_list[[i]]$time,
                                         y = trait_list[[i]]$traitValue,
                                         xout = sim_times)$y,
                           lineage = rep(as.character(i), length(sim_times)))
      time_df = rbind(time_df,df_temp)
    }
    
    plot_list[["time"]] = ggplot2::ggplot(data = time_df, aes(x = t, y = val, col = lineage)) +
      geom_line(lwd = lwd) +
      theme_bw() + 
      theme(legend.position = "none") +
      xlab(time_label) +
      ylab(trait_label) +
      labs(tag = labels[2]) +
      theme(axis.title = element_text(size = axis_label_size)) +
      theme(axis.text = element_text(size = tick_size)) +
      scale_color_brewer(palette=lineage_palette)
    
    return(plot_list)
  }
  
  comb_list = list()
  for (i in seq_along(simulatedEvoModes)){
    comb_list[[simulatedEvoModes[i]]] = compare_evo_modes_time_strat_plot(name = simulatedEvoModes[i],
                                                          labels = LETTERS[c(i,i+3)],
                                                          lwd = evo_mode_lwds[i],
                                                          no_of_lineages = 3)
  }
  
  file_name = paste("figs/R/comparison_pres_of_modes_scenario_", scenario," " , pos, "_raw.pdf", sep = "")
  pdf(file = file_name , width=default_width_fp_in, height = 3.25)
  combined_plot = gridExtra::grid.arrange(comb_list$stasis$strat,
                               comb_list$`Brownian motion`$strat,
                               comb_list$`Brownian drift`$strat,
                               comb_list$stasis$time,
                               comb_list$`Brownian motion`$time,
                               comb_list$`Brownian drift`$time,
                               nrow = 2)
  
  dev.off()
  
}
    
for (scenario in scenarioNames) {
  for (pos in examinedBasinPositions) {
    make_var_pres_of_modes_plot(pos = pos, scenario = scenario, no_of_lineages = 3, plot_seed = 1)
  }
}

#### plot adms from both scenarios ####

make_adm_comparison_plot = function(){
  make_adm_subplot = function(scenario, distances_from_shore_km, label){
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
    suppressWarnings( { ret_plot = ggplot2::ggplot(df, aes(x = t, y = h, col = distance)) +
      geom_line() + 
      theme_bw() + 
      xlab(time_label) +
      ylab(strat_label) +
      labs(tag = label) +
      theme(axis.title = element_text(size = axis_label_size)) +
      theme(axis.text = element_text(size = tick_size)) +
      theme(legend.position = c(0.2, 0.8)) +
      theme(legend.key.size = unit(0.1, "cm")) +
      scale_color_manual(values=adm_palette) +
      ggtitle(paste0("Scenario " , scenario)) +
      theme(plot.title = element_text(size=10))
    } )
    return(ret_plot)
  }
  
  scena_A_admplot = make_adm_subplot(scenario  = "A", distances_from_shore_km = examinedBasinPositions, label = "A")
  scena_B_admplot = make_adm_subplot(scenario  = "B", distances_from_shore_km = examinedBasinPositions, label = "B")
  
  file_name = paste("figs/R/comparison_adms_both_scenario.pdf", sep = "")
  pdf(file = file_name , width=default_width_fp_in, height = 3.25)
  combined_plot = gridExtra::grid.arrange(scena_A_admplot, scena_B_admplot, ncol = 2)
  
  dev.off()
  
}

make_adm_comparison_plot()

