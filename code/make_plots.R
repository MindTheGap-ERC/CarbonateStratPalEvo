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

#### Figure 4 ####
#Figure 

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
{
  #Setting the correct sampling points:
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
}
#### Figure 6 ####
{
  #Setting the correct sampling points:
  ts_lengths = noOfSamplingLoc_time
  #The plots:
Plot6_1=Plot_time(ts_lengths,"A","stasis","A")
Plot6_2=Plot_time(ts_lengths,"A","Brownian motion","B")
Plot6_3=Plot_time(ts_lengths,"A","weak Brownian drift","C")
Plot6_4=Plot_time(ts_lengths,"A","strong Brownian drift","D")

#Combining all the plots:
combined_plot=grid.arrange(Plot6_1,Plot6_2,Plot6_3,Plot6_4, ncol=2)

#Printing the plots plus the legend to .pdf
{
  pdf(file = paste("figs/R/fig6_raw.pdf"), width=6.5, height = 3.25)
  grid.arrange(combined_plot, Legend, nrow = 2, heights = c(10, 1))  #The multiplot
  dev.off()
}
}

#### Plot ???: Completeness and Distribution of hiatus durations ####
## TODO:
# 1. Merge plots with completeness and hiatus duration distribution. Use the left
# y axis for completeness and the right for hiatus duration
# 2. make uniform y axis scale for hiatus duration between scenarios. This is so
# the plots for different scenarios are comparable with each other
# 3. Adjust x axis. should be real distance from shore (in km) instead of the index as here
# 4. add legend for hiatus duration & use different line types for hiatus duration.
# should be "first quartile", "median", "third quartile", and "maximum"

{
#Getting all the input data for the graph:
hiatusA=hiat_measures$A$completeness*100
hiatus_maxA=hiat_measures$A$max_duration_myr
hiatus_medianA=hiat_measures$A$median_duration_myr
hiatus_1st_quartA=hiat_measures$A$first_quartile_duration_myr
hiatus_3rd_quartA=hiat_measures$A$third_quartile_duration_myr
xaxis=(1:150)

dfA=data.frame(x=xaxis,y1=hiatusA,y2=hiatus_maxA,y3=hiatus_medianA,y4=hiatus_1st_quartA,y5=hiatus_3rd_quartA)

#The plot for basin A
Plot_1=ggplot(data = dfA, aes(x=1:150))+
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
  ggtitle(paste("Completeness over scenario A"))+ #for the title
  xlab("Distance from shore (km)")+ # for the x axis label
  ylab("Completenes")+ # for the y axis label
  theme_bw()+ #Makes the background white.
  theme(text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),legend.position="none",plot.title = element_text(size=9)) #Changes text size
  
#Getting all the input data for the graph
hiatusB=hiat_measures$B$completeness*100
hiatus_maxB=hiat_measures$B$max_duration_myr
hiatus_medianB=hiat_measures$B$median_duration_myr
hiatus_1st_quartB=hiat_measures$B$first_quartile_duration_myr
hiatus_3rd_quartB=hiat_measures$B$third_quartile_duration_myr
xaxis=(1:150)

dfB=data.frame(x=xaxis,y1=hiatusB,y2=hiatus_maxB,y3=hiatus_medianB,y4=hiatus_1st_quartB,y5=hiatus_3rd_quartB)

#The plot for basin B
Plot_2=ggplot(data = dfB, aes(x=1:150))+
  geom_line(aes(y=y1), size=1)+
  geom_line(aes(y=y2*130), colour = "red")+
  geom_line(aes(y=y3*130), colour = "blue")+
  geom_line(aes(y=y4*130), colour = "lightblue", linetype = "dashed")+
  geom_line(aes(y=y5*130), colour = "brown", linetype = "dashed")+
  labs(tag = "B")+
  scale_y_continuous(
    limits=c(0,100),
    name = "Completeness",
    sec.axis = sec_axis(~.*130, name="Hiatus duration (Myr)", breaks= seq(0, 12500, by = 2500), labels = c(seq(0, 12500, by = 2500)/1000000))
  )+
  scale_x_continuous(
    breaks= seq(0, 150, by = 25),labels = c(seq(0, 150, by = 25)/10)
  )+
  ggtitle(paste("Completeness over scenario B"))+ #for the title
  xlab("Height (m)")+ # for the x axis label
  ylab("Completenes")+ # for the y axis label
  theme_bw()+ #Makes the background white.
  theme(text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),legend.position="none",plot.title = element_text(size=9)) #Changes text size


#Combining all the plots:
combined_plot=grid.arrange(Plot_1,Plot_2,ncol=2)

#Printing the plots plus the legend to .pdf
{
  pdf(file = paste("figs/R/figUnknown_raw.pdf"), width=6.5, height = 3.25)
  grid.arrange(combined_plot, ncol = 1, heights = c(10, 1))  #The multiplot
  dev.off()
}
}

#### Figure 9 ####

#Inputs for the correct simulated mode of evolution:
Mode=myBD
simulatedmode="Strong Brownian drift"
Mean=10
Deviation=1

  #1. The forming of a full evolutionary record over time at all sampled locations.#

    myTraitValues=c() #Creating vector myTraitValues
    myTraitValues$time=c() #Creating vector myTraitValues$time.
    allTimes=c() #Creating vector allTimes
    for(i in 1:120){ #Values here are the places in the basin for which the evolution is completely simulated
      #Retrieving necessary values:
      p="A"
      AMTimeA=ageDepthModels[[p]][[i]]$time # extract time
      AMHeightA=ageDepthModels[[p]][[i]]$height # extract strat height
       #Calculating height steps every 0,5 meter.
      myHeightsOfObservationsA=seq(0.5,max(ageDepthModels[[p]][[i]]$height)-(max(ageDepthModels[[p]][[i]]$height)/200),by = 0.5)
      
       p="B"
      AMTimeB=ageDepthModels[[p]][[i]]$time # extract time
      AMHeightB=ageDepthModels[[p]][[i]]$height # extract strat height
      #Calculating height steps every 0,5 meter.
      myHeightsOfObservationsB=seq(0.5,max(ageDepthModels[[p]][[i]]$height)-(max(ageDepthModels[[p]][[i]]$height)/200),by = 0.5)
      
      #Adjusting values to remove duplicates:
      adjustAMHeightA=AMHeightA[!duplicated(AMHeightA)] #Adjust height by removing duplicates
      adjustAMTimeA=AMTimeA[!duplicated(AMHeightA)] #Adjust time by removing values where height is duplicated
      adjustAMHeightB=AMHeightB[!duplicated(AMHeightB)] #Adjust height by removing duplicates
      adjustAMTimeB=AMTimeB[!duplicated(AMHeightB)] #Adjust time by removing values where height is duplicated
      
      
      #Transforming the times of observation into stratigraphic height.
      transValA=pointtransform(points= myHeightsOfObservationsA,
                              xdep=adjustAMHeightA,
                              ydep=adjustAMTimeA,
                              direction="height to time", 
                              depositionmodel = "age model")
      
      transValB=pointtransform(points= myHeightsOfObservationsB,
                              xdep=adjustAMHeightB,
                              ydep=adjustAMTimeB,
                              direction="height to time", 
                              depositionmodel = "age model")
      
      #Adds all the transformd times together to formed all the times which would be found by sampling facies at similar distances
      allTimes=c(allTimes,transValA$time,transValB$time)
    } 
    allTimes=sort(allTimes)
    #Removing all the times which apear more than once
    adjustAllTimes=allTimes[!duplicated(allTimes)]
    #Simulating a evolution through all found times
    allTraitValues1=Mode(adjustAllTimes,Mean,Deviation)
    allTraitValues2=Mode(adjustAllTimes,Mean,Deviation)
    allTraitValues3=Mode(adjustAllTimes,Mean,Deviation)
    allTraitValues4=Mode(adjustAllTimes,Mean,Deviation)
  #Plotting the drift at 2km in basin A  
{
  p="A"
     wantedDist=20  
      for (i in wantedDist){ #Value here is the point in the basis at which the graph is formed
        #Retrieving necessary values:
        AMTime=ageDepthModels[[p]][[i]]$time # extract time
        AMHeight=ageDepthModels[[p]][[i]]$height # extract strat height
        #Calculating height steps every 0,5 meter.
        myHeightsOfObservations=seq(0.5,max(ageDepthModels[[p]][[i]]$height)-(max(ageDepthModels[[p]][[i]]$height)/200),by=0.5)
        
        #Adjusting values to remove duplicates:
        adjustAMHeight=AMHeight[!duplicated(AMHeight)] #Adjust height by removing duplicates
        adjustAMTime=AMTime[!duplicated(AMHeight)] #Adjust time by removing values where height is duplicated
        
        #Transforming the times of observation into stratigraphic height.
        transVal=pointtransform(points= myHeightsOfObservations,
                                xdep=adjustAMHeight,
                                ydep=adjustAMTime,
                                direction="height to time", 
                                depositionmodel = "age model")
        #Finding the locations of the simulated evolution over time made before
        sameTime1=match(transVal$time,allTraitValues1$time)
        sameTime2=match(transVal$time,allTraitValues2$time)
        sameTime3=match(transVal$time,allTraitValues3$time)
        sameTime4=match(transVal$time,allTraitValues4$time)
        #Making the vector to put all the values in
        traitValueUncalibrated1=0
        traitValueUncalibrated2=0
        traitValueUncalibrated3=0
        traitValueUncalibrated4=0
        #Retrieving all the required values form the simulated evolutions
        for (i in sameTime1){
          traitValueUncalibrated1=append(traitValueUncalibrated1,allTraitValues1$traitValue[i])
        }
        #Removing the first value which was only there to allow the input in the for-loop.
        traitValue1=traitValueUncalibrated1[-1]
        
        for (i in sameTime2){
          traitValueUncalibrated2=append(traitValueUncalibrated2,allTraitValues2$traitValue[i])
        }
        #Removing the first value which was only there to allow the input in the for-loop.
        traitValue2=traitValueUncalibrated2[-1]
        
        for (i in sameTime3){
          traitValueUncalibrated3=append(traitValueUncalibrated3,allTraitValues3$traitValue[i])
        }
        #Removing the first value which was only there to allow the input in the for-loop.
        traitValue3=traitValueUncalibrated3[-1]
        
        for (i in sameTime4){
          traitValueUncalibrated4=append(traitValueUncalibrated4,allTraitValues4$traitValue[i])
        }
        #Removing the first value which was only there to allow the input in the for-loop.
        traitValue4=traitValueUncalibrated4[-1]
        
        
        #Calculating traitvalues over time and adding time and height to myTraitValues.
        myTraitValues1=c()#Creating the vector so that all other values get inputted correctly
        myTraitValues2=c()
        myTraitValues3=c()
        myTraitValues4=c()
        
        myTraitValues1$traitValue=traitValue1 #Inputting Brownian drift
        myTraitValues2$traitValue=traitValue2
        myTraitValues3$traitValue=traitValue3 
        myTraitValues4$traitValue=traitValue4 
        
        myTraitValues1$height=myHeightsOfObservations #Adding the respective heights to the list
        myTraitValues2$height=myHeightsOfObservations
        myTraitValues3$height=myHeightsOfObservations
        myTraitValues4$height=myHeightsOfObservations
        
        myTraitValues1$time=transVal$time #adding the respective times to the list
        myTraitValues2$time=transVal$time
        myTraitValues3$time=transVal$time
        myTraitValues4$time=transVal$time
        
        
        # Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
        graphs=NA
        graphs[1:length(myTraitValues1$traitValue)]=1
        graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues2$traitValue))]=2
        graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues3$traitValue))]=3
        graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues4$traitValue))]=4
        #This puts all the values together for use in ggplot
        full=NA
        full=c(myTraitValues1$traitValue,myTraitValues2$traitValue,myTraitValues3$traitValue,myTraitValues4$traitValue)
        #This makes sure the x values are coupled to the heights properly
        Timespan=NA
        Timespan=c(myTraitValues1$height,myTraitValues2$height,myTraitValues3$height,myTraitValues4$height)
        #This creates the labels for colours in the graph
        Label=c(rep("run 1",length(myTraitValues1$traitValue)),rep("run 2",length(myTraitValues2$traitValue)),rep("run 3",length(myTraitValues3$traitValue)),rep("run 4",length(myTraitValues4$traitValue)))
        #Creates a dataframe with all the earlier info
        df2=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
        return(df2)
      }
      
      #The plot for all the four lines, forming one graph.
      Plot9_1=ggplot(data = df2, aes(x=x, y=y,col=Distance))+
        geom_line(size=1)+
        labs(tag = "A")+
        ggtitle(paste("Scenario A, ", as.character(wantedDist/10), " km,", sep="", " 32,7% completeness"))+ #for the title
        xlab("Height (m)")+ # for the x axis label
        ylab("Trait Value")+ # for the y axis label
        scale_y_continuous(limit=c(0,30))+
        theme_bw()+ #Makes the background white.
        theme(text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),legend.position="none",plot.title = element_text(size=9)) #Changes text size
    }  
    
   #Plotting the drift at 6 km in basin B
    {
      p="B"
      wantedDist=60  
      for (i in wantedDist){ #Value here is the point in the basis at which the graph is formed
        #Retrieving necessary values:
        AMTime=ageDepthModels[[p]][[i]]$time # extract time
        AMHeight=ageDepthModels[[p]][[i]]$height # extract strat height
        #Calculating height steps every 0,5 meter.
        myHeightsOfObservations=seq(0.5,max(ageDepthModels[[p]][[i]]$height)-(max(ageDepthModels[[p]][[i]]$height)/200),by=0.5)
        
        #Adjusting values to remove duplicates:
        adjustAMHeight=AMHeight[!duplicated(AMHeight)] #Adjust height by removing duplicates
        adjustAMTime=AMTime[!duplicated(AMHeight)] #Adjust time by removing values where height is duplicated
        
        #Transforming the times of observation into stratigraphic height.
        transVal=pointtransform(points= myHeightsOfObservations,
                                xdep=adjustAMHeight,
                                ydep=adjustAMTime,
                                direction="height to time", 
                                depositionmodel = "age model")
        #Finding the locations of the simulated evolution over time made before
        sameTime1=match(transVal$time,allTraitValues1$time)
        sameTime2=match(transVal$time,allTraitValues2$time)
        sameTime3=match(transVal$time,allTraitValues3$time)
        sameTime4=match(transVal$time,allTraitValues4$time)
        #Making the vector to put all the values in
        traitValueUncalibrated1=0
        traitValueUncalibrated2=0
        traitValueUncalibrated3=0
        traitValueUncalibrated4=0
        #Retrieving all the required values form the simulated evolutions
        for (i in sameTime1){
          traitValueUncalibrated1=append(traitValueUncalibrated1,allTraitValues1$traitValue[i])
        }
        #Removing the first value which was only there to allow the input in the for-loop.
        traitValue1=traitValueUncalibrated1[-1]
        
        for (i in sameTime2){
          traitValueUncalibrated2=append(traitValueUncalibrated2,allTraitValues2$traitValue[i])
        }
        #Removing the first value which was only there to allow the input in the for-loop.
        traitValue2=traitValueUncalibrated2[-1]
        
        for (i in sameTime3){
          traitValueUncalibrated3=append(traitValueUncalibrated3,allTraitValues3$traitValue[i])
        }
        #Removing the first value which was only there to allow the input in the for-loop.
        traitValue3=traitValueUncalibrated3[-1]
        
        for (i in sameTime4){
          traitValueUncalibrated4=append(traitValueUncalibrated4,allTraitValues4$traitValue[i])
        }
        #Removing the first value which was only there to allow the input in the for-loop.
        traitValue4=traitValueUncalibrated4[-1]
        
        
        #Calculating traitvalues over time and adding time and height to myTraitValues.
        myTraitValues1=c()#Creating the vector so that all other values get inputted correctly
        myTraitValues2=c()
        myTraitValues3=c()
        myTraitValues4=c()
        
        myTraitValues1$traitValue=traitValue1 #Inputting Brownian drift
        myTraitValues2$traitValue=traitValue2
        myTraitValues3$traitValue=traitValue3 
        myTraitValues4$traitValue=traitValue4 
        
        myTraitValues1$height=myHeightsOfObservations #Adding the respective heights to the list
        myTraitValues2$height=myHeightsOfObservations
        myTraitValues3$height=myHeightsOfObservations
        myTraitValues4$height=myHeightsOfObservations
        
        myTraitValues1$time=transVal$time #adding the respective times to the list
        myTraitValues2$time=transVal$time
        myTraitValues3$time=transVal$time
        myTraitValues4$time=transVal$time
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
        graphs=NA
        graphs[1:length(myTraitValues1$traitValue)]=1
        graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues2$traitValue))]=2
        graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues3$traitValue))]=3
        graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues4$traitValue))]=4
        #This puts all the values together for use in ggplot
        full=NA
        full=c(myTraitValues1$traitValue,myTraitValues2$traitValue,myTraitValues3$traitValue,myTraitValues4$traitValue)
        #This makes sure the x values are coupled to the heights properly
        Timespan=NA
        Timespan=c(myTraitValues1$height,myTraitValues2$height,myTraitValues3$height,myTraitValues4$height)
        #This creates the labels for colours in the graph
        Label=c(rep("run 1",length(myTraitValues1$traitValue)),rep("run 2",length(myTraitValues2$traitValue)),rep("run 3",length(myTraitValues3$traitValue)),rep("run 4",length(myTraitValues4$traitValue)))
        #Creates a dataframe with all the earlier info
        df3=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
        return(df3)
      }
      
      
      #The plot for all the four lines, forming one graph.
      Plot9_2=ggplot(data = df3, aes(x=x, y=y,col=Distance))+
        geom_line(size=1)+
        labs(tag = "B")+
        ggtitle(paste("Scenario B,", as.character(wantedDist/10), " km,", sep="", " 34,5% completeness"))+ #for the title
        xlab("Height (m)")+ # for the x axis label
        ylab("Trait Value")+ # for the y axis label
        scale_y_continuous(limit=c(0,30))+
        theme_bw()+ #Makes the background white.
        theme(text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),legend.position="none",plot.title = element_text(size=9)) #Changes text size
      
}

    #Combining all the plots:
    combined_plot=grid.arrange(Plot9_1,Plot9_2,ncol=2)
    
    #Printing the plots plus the legend to .pdf
    {
      pdf(file = paste("figs/R/fig9_raw.pdf"), width=6.5, height = 3.25)
      grid.arrange(combined_plot, ncol = 1, heights = c(10, 1))  #The multiplot
      dev.off()
    }
  
#### Plot ???: spatial variety in preservation #### 
  #Make figure displaying spatial variability in preservation. Use strong Brownian drift in scenario A
 {  
     #Creates the ADM for platform A
    
    {
      # plot age model with erosion  for Basin A#
      # choose age model
      i=20
      {
        AMTime=ageDepthModels$A[[i]]$time # extract time
        
        AMHeight=ageDepthModels$A[[i]]$height # extract strat height
        
        # times where the values of the age model is determined
        timesOfInterest=seq(0,max(AMTime),length.out=1000)
        
        
        #create age model with hiatuses removed
        
        # determine stratigraphic heights of the times "timesOfInterest"
        AMHiatusesRemoved=pointtransform(points=timesOfInterest, # times that you want to know the strat height of
                                         xdep=AMTime, # tie points in time of the age model
                                         ydep=AMHeight, # tie points in strat height of the age model
                                         direction="time to height", 
                                         depositionmodel = "age model") 
        # stratighraphic heights are contained in AMHiatusesRemoved$height
        # AMHiatusesRemoved$time is a duplicate of timesOfInterest
      }
      HeightI=AMHiatusesRemoved$height
      i=60
      {
        AMTime=ageDepthModels$A[[i]]$time # extract time
        
        AMHeight=ageDepthModels$A[[i]]$height # extract strat height
        
        # times where the values of the age model is determined
        timesOfInterest=seq(0,max(AMTime),length.out=10000)
        
        
        #create age model with hiatuses removed
        
        # determine stratigraphic heights of the times "timesOfInterest"
        AMHiatusesRemoved=pointtransform(points=timesOfInterest, # times that you want to know the strat height of
                                         xdep=AMTime, # tie points in time of the age model
                                         ydep=AMHeight, # tie points in strat height of the age model
                                         direction="time to height", 
                                         depositionmodel = "age model") 
        # stratighraphic heights are contained in AMHiatusesRemoved$height
        # AMHiatusesRemoved$time is a duplicate of timesOfInterest
      }
      HeightII=AMHiatusesRemoved$height
      i=80
      {
        AMTime=ageDepthModels$A[[i]]$time # extract time
        
        AMHeight=ageDepthModels$A[[i]]$height # extract strat height
        
        # times where the values of the age model is determined
        timesOfInterest=seq(0,max(AMTime),length.out=10000)
        
        
        #create age model with hiatuses removed
        
        # determine stratigraphic heights of the times "timesOfInterest"
        AMHiatusesRemoved=pointtransform(points=timesOfInterest, # times that you want to know the strat height of
                                         xdep=AMTime, # tie points in time of the age model
                                         ydep=AMHeight, # tie points in strat height of the age model
                                         direction="time to height", 
                                         depositionmodel = "age model") 
        # stratighraphic heights are contained in AMHiatusesRemoved$height
        # AMHiatusesRemoved$time is a duplicate of timesOfInterest
      }
      HeightIII=AMHiatusesRemoved$height
      i=100
      {
        AMTime=ageDepthModels$A[[i]]$time # extract time
        
        AMHeight=ageDepthModels$A[[i]]$height # extract strat height
        
        # times where the values of the age model is determined
        timesOfInterest=seq(0,max(AMTime),length.out=10000)
        
        
        #create age model with hiatuses removed
        
        # determine stratigraphic heights of the times "timesOfInterest"
        AMHiatusesRemoved=pointtransform(points=timesOfInterest, # times that you want to know the strat height of
                                         xdep=AMTime, # tie points in time of the age model
                                         ydep=AMHeight, # tie points in strat height of the age model
                                         direction="time to height", 
                                         depositionmodel = "age model") 
        # stratighraphic heights are contained in AMHiatusesRemoved$height
        # AMHiatusesRemoved$time is a duplicate of timesOfInterest
      }
      HeightIV=AMHiatusesRemoved$height
      i=120
      {
        AMTime=ageDepthModels$A[[i]]$time # extract time
        
        AMHeight=ageDepthModels$A[[i]]$height # extract strat height
        
        # times where the values of the age model is determined
        timesOfInterest=seq(0,max(AMTime),length.out=10000)
        
        
        #create age model with hiatuses removed
        
        # determine stratigraphic heights of the times "timesOfInterest"
        AMHiatusesRemoved=pointtransform(points=timesOfInterest, # times that you want to know the strat height of
                                         xdep=AMTime, # tie points in time of the age model
                                         ydep=AMHeight, # tie points in strat height of the age model
                                         direction="time to height", 
                                         depositionmodel = "age model") 
        # stratighraphic heights are contained in AMHiatusesRemoved$height
        # AMHiatusesRemoved$time is a duplicate of timesOfInterest
      }
      HeightV=AMHiatusesRemoved$height
      #These are the 4 lines of the graph. Use each of these after a run of the code before this
      
      
      {
        #This makes the 4 graphs detectable by ggplot
        graphs=NA
        graphs[1:10000]=1
        graphs[10001:20000]=2
        graphs[20001:30000]=3
        graphs[30001:40000]=4
        graphs[40001:50000]=5
        #This puts all the values together for use in ggplot
        full=NA
        full=c(HeightI,HeightII,HeightIII,HeightIV,HeightV)
        #This makes sure the x values are coupled to the heights properly
        Timespan=NA
        Timespan=c(AMHiatusesRemoved$time,AMHiatusesRemoved$time,AMHiatusesRemoved$time,AMHiatusesRemoved$time,AMHiatusesRemoved$time)
        #This creates the labels for colours in the graph
        Label=c(rep("2 km",10000),rep("6 km",10000),rep("8 km",10000),rep("10 km",10000),rep("12 km",10000))
        #Creates a dataframe with all the earlier info
        df=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
      } #Puts everything into the righ format for the graphs.
      #The plot for all the four lines, forming one graph.
      ADM_A=ggplot(data = df, aes(x=x, y=y,col=Distance))+
        geom_path(size=1)+
        labs(tag = "A")+
        ggtitle("Age-Depth Model")+ #for the title
        xlab("Time (Myr)")+ # for the x axis label
        ylab("Height (m)")+ # for the y axis label
        theme_bw()+ #Makes the background white.
        scale_colour_brewer(breaks=c( "2 km", '6 km', '8 km', "10 km", "12 km"),palette="Set1")+
        theme(text = element_text(size = 8), 
              plot.tag = element_text(face = "bold"), 
              plot.tag.position = c(0.01, 0.98),
              plot.title = element_text(size=9), 
              legend.key.size = unit(1, 'cm'), 
              legend.key.height = unit(0.1, 'cm'), 
              legend.key.width = unit(1, 'cm'), 
              legend.text = element_text(size = 5),
              legend.title = element_text(size = 5),
              legend.position = c(0.005, .99),
              legend.justification = c("left", "top"),
              legend.box.just = "left",
              legend.margin = margin(2, 2, 2, 2),
              legend.box.background = element_rect(color="black", size=0.25)
        ) 
      ADM_A
    }
   
    {
      Mode=myBD
      simulatedmode="Strong Brownian drift"
      Mean=10
      Deviation=1
      
      #Basin A:
      p = "A"
      {
        #1. The forming of a full evolutionary record over time at all sampled locations.#
        {
          myTraitValues=c() #Creating vector myTraitValues
          myTraitValues$time=c() #Creating vector myTraitValues$time.
          allTimes=c() #Creating vector allTimes
          for(i in 1:120){ #Values here are the places in the basin for which the evolution is completely simulated
            #Retrieving necessary values:
            AMTime=ageDepthModels[[p]][[i]]$time # extract time
            AMHeight=ageDepthModels[[p]][[i]]$height # extract strat height
            
            #Calculating height steps every 0,5 meter.
            myHeightsOfObservations=seq(0.5,max(ageDepthModels[[p]][[i]]$height)-(max(ageDepthModels[[p]][[i]]$height)/200),by = 0.5)
            
            #Adjusting values to remove duplicates:
            adjustAMHeight=AMHeight[!duplicated(AMHeight)] #Adjust height by removing duplicates
            adjustAMTime=AMTime[!duplicated(AMHeight)] #Adjust time by removing values where height is du(plicated
            
            #Transforming the times of observation into stratigraphic height.
            transVal=pointtransform(points= myHeightsOfObservations,
                                    xdep=adjustAMHeight,
                                    ydep=adjustAMTime,
                                    direction="height to time", 
                                    depositionmodel = "age model")
            #Adds all the transformd times together to formed all the times which would be found by sampling facies at similar distances
            allTimes=c(allTimes,transVal$time)
          } 
          #Removing all the times which apear more than once
          adjustAllTimes=allTimes[!duplicated(allTimes)]
          #Simulating a evolution through all found times
          allTraitValues1=Mode(adjustAllTimes,Mean,Deviation)
          allTraitValues2=Mode(adjustAllTimes,Mean,Deviation)
          allTraitValues3=Mode(adjustAllTimes,Mean,Deviation)
          allTraitValues4=Mode(adjustAllTimes,Mean,Deviation)
          
          #2. Making the trait values over time graph#
          #This makes the 5 graphs detectable by ggplot
          graphs=NA
          graphs[1:length(allTraitValues1$traitValue)]=1
          graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues2$traitValue))]=2
          graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues3$traitValue))]=3
          graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues4$traitValue))]=4
          #This puts all the values together for use in ggplot
          full=NA
          full=c(allTraitValues1$traitValue,allTraitValues2$traitValue,allTraitValues3$traitValue,allTraitValues4$traitValue)
          #This makes sure the x values are coupled to the heights properly
          Timespan=NA
          Timespan=c(rep(allTraitValues1$time,4))
          #This creates the labels for colours in the graph
          Label=c(rep("run 1",length(adjustAllTimes)),rep("run 2",length(adjustAllTimes)),rep("run 3",length(adjustAllTimes)),rep("run 4",length(adjustAllTimes)))
          #Creates a dataframe with all the earlier info
          df=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
          
          #The plot for all the four lines, forming one graph.
          PlotTT=ggplot(data = df, aes(x=x, y=y,col=Distance))+
            geom_line(size=1)+
            labs(tag = "B")+
            ggtitle("Time Domain")+ #for the title
            xlab("Time (Myr)")+ # for the x axis label
            ylab("Trait Value")+ # for the y axis label
            theme_bw()+ #Makes the background white.
            theme(text = element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98), legend.position="none",plot.title = element_text(size=9)) #Changes text size 
          
        }
        
        #3.1. The plotting of the evolution as found in at distance 20
        #postition to be sampled in the platform.
        {
          wantedDist=20  
          for (i in wantedDist){ #Value here is the point in the basis at which the graph is formed
            #Retrieving necessary values:
            AMTime=ageDepthModels[[p]][[i]]$time # extract time
            AMHeight=ageDepthModels[[p]][[i]]$height # extract strat height
            #Calculating height steps every 0,5 meter.
            myHeightsOfObservations=seq(0.5,max(ageDepthModels[[p]][[i]]$height)-(max(ageDepthModels[[p]][[i]]$height)/200),by=0.5)
            
            #Adjusting values to remove duplicates:
            adjustAMHeight=AMHeight[!duplicated(AMHeight)] #Adjust height by removing duplicates
            adjustAMTime=AMTime[!duplicated(AMHeight)] #Adjust time by removing values where height is duplicated
            
            #Transforming the times of observation into stratigraphic height.
            transVal=pointtransform(points= myHeightsOfObservations,
                                    xdep=adjustAMHeight,
                                    ydep=adjustAMTime,
                                    direction="height to time", 
                                    depositionmodel = "age model")
            #Finding the locations of the simulated evolution over time made before
            sameTime1=match(transVal$time,allTraitValues1$time)
            sameTime2=match(transVal$time,allTraitValues2$time)
            sameTime3=match(transVal$time,allTraitValues3$time)
            sameTime4=match(transVal$time,allTraitValues4$time)
            #Making the vector to put all the values in
            traitValueUncalibrated1=0
            traitValueUncalibrated2=0
            traitValueUncalibrated3=0
            traitValueUncalibrated4=0
            #Retrieving all the required values form the simulated evolutions
            for (i in sameTime1){
              traitValueUncalibrated1=append(traitValueUncalibrated1,allTraitValues1$traitValue[i])
            }
            #Removing the first value which was only there to allow the input in the for-loop.
            traitValue1=traitValueUncalibrated1[-1]
            
            for (i in sameTime2){
              traitValueUncalibrated2=append(traitValueUncalibrated2,allTraitValues2$traitValue[i])
            }
            #Removing the first value which was only there to allow the input in the for-loop.
            traitValue2=traitValueUncalibrated2[-1]
            
            for (i in sameTime3){
              traitValueUncalibrated3=append(traitValueUncalibrated3,allTraitValues3$traitValue[i])
            }
            #Removing the first value which was only there to allow the input in the for-loop.
            traitValue3=traitValueUncalibrated3[-1]
            
            for (i in sameTime4){
              traitValueUncalibrated4=append(traitValueUncalibrated4,allTraitValues4$traitValue[i])
            }
            #Removing the first value which was only there to allow the input in the for-loop.
            traitValue4=traitValueUncalibrated4[-1]
            
            
            #Calculating traitvalues over time and adding time and height to myTraitValues.
            myTraitValues1=c()#Creating the vector so that all other values get inputted correctly
            myTraitValues2=c()
            myTraitValues3=c()
            myTraitValues4=c()
            
            myTraitValues1$traitValue=traitValue1 #Inputting Brownian drift
            myTraitValues2$traitValue=traitValue2
            myTraitValues3$traitValue=traitValue3 
            myTraitValues4$traitValue=traitValue4 
            
            myTraitValues1$height=myHeightsOfObservations #Adding the respective heights to the list
            myTraitValues2$height=myHeightsOfObservations
            myTraitValues3$height=myHeightsOfObservations
            myTraitValues4$height=myHeightsOfObservations
            
            myTraitValues1$time=transVal$time #adding the respective times to the list
            myTraitValues2$time=transVal$time
            myTraitValues3$time=transVal$time
            myTraitValues4$time=transVal$time
            
            
            # Making the trait values over time graph
            #This makes the 5 graphs detectable by ggplot  
            graphs=NA
            graphs[1:length(myTraitValues1$traitValue)]=1
            graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues2$traitValue))]=2
            graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues3$traitValue))]=3
            graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues4$traitValue))]=4
            #This puts all the values together for use in ggplot
            full=NA
            full=c(myTraitValues1$traitValue,myTraitValues2$traitValue,myTraitValues3$traitValue,myTraitValues4$traitValue)
            #This makes sure the x values are coupled to the heights properly
            Timespan=NA
            Timespan=c(myTraitValues1$height,myTraitValues2$height,myTraitValues3$height,myTraitValues4$height)
            #This creates the labels for colours in the graph
            Label=c(rep("run 1",length(myTraitValues1$traitValue)),rep("run 2",length(myTraitValues2$traitValue)),rep("run 3",length(myTraitValues3$traitValue)),rep("run 4",length(myTraitValues4$traitValue)))
            #Creates a dataframe with all the earlier info
            df2=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
            return(df2)
          }
          
          
          #The plot for all the four lines, forming one graph.
          Plot1=ggplot(data = df2, aes(x=x, y=y,col=Distance))+
            geom_line(size=1)+
            labs(tag = "C")+
            ggtitle(paste("Distance From Shore: ", as.character(wantedDist/10), " km", sep=""))+ #for the title
            xlab("Height (m)")+ # for the x axis label
            ylab("Trait Value")+ # for the y axis label
            theme_bw()+ #Makes the background white.
            theme(text = element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),legend.position="none",plot.title = element_text(size=9)) #Changes text size
        }
        #3.2. The plotting of the evolution as found in at distance 60
        {wantedDist=60  
          for (i in wantedDist){ #Value here is the point in the basis at which the graph is formed
            #Retrieving necessary values:
            AMTime=ageDepthModels[[p]][[i]]$time # extract time
            AMHeight=ageDepthModels[[p]][[i]]$height # extract strat height
            #Calculating height steps every 0,5 meter.
            myHeightsOfObservations=seq(0.5,max(ageDepthModels[[p]][[i]]$height)-(max(ageDepthModels[[p]][[i]]$height)/200),by=0.5)
            
            #Adjusting values to remove duplicates:
            adjustAMHeight=AMHeight[!duplicated(AMHeight)] #Adjust height by removing duplicates
            adjustAMTime=AMTime[!duplicated(AMHeight)] #Adjust time by removing values where height is duplicated
            
            #Transforming the times of observation into stratigraphic height.
            transVal=pointtransform(points= myHeightsOfObservations,
                                    xdep=adjustAMHeight,
                                    ydep=adjustAMTime,
                                    direction="height to time", 
                                    depositionmodel = "age model")
            #Finding the locations of the simulated evolution over time made before
            sameTime1=match(transVal$time,allTraitValues1$time)
            sameTime2=match(transVal$time,allTraitValues2$time)
            sameTime3=match(transVal$time,allTraitValues3$time)
            sameTime4=match(transVal$time,allTraitValues4$time)
            #Making the vector to put all the values in
            traitValueUncalibrated1=0
            traitValueUncalibrated2=0
            traitValueUncalibrated3=0
            traitValueUncalibrated4=0
            #Retrieving all the required values form the simulated evolutions
            for (i in sameTime1){
              traitValueUncalibrated1=append(traitValueUncalibrated1,allTraitValues1$traitValue[i])
            }
            #Removing the first value which was only there to allow the input in the for-loop.
            traitValue1=traitValueUncalibrated1[-1]
            
            for (i in sameTime2){
              traitValueUncalibrated2=append(traitValueUncalibrated2,allTraitValues2$traitValue[i])
            }
            #Removing the first value which was only there to allow the input in the for-loop.
            traitValue2=traitValueUncalibrated2[-1]
            
            for (i in sameTime3){
              traitValueUncalibrated3=append(traitValueUncalibrated3,allTraitValues3$traitValue[i])
            }
            #Removing the first value which was only there to allow the input in the for-loop.
            traitValue3=traitValueUncalibrated3[-1]
            
            for (i in sameTime4){
              traitValueUncalibrated4=append(traitValueUncalibrated4,allTraitValues4$traitValue[i])
            }
            #Removing the first value which was only there to allow the input in the for-loop.
            traitValue4=traitValueUncalibrated4[-1]
            
            
            #Calculating traitvalues over time and adding time and height to myTraitValues.
            myTraitValues1=c()#Creating the vector so that all other values get inputted correctly
            myTraitValues2=c()
            myTraitValues3=c()
            myTraitValues4=c()
            
            myTraitValues1$traitValue=traitValue1 #Inputting Brownian drift
            myTraitValues2$traitValue=traitValue2
            myTraitValues3$traitValue=traitValue3 
            myTraitValues4$traitValue=traitValue4 
            
            myTraitValues1$height=myHeightsOfObservations #Adding the respective heights to the list
            myTraitValues2$height=myHeightsOfObservations
            myTraitValues3$height=myHeightsOfObservations
            myTraitValues4$height=myHeightsOfObservations
            
            myTraitValues1$time=transVal$time #adding the respective times to the list
            myTraitValues2$time=transVal$time
            myTraitValues3$time=transVal$time
            myTraitValues4$time=transVal$time
            
            
            #Making the trait values over time graph
            #This makes the 5 graphs detectable by ggplot  
            graphs=NA
            graphs[1:length(myTraitValues1$traitValue)]=1
            graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues2$traitValue))]=2
            graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues3$traitValue))]=3
            graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues4$traitValue))]=4
            #This puts all the values together for use in ggplot
            full=NA
            full=c(myTraitValues1$traitValue,myTraitValues2$traitValue,myTraitValues3$traitValue,myTraitValues4$traitValue)
            #This makes sure the x values are coupled to the heights properly
            Timespan=NA
            Timespan=c(myTraitValues1$height,myTraitValues2$height,myTraitValues3$height,myTraitValues4$height)
            #This creates the labels for colours in the graph
            Label=c(rep("run 1",length(myTraitValues1$traitValue)),rep("run 2",length(myTraitValues2$traitValue)),rep("run 3",length(myTraitValues3$traitValue)),rep("run 4",length(myTraitValues4$traitValue)))
            #Creates a dataframe with all the earlier info
            df3=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
            return(df3)
          }
          
          
          #The plot for all the four lines, forming one graph.
          Plot2=ggplot(data = df3, aes(x=x, y=y,col=Distance))+
            geom_line(size=1)+
            labs(tag = "D")+
            ggtitle(paste("Distance From Shore: ", as.character(wantedDist/10), " km", sep=""))+ #for the title
            xlab("Height (m)")+ # for the x axis label
            ylab("Trait Value")+ # for the y axis label
            theme_bw()+ #Makes the background white.
            theme(text = element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),legend.position="none",plot.title = element_text(size=9)) #Changes text size
        }
        #3.3. The plotting of the evolution as found in at distance 80
        {wantedDist=80  
          for (i in wantedDist){ #Value here is the point in the basis at which the graph is formed
            #Retrieving necessary values:
            AMTime=ageDepthModels[[p]][[i]]$time # extract time
            AMHeight=ageDepthModels[[p]][[i]]$height # extract strat height
            #Calculating height steps every 0,5 meter.
            myHeightsOfObservations=seq(0.5,max(ageDepthModels[[p]][[i]]$height)-(max(ageDepthModels[[p]][[i]]$height)/200),by=0.5)
            
            #Adjusting values to remove duplicates:
            adjustAMHeight=AMHeight[!duplicated(AMHeight)] #Adjust height by removing duplicates
            adjustAMTime=AMTime[!duplicated(AMHeight)] #Adjust time by removing values where height is duplicated
            
            #Transforming the times of observation into stratigraphic height.
            transVal=pointtransform(points= myHeightsOfObservations,
                                    xdep=adjustAMHeight,
                                    ydep=adjustAMTime,
                                    direction="height to time", 
                                    depositionmodel = "age model")
            #Finding the locations of the simulated evolution over time made before
            sameTime1=match(transVal$time,allTraitValues1$time)
            sameTime2=match(transVal$time,allTraitValues2$time)
            sameTime3=match(transVal$time,allTraitValues3$time)
            sameTime4=match(transVal$time,allTraitValues4$time)
            #Making the vector to put all the values in
            traitValueUncalibrated1=0
            traitValueUncalibrated2=0
            traitValueUncalibrated3=0
            traitValueUncalibrated4=0
            #Retrieving all the required values form the simulated evolutions
            for (i in sameTime1){
              traitValueUncalibrated1=append(traitValueUncalibrated1,allTraitValues1$traitValue[i])
            }
            #Removing the first value which was only there to allow the input in the for-loop.
            traitValue1=traitValueUncalibrated1[-1]
            
            for (i in sameTime2){
              traitValueUncalibrated2=append(traitValueUncalibrated2,allTraitValues2$traitValue[i])
            }
            #Removing the first value which was only there to allow the input in the for-loop.
            traitValue2=traitValueUncalibrated2[-1]
            
            for (i in sameTime3){
              traitValueUncalibrated3=append(traitValueUncalibrated3,allTraitValues3$traitValue[i])
            }
            #Removing the first value which was only there to allow the input in the for-loop.
            traitValue3=traitValueUncalibrated3[-1]
            
            for (i in sameTime4){
              traitValueUncalibrated4=append(traitValueUncalibrated4,allTraitValues4$traitValue[i])
            }
            #Removing the first value which was only there to allow the input in the for-loop.
            traitValue4=traitValueUncalibrated4[-1]
            
            
            #Calculating traitvalues over time and adding time and height to myTraitValues.
            myTraitValues1=c()#Creating the vector so that all other values get inputted correctly
            myTraitValues2=c()
            myTraitValues3=c()
            myTraitValues4=c()
            
            myTraitValues1$traitValue=traitValue1 #Inputting Brownian drift
            myTraitValues2$traitValue=traitValue2
            myTraitValues3$traitValue=traitValue3 
            myTraitValues4$traitValue=traitValue4 
            
            myTraitValues1$height=myHeightsOfObservations #Adding the respective heights to the list
            myTraitValues2$height=myHeightsOfObservations
            myTraitValues3$height=myHeightsOfObservations
            myTraitValues4$height=myHeightsOfObservations
            
            myTraitValues1$time=transVal$time #adding the respective times to the list
            myTraitValues2$time=transVal$time
            myTraitValues3$time=transVal$time
            myTraitValues4$time=transVal$time
            
            
            #Making the trait values over time graph#
            #This makes the 5 graphs detectable by ggplot  
            graphs=NA
            graphs[1:length(myTraitValues1$traitValue)]=1
            graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues2$traitValue))]=2
            graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues3$traitValue))]=3
            graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues4$traitValue))]=4
            #This puts all the values together for use in ggplot
            full=NA
            full=c(myTraitValues1$traitValue,myTraitValues2$traitValue,myTraitValues3$traitValue,myTraitValues4$traitValue)
            #This makes sure the x values are coupled to the heights properly
            Timespan=NA
            Timespan=c(myTraitValues1$height,myTraitValues2$height,myTraitValues3$height,myTraitValues4$height)
            #This creates the labels for colours in the graph
            Label=c(rep("run 1",length(myTraitValues1$traitValue)),rep("run 2",length(myTraitValues2$traitValue)),rep("run 3",length(myTraitValues3$traitValue)),rep("run 4",length(myTraitValues4$traitValue)))
            #Creates a dataframe with all the earlier info
            df4=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
            return(df4)
          }
          
          
          #The plot for all the four lines, forming one graph.
          Plot3=ggplot(data = df4, aes(x=x, y=y,col=Distance))+
            geom_line(size=1)+
            labs(tag = "E")+
            ggtitle(paste("Distance From Shore: ", as.character(wantedDist/10), " km", sep=""))+ #for the title
            xlab("Height (m)")+ # for the x axis label
            ylab("Trait Value")+ # for the y axis label
            theme_bw()+ #Makes the background white.
            theme(text = element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),legend.position="none",plot.title = element_text(size=9)) #Changes text size
        }
        #3.4. The plotting of the evolution as found in at distance 100
        {wantedDist=100  
          for (i in wantedDist){ #Value here is the point in the basis at which the graph is formed
            #Retrieving necessary values:
            AMTime=ageDepthModels[[p]][[i]]$time # extract time
            AMHeight=ageDepthModels[[p]][[i]]$height # extract strat height
            #Calculating height steps every 0,5 meter.
            myHeightsOfObservations=seq(0.5,max(ageDepthModels[[p]][[i]]$height)-(max(ageDepthModels[[p]][[i]]$height)/200),by=0.5)
            
            #Adjusting values to remove duplicates:
            adjustAMHeight=AMHeight[!duplicated(AMHeight)] #Adjust height by removing duplicates
            adjustAMTime=AMTime[!duplicated(AMHeight)] #Adjust time by removing values where height is duplicated
            
            #Transforming the times of observation into stratigraphic height.
            transVal=pointtransform(points= myHeightsOfObservations,
                                    xdep=adjustAMHeight,
                                    ydep=adjustAMTime,
                                    direction="height to time", 
                                    depositionmodel = "age model")
            #Finding the locations of the simulated evolution over time made before
            sameTime1=match(transVal$time,allTraitValues1$time)
            sameTime2=match(transVal$time,allTraitValues2$time)
            sameTime3=match(transVal$time,allTraitValues3$time)
            sameTime4=match(transVal$time,allTraitValues4$time)
            #Making the vector to put all the values in
            traitValueUncalibrated1=0
            traitValueUncalibrated2=0
            traitValueUncalibrated3=0
            traitValueUncalibrated4=0
            #Retrieving all the required values form the simulated evolutions
            for (i in sameTime1){
              traitValueUncalibrated1=append(traitValueUncalibrated1,allTraitValues1$traitValue[i])
            }
            #Removing the first value which was only there to allow the input in the for-loop.
            traitValue1=traitValueUncalibrated1[-1]
            
            for (i in sameTime2){
              traitValueUncalibrated2=append(traitValueUncalibrated2,allTraitValues2$traitValue[i])
            }
            #Removing the first value which was only there to allow the input in the for-loop.
            traitValue2=traitValueUncalibrated2[-1]
            
            for (i in sameTime3){
              traitValueUncalibrated3=append(traitValueUncalibrated3,allTraitValues3$traitValue[i])
            }
            #Removing the first value which was only there to allow the input in the for-loop.
            traitValue3=traitValueUncalibrated3[-1]
            
            for (i in sameTime4){
              traitValueUncalibrated4=append(traitValueUncalibrated4,allTraitValues4$traitValue[i])
            }
            #Removing the first value which was only there to allow the input in the for-loop.
            traitValue4=traitValueUncalibrated4[-1]
            
            
            #Calculating traitvalues over time and adding time and height to myTraitValues.
            myTraitValues1=c()#Creating the vector so that all other values get inputted correctly
            myTraitValues2=c()
            myTraitValues3=c()
            myTraitValues4=c()
            
            myTraitValues1$traitValue=traitValue1 #Inputting Brownian drift
            myTraitValues2$traitValue=traitValue2
            myTraitValues3$traitValue=traitValue3 
            myTraitValues4$traitValue=traitValue4 
            
            myTraitValues1$height=myHeightsOfObservations #Adding the respective heights to the list
            myTraitValues2$height=myHeightsOfObservations
            myTraitValues3$height=myHeightsOfObservations
            myTraitValues4$height=myHeightsOfObservations
            
            myTraitValues1$time=transVal$time #adding the respective times to the list
            myTraitValues2$time=transVal$time
            myTraitValues3$time=transVal$time
            myTraitValues4$time=transVal$time
            
            
            #Making the trait values over time graph
            #This makes the 5 graphs detectable by ggplot  
            graphs=NA
            graphs[1:length(myTraitValues1$traitValue)]=1
            graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues2$traitValue))]=2
            graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues3$traitValue))]=3
            graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues4$traitValue))]=4
            #This puts all the values together for use in ggplot
            full=NA
            full=c(myTraitValues1$traitValue,myTraitValues2$traitValue,myTraitValues3$traitValue,myTraitValues4$traitValue)
            #This makes sure the x values are coupled to the heights properly
            Timespan=NA
            Timespan=c(myTraitValues1$height,myTraitValues2$height,myTraitValues3$height,myTraitValues4$height)
            #This creates the labels for colours in the graph
            Label=c(rep("run 1",length(myTraitValues1$traitValue)),rep("run 2",length(myTraitValues2$traitValue)),rep("run 3",length(myTraitValues3$traitValue)),rep("run 4",length(myTraitValues4$traitValue)))
            #Creates a dataframe with all the earlier info
            df5=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
            return(df5)
          }
          
          
          #The plot for all the four lines, forming one graph.
          Plot4=ggplot(data = df5, aes(x=x, y=y,col=Distance))+
            geom_line(size=1)+
            labs(tag = "F")+
            ggtitle(paste("Distance From Shore: ", as.character(wantedDist/10), " km", sep=""))+ #for the title
            xlab("Height (m)")+ # for the x axis label
            ylab("Trait Value")+ # for the y axis label
            theme_bw()+ #Makes the background white.
            theme(text = element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),legend.position="none",plot.title = element_text(size=9)) #Changes text size
        }
        #3.5. The plotting of the evolution as found in at distance 120
        {wantedDist=120  
          for (i in wantedDist){ #Value here is the point in the basis at which the graph is formed
            #Retrieving necessary values:
            AMTime=ageDepthModels[[p]][[i]]$time # extract time
            AMHeight=ageDepthModels[[p]][[i]]$height # extract strat height
            #Calculating height steps every 0,5 meter.
            myHeightsOfObservations=seq(0.5,max(ageDepthModels[[p]][[i]]$height)-(max(ageDepthModels[[p]][[i]]$height)/200),by=0.5)
            
            #Adjusting values to remove duplicates:
            adjustAMHeight=AMHeight[!duplicated(AMHeight)] #Adjust height by removing duplicates
            adjustAMTime=AMTime[!duplicated(AMHeight)] #Adjust time by removing values where height is duplicated
            
            #Transforming the times of observation into stratigraphic height.
            transVal=pointtransform(points= myHeightsOfObservations,
                                    xdep=adjustAMHeight,
                                    ydep=adjustAMTime,
                                    direction="height to time", 
                                    depositionmodel = "age model")
            #Finding the locations of the simulated evolution over time made before
            sameTime1=match(transVal$time,allTraitValues1$time)
            sameTime2=match(transVal$time,allTraitValues2$time)
            sameTime3=match(transVal$time,allTraitValues3$time)
            sameTime4=match(transVal$time,allTraitValues4$time)
            #Making the vector to put all the values in
            traitValueUncalibrated1=0
            traitValueUncalibrated2=0
            traitValueUncalibrated3=0
            traitValueUncalibrated4=0
            #Retrieving all the required values form the simulated evolutions
            for (i in sameTime1){
              traitValueUncalibrated1=append(traitValueUncalibrated1,allTraitValues1$traitValue[i])
            }
            #Removing the first value which was only there to allow the input in the for-loop.
            traitValue1=traitValueUncalibrated1[-1]
            
            for (i in sameTime2){
              traitValueUncalibrated2=append(traitValueUncalibrated2,allTraitValues2$traitValue[i])
            }
            #Removing the first value which was only there to allow the input in the for-loop.
            traitValue2=traitValueUncalibrated2[-1]
            
            for (i in sameTime3){
              traitValueUncalibrated3=append(traitValueUncalibrated3,allTraitValues3$traitValue[i])
            }
            #Removing the first value which was only there to allow the input in the for-loop.
            traitValue3=traitValueUncalibrated3[-1]
            
            for (i in sameTime4){
              traitValueUncalibrated4=append(traitValueUncalibrated4,allTraitValues4$traitValue[i])
            }
            #Removing the first value which was only there to allow the input in the for-loop.
            traitValue4=traitValueUncalibrated4[-1]
            
            
            #Calculating traitvalues over time and adding time and height to myTraitValues.
            myTraitValues1=c()#Creating the vector so that all other values get inputted correctly
            myTraitValues2=c()
            myTraitValues3=c()
            myTraitValues4=c()
            
            myTraitValues1$traitValue=traitValue1 #Inputting Brownian drift
            myTraitValues2$traitValue=traitValue2
            myTraitValues3$traitValue=traitValue3 
            myTraitValues4$traitValue=traitValue4 
            
            myTraitValues1$height=myHeightsOfObservations #Adding the respective heights to the list
            myTraitValues2$height=myHeightsOfObservations
            myTraitValues3$height=myHeightsOfObservations
            myTraitValues4$height=myHeightsOfObservations
            
            myTraitValues1$time=transVal$time #adding the respective times to the list
            myTraitValues2$time=transVal$time
            myTraitValues3$time=transVal$time
            myTraitValues4$time=transVal$time
            
            
            #Making the trait values over time graph
            #This makes the 5 graphs detectable by ggplot  
            graphs=NA
            graphs[1:length(myTraitValues1$traitValue)]=1
            graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues2$traitValue))]=2
            graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues3$traitValue))]=3
            graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues4$traitValue))]=4
            #This puts all the values together for use in ggplot
            full=NA
            full=c(myTraitValues1$traitValue,myTraitValues2$traitValue,myTraitValues3$traitValue,myTraitValues4$traitValue)
            #This makes sure the x values are coupled to the heights properly
            Timespan=NA
            Timespan=c(myTraitValues1$height,myTraitValues2$height,myTraitValues3$height,myTraitValues4$height)
            #This creates the labels for colours in the graph
            Label=c(rep("run 1",length(myTraitValues1$traitValue)),rep("run 2",length(myTraitValues2$traitValue)),rep("run 3",length(myTraitValues3$traitValue)),rep("run 4",length(myTraitValues4$traitValue)))
            #Creates a dataframe with all the earlier info
            df6=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
            return(df6)
          }
          
          
          #The plot for all the four lines, forming one graph.
          Plot5=ggplot(data = df6, aes(x=x, y=y,col=Distance))+
            geom_line(size=1)+
            labs(tag = "G")+
            ggtitle(paste("Distance From Shore: ", as.character(wantedDist/10), " km", sep=""))+ #for the title
            xlab("Height (m)")+ # for the x axis label
            ylab("Trait Value")+ # for the y axis label
            theme_bw()+ #Makes the background white.
            theme(text = element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),legend.position="none",plot.title = element_text(size=9)) #Changes text size
        }
        {
          pdf(file = paste("figs/R/figVariable_raw.pdf"), width=6.5, height = 7.5)
          
          multiplot(ADM_A,Plot1,NA,PlotTT,Plot2,Plot4,NA,Plot3,Plot5,cols=3) #The multiplot
          dev.off()
        }
      }
    }
  }        
    