#Creates the trait value over height graphs for both platforms


# 1. Dependencies:
#choose the platform:
load("data/R_outputs/age_depth_models.Rdata")

myBM=function(t, mu=0, sigma=1){
  #Sort 't' and adds a vector "variance" with the same length as 't'
  t=sort(t)
  
  ##PART 1: Brownian motion
  #Gets the variance between values from t.
  variance=diff(t)
  #Calculates the deviation from the square root of variance
  deviation=sqrt(variance)
  #Generates a random number according to the deviation calculated earlier.
  rNorm=rnorm(length(t)-1, mean=0, sd=deviation)
  #Sets the initial value of the trait
  startpoint=0
  #Generates the values by adding all the random numbers together.
  brownMotion=cumsum(c(startpoint,rNorm))
  
  ## PART 2: Calculate the drift based on the brownian motion
  #Calculates the drift
  traitValues=mu*t+sigma*brownMotion
  
  ## PART 3: output
  #Makes a list of the values combined with the original t
  result=list(time=t,TraitValue=traitValues)
  return(result)
} #The formula for Brownian motion/drift.

require(DAIME)
require(paleoTS)
require("ggplot2")
#You also need to load the function for the mode of evolution you want to test, in this case myBM.

#There are also two other .R files required: Multiplot.R and ADM Grapher.R:
source("code/ADM Grapher.R")
source("code/Multiplot.R")


p = "A"
{
  ############ 2. The forming of a full evolutionary record over time at all sampled locations.###########
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
    allTraitValues1=myBM(adjustAllTimes,5)
    allTraitValues2=myBM(adjustAllTimes,5)
    allTraitValues3=myBM(adjustAllTimes,5)
    allTraitValues4=myBM(adjustAllTimes,5)
    
    
    
    ######3. Making the trait values over time graph############
    #This makes the 4 graphs detectable by ggplot
    graphs=NA
    graphs[1:length(allTraitValues1$TraitValue)]=1
    graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues2$TraitValue))]=2
    graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues3$TraitValue))]=3
    graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues4$TraitValue))]=4
    #This puts all the values together for use in ggplot
    full=NA
    full=c(allTraitValues1$TraitValue,allTraitValues2$TraitValue,allTraitValues3$TraitValue,allTraitValues4$TraitValue)
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
      ggtitle("Trait values over time")+ #for the title
      xlab("Time (Myr)")+ # for the x axis label
      ylab("Trait values")+ # for the y axis label
      theme_bw()+ #Makes the background white.
      theme(text = element_text(size = 20), legend.position="none",plot.title = element_text(size=15)) #Changes text size 
    
    
    
    
    
  }
  
  ############## 3.The plotting of the evolution as found in a certain point ################
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
        traitValueUncalibrated1=append(traitValueUncalibrated1,allTraitValues1$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue1=traitValueUncalibrated1[-1]
      
      for (i in sameTime2){
        traitValueUncalibrated2=append(traitValueUncalibrated2,allTraitValues2$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue2=traitValueUncalibrated2[-1]
      
      for (i in sameTime3){
        traitValueUncalibrated3=append(traitValueUncalibrated3,allTraitValues3$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue3=traitValueUncalibrated3[-1]
      
      for (i in sameTime4){
        traitValueUncalibrated4=append(traitValueUncalibrated4,allTraitValues4$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue4=traitValueUncalibrated4[-1]
      
      
      #Calculating traitvalues over time and adding time and height to myTraitValues.
      myTraitValues1=c()#Creating the vector so that all other values get inputted correctly
      myTraitValues2=c()
      myTraitValues3=c()
      myTraitValues4=c()
      
      myTraitValues1$TraitValue=traitValue1 #Inputting Brownian drift
      myTraitValues2$TraitValue=traitValue2
      myTraitValues3$TraitValue=traitValue3 
      myTraitValues4$TraitValue=traitValue4 
      
      myTraitValues1$height=myHeightsOfObservations #Adding the respective heights to the list
      myTraitValues2$height=myHeightsOfObservations
      myTraitValues3$height=myHeightsOfObservations
      myTraitValues4$height=myHeightsOfObservations
      
      myTraitValues1$time=transVal$time #adding the respective times to the list
      myTraitValues2$time=transVal$time
      myTraitValues3$time=transVal$time
      myTraitValues4$time=transVal$time
      
      
      ######3. Making the trait values over time graph############
      #This makes the 4 graphs detectable by ggplot  
      graphs=NA
      graphs[1:length(myTraitValues1$TraitValue)]=1
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues2$TraitValue))]=2
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues3$TraitValue))]=3
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues4$TraitValue))]=4
      #This puts all the values together for use in ggplot
      full=NA
      full=c(myTraitValues1$TraitValue,myTraitValues2$TraitValue,myTraitValues3$TraitValue,myTraitValues4$TraitValue)
      #This makes sure the x values are coupled to the heights properly
      Timespan=NA
      Timespan=c(myTraitValues1$height,myTraitValues2$height,myTraitValues3$height,myTraitValues4$height)
      #This creates the labels for colours in the graph
      Label=c(rep("run 1",length(myTraitValues1$TraitValue)),rep("run 2",length(myTraitValues2$TraitValue)),rep("run 3",length(myTraitValues3$TraitValue)),rep("run 4",length(myTraitValues4$TraitValue)))
      #Creates a dataframe with all the earlier info
      df2=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
      return(df2)
    }
    
    
    #The plot for all the four lines, forming one graph.
    Plot1=ggplot(data = df2, aes(x=x, y=y,col=Distance))+
      geom_line(size=1)+
      ggtitle(paste("Distance From Shore: ", as.character(wantedDist/10), " km", sep=""))+ #for the title
      xlab("Height (m)")+ # for the x axis label
      ylab("Trait values")+ # for the y axis label
      theme_bw()+ #Makes the background white.
      theme(text = element_text(size = 20),legend.position="none",plot.title = element_text(size=15)) #Changes text size
  }
  
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
        traitValueUncalibrated1=append(traitValueUncalibrated1,allTraitValues1$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue1=traitValueUncalibrated1[-1]
      
      for (i in sameTime2){
        traitValueUncalibrated2=append(traitValueUncalibrated2,allTraitValues2$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue2=traitValueUncalibrated2[-1]
      
      for (i in sameTime3){
        traitValueUncalibrated3=append(traitValueUncalibrated3,allTraitValues3$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue3=traitValueUncalibrated3[-1]
      
      for (i in sameTime4){
        traitValueUncalibrated4=append(traitValueUncalibrated4,allTraitValues4$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue4=traitValueUncalibrated4[-1]
      
      
      #Calculating traitvalues over time and adding time and height to myTraitValues.
      myTraitValues1=c()#Creating the vector so that all other values get inputted correctly
      myTraitValues2=c()
      myTraitValues3=c()
      myTraitValues4=c()
      
      myTraitValues1$TraitValue=traitValue1 #Inputting Brownian drift
      myTraitValues2$TraitValue=traitValue2
      myTraitValues3$TraitValue=traitValue3 
      myTraitValues4$TraitValue=traitValue4 
      
      myTraitValues1$height=myHeightsOfObservations #Adding the respective heights to the list
      myTraitValues2$height=myHeightsOfObservations
      myTraitValues3$height=myHeightsOfObservations
      myTraitValues4$height=myHeightsOfObservations
      
      myTraitValues1$time=transVal$time #adding the respective times to the list
      myTraitValues2$time=transVal$time
      myTraitValues3$time=transVal$time
      myTraitValues4$time=transVal$time
      
      
      ######3. Making the trait values over time graph############
      #This makes the 4 graphs detectable by ggplot  
      graphs=NA
      graphs[1:length(myTraitValues1$TraitValue)]=1
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues2$TraitValue))]=2
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues3$TraitValue))]=3
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues4$TraitValue))]=4
      #This puts all the values together for use in ggplot
      full=NA
      full=c(myTraitValues1$TraitValue,myTraitValues2$TraitValue,myTraitValues3$TraitValue,myTraitValues4$TraitValue)
      #This makes sure the x values are coupled to the heights properly
      Timespan=NA
      Timespan=c(myTraitValues1$height,myTraitValues2$height,myTraitValues3$height,myTraitValues4$height)
      #This creates the labels for colours in the graph
      Label=c(rep("run 1",length(myTraitValues1$TraitValue)),rep("run 2",length(myTraitValues2$TraitValue)),rep("run 3",length(myTraitValues3$TraitValue)),rep("run 4",length(myTraitValues4$TraitValue)))
      #Creates a dataframe with all the earlier info
      df3=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
      return(df3)
    }
    
    
    #The plot for all the four lines, forming one graph.
    Plot2=ggplot(data = df3, aes(x=x, y=y,col=Distance))+
      geom_line(size=1)+
      ggtitle(paste("Distance From Shore: ", as.character(wantedDist/10), " km", sep=""))+ #for the title
      xlab("Height (m)")+ # for the x axis label
      ylab("Trait values")+ # for the y axis label
      theme_bw()+ #Makes the background white.
      theme(text = element_text(size = 20),legend.position="none",plot.title = element_text(size=15)) #Changes text size
  }
  
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
        traitValueUncalibrated1=append(traitValueUncalibrated1,allTraitValues1$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue1=traitValueUncalibrated1[-1]
      
      for (i in sameTime2){
        traitValueUncalibrated2=append(traitValueUncalibrated2,allTraitValues2$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue2=traitValueUncalibrated2[-1]
      
      for (i in sameTime3){
        traitValueUncalibrated3=append(traitValueUncalibrated3,allTraitValues3$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue3=traitValueUncalibrated3[-1]
      
      for (i in sameTime4){
        traitValueUncalibrated4=append(traitValueUncalibrated4,allTraitValues4$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue4=traitValueUncalibrated4[-1]
      
      
      #Calculating traitvalues over time and adding time and height to myTraitValues.
      myTraitValues1=c()#Creating the vector so that all other values get inputted correctly
      myTraitValues2=c()
      myTraitValues3=c()
      myTraitValues4=c()
      
      myTraitValues1$TraitValue=traitValue1 #Inputting Brownian drift
      myTraitValues2$TraitValue=traitValue2
      myTraitValues3$TraitValue=traitValue3 
      myTraitValues4$TraitValue=traitValue4 
      
      myTraitValues1$height=myHeightsOfObservations #Adding the respective heights to the list
      myTraitValues2$height=myHeightsOfObservations
      myTraitValues3$height=myHeightsOfObservations
      myTraitValues4$height=myHeightsOfObservations
      
      myTraitValues1$time=transVal$time #adding the respective times to the list
      myTraitValues2$time=transVal$time
      myTraitValues3$time=transVal$time
      myTraitValues4$time=transVal$time
      
      
      ######3. Making the trait values over time graph############
      #This makes the 4 graphs detectable by ggplot  
      graphs=NA
      graphs[1:length(myTraitValues1$TraitValue)]=1
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues2$TraitValue))]=2
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues3$TraitValue))]=3
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues4$TraitValue))]=4
      #This puts all the values together for use in ggplot
      full=NA
      full=c(myTraitValues1$TraitValue,myTraitValues2$TraitValue,myTraitValues3$TraitValue,myTraitValues4$TraitValue)
      #This makes sure the x values are coupled to the heights properly
      Timespan=NA
      Timespan=c(myTraitValues1$height,myTraitValues2$height,myTraitValues3$height,myTraitValues4$height)
      #This creates the labels for colours in the graph
      Label=c(rep("run 1",length(myTraitValues1$TraitValue)),rep("run 2",length(myTraitValues2$TraitValue)),rep("run 3",length(myTraitValues3$TraitValue)),rep("run 4",length(myTraitValues4$TraitValue)))
      #Creates a dataframe with all the earlier info
      df4=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
      return(df4)
    }
    
    
    #The plot for all the four lines, forming one graph.
    Plot3=ggplot(data = df4, aes(x=x, y=y,col=Distance))+
      geom_line(size=1)+
      ggtitle(paste("Distance From Shore: ", as.character(wantedDist/10), " km", sep=""))+ #for the title
      xlab("Height (m)")+ # for the x axis label
      ylab("Trait values")+ # for the y axis label
      theme_bw()+ #Makes the background white.
      theme(text = element_text(size = 20),legend.position="none",plot.title = element_text(size=15)) #Changes text size
  }
  
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
        traitValueUncalibrated1=append(traitValueUncalibrated1,allTraitValues1$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue1=traitValueUncalibrated1[-1]
      
      for (i in sameTime2){
        traitValueUncalibrated2=append(traitValueUncalibrated2,allTraitValues2$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue2=traitValueUncalibrated2[-1]
      
      for (i in sameTime3){
        traitValueUncalibrated3=append(traitValueUncalibrated3,allTraitValues3$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue3=traitValueUncalibrated3[-1]
      
      for (i in sameTime4){
        traitValueUncalibrated4=append(traitValueUncalibrated4,allTraitValues4$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue4=traitValueUncalibrated4[-1]
      
      
      #Calculating traitvalues over time and adding time and height to myTraitValues.
      myTraitValues1=c()#Creating the vector so that all other values get inputted correctly
      myTraitValues2=c()
      myTraitValues3=c()
      myTraitValues4=c()
      
      myTraitValues1$TraitValue=traitValue1 #Inputting Brownian drift
      myTraitValues2$TraitValue=traitValue2
      myTraitValues3$TraitValue=traitValue3 
      myTraitValues4$TraitValue=traitValue4 
      
      myTraitValues1$height=myHeightsOfObservations #Adding the respective heights to the list
      myTraitValues2$height=myHeightsOfObservations
      myTraitValues3$height=myHeightsOfObservations
      myTraitValues4$height=myHeightsOfObservations
      
      myTraitValues1$time=transVal$time #adding the respective times to the list
      myTraitValues2$time=transVal$time
      myTraitValues3$time=transVal$time
      myTraitValues4$time=transVal$time
      
      
      ######3. Making the trait values over time graph############
      #This makes the 4 graphs detectable by ggplot  
      graphs=NA
      graphs[1:length(myTraitValues1$TraitValue)]=1
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues2$TraitValue))]=2
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues3$TraitValue))]=3
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues4$TraitValue))]=4
      #This puts all the values together for use in ggplot
      full=NA
      full=c(myTraitValues1$TraitValue,myTraitValues2$TraitValue,myTraitValues3$TraitValue,myTraitValues4$TraitValue)
      #This makes sure the x values are coupled to the heights properly
      Timespan=NA
      Timespan=c(myTraitValues1$height,myTraitValues2$height,myTraitValues3$height,myTraitValues4$height)
      #This creates the labels for colours in the graph
      Label=c(rep("run 1",length(myTraitValues1$TraitValue)),rep("run 2",length(myTraitValues2$TraitValue)),rep("run 3",length(myTraitValues3$TraitValue)),rep("run 4",length(myTraitValues4$TraitValue)))
      #Creates a dataframe with all the earlier info
      df5=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
      return(df5)
    }
    
    
    #The plot for all the four lines, forming one graph.
    Plot4=ggplot(data = df5, aes(x=x, y=y,col=Distance))+
      geom_line(size=1)+
      ggtitle(paste("Distance From Shore: ", as.character(wantedDist/10), " km", sep=""))+ #for the title
      xlab("Height (m)")+ # for the x axis label
      ylab("Trait values")+ # for the y axis label
      theme_bw()+ #Makes the background white.
      theme(text = element_text(size = 20),legend.position="none",plot.title = element_text(size=15)) #Changes text size
  }
  
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
        traitValueUncalibrated1=append(traitValueUncalibrated1,allTraitValues1$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue1=traitValueUncalibrated1[-1]
      
      for (i in sameTime2){
        traitValueUncalibrated2=append(traitValueUncalibrated2,allTraitValues2$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue2=traitValueUncalibrated2[-1]
      
      for (i in sameTime3){
        traitValueUncalibrated3=append(traitValueUncalibrated3,allTraitValues3$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue3=traitValueUncalibrated3[-1]
      
      for (i in sameTime4){
        traitValueUncalibrated4=append(traitValueUncalibrated4,allTraitValues4$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue4=traitValueUncalibrated4[-1]
      
      
      #Calculating traitvalues over time and adding time and height to myTraitValues.
      myTraitValues1=c()#Creating the vector so that all other values get inputted correctly
      myTraitValues2=c()
      myTraitValues3=c()
      myTraitValues4=c()
      
      myTraitValues1$TraitValue=traitValue1 #Inputting Brownian drift
      myTraitValues2$TraitValue=traitValue2
      myTraitValues3$TraitValue=traitValue3 
      myTraitValues4$TraitValue=traitValue4 
      
      myTraitValues1$height=myHeightsOfObservations #Adding the respective heights to the list
      myTraitValues2$height=myHeightsOfObservations
      myTraitValues3$height=myHeightsOfObservations
      myTraitValues4$height=myHeightsOfObservations
      
      myTraitValues1$time=transVal$time #adding the respective times to the list
      myTraitValues2$time=transVal$time
      myTraitValues3$time=transVal$time
      myTraitValues4$time=transVal$time
      
      
      ######3. Making the trait values over time graph############
      #This makes the 4 graphs detectable by ggplot  
      graphs=NA
      graphs[1:length(myTraitValues1$TraitValue)]=1
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues2$TraitValue))]=2
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues3$TraitValue))]=3
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues4$TraitValue))]=4
      #This puts all the values together for use in ggplot
      full=NA
      full=c(myTraitValues1$TraitValue,myTraitValues2$TraitValue,myTraitValues3$TraitValue,myTraitValues4$TraitValue)
      #This makes sure the x values are coupled to the heights properly
      Timespan=NA
      Timespan=c(myTraitValues1$height,myTraitValues2$height,myTraitValues3$height,myTraitValues4$height)
      #This creates the labels for colours in the graph
      Label=c(rep("run 1",length(myTraitValues1$TraitValue)),rep("run 2",length(myTraitValues2$TraitValue)),rep("run 3",length(myTraitValues3$TraitValue)),rep("run 4",length(myTraitValues4$TraitValue)))
      #Creates a dataframe with all the earlier info
      df6=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
      return(df6)
    }
    
    
    #The plot for all the four lines, forming one graph.
    Plot5=ggplot(data = df6, aes(x=x, y=y,col=Distance))+
      geom_line(size=1)+
      ggtitle(paste("Distance From Shore: ", as.character(wantedDist/10), " km", sep=""))+ #for the title
      xlab("Height (m)")+ # for the x axis label
      ylab("Trait values")+ # for the y axis label
      theme_bw()+ #Makes the background white.
      theme(text = element_text(size = 20),legend.position="none",plot.title = element_text(size=15)) #Changes text size
  }
  {
    pdf(file = paste("figs/R/Multiplot_",p,".pdf"), width= 10, height= 12)
    
    multiplot(ADM_A,PlotTT,NA,Plot1,Plot2,Plot3,Plot4,Plot5,cols=3) #The multiplot
    dev.off()
  }
}

p = "B"
{
  ############ 2. The forming of a full evolutionary record over time at all sampled locations.###########
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
    allTraitValues1=myBM(adjustAllTimes,5)
    allTraitValues2=myBM(adjustAllTimes,5)
    allTraitValues3=myBM(adjustAllTimes,5)
    allTraitValues4=myBM(adjustAllTimes,5)
    
    
    
    ######3. Making the trait values over time graph############
    #This makes the 4 graphs detectable by ggplot
    graphs=NA
    graphs[1:length(allTraitValues1$TraitValue)]=1
    graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues2$TraitValue))]=2
    graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues3$TraitValue))]=3
    graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues4$TraitValue))]=4
    #This puts all the values together for use in ggplot
    full=NA
    full=c(allTraitValues1$TraitValue,allTraitValues2$TraitValue,allTraitValues3$TraitValue,allTraitValues4$TraitValue)
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
      ggtitle("Trait values over time")+ #for the title
      xlab("Time (Myr)")+ # for the x axis label
      ylab("Trait values")+ # for the y axis label
      theme_bw()+ #Makes the background white.
      theme(text = element_text(size = 20), legend.position="none",plot.title = element_text(size=15)) #Changes text size 
    
    
    
    
    
  }
  
  ############## 3.The plotting of the evolution as found in a certain point ################
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
        traitValueUncalibrated1=append(traitValueUncalibrated1,allTraitValues1$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue1=traitValueUncalibrated1[-1]
      
      for (i in sameTime2){
        traitValueUncalibrated2=append(traitValueUncalibrated2,allTraitValues2$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue2=traitValueUncalibrated2[-1]
      
      for (i in sameTime3){
        traitValueUncalibrated3=append(traitValueUncalibrated3,allTraitValues3$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue3=traitValueUncalibrated3[-1]
      
      for (i in sameTime4){
        traitValueUncalibrated4=append(traitValueUncalibrated4,allTraitValues4$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue4=traitValueUncalibrated4[-1]
      
      
      #Calculating traitvalues over time and adding time and height to myTraitValues.
      myTraitValues1=c()#Creating the vector so that all other values get inputted correctly
      myTraitValues2=c()
      myTraitValues3=c()
      myTraitValues4=c()
      
      myTraitValues1$TraitValue=traitValue1 #Inputting Brownian drift
      myTraitValues2$TraitValue=traitValue2
      myTraitValues3$TraitValue=traitValue3 
      myTraitValues4$TraitValue=traitValue4 
      
      myTraitValues1$height=myHeightsOfObservations #Adding the respective heights to the list
      myTraitValues2$height=myHeightsOfObservations
      myTraitValues3$height=myHeightsOfObservations
      myTraitValues4$height=myHeightsOfObservations
      
      myTraitValues1$time=transVal$time #adding the respective times to the list
      myTraitValues2$time=transVal$time
      myTraitValues3$time=transVal$time
      myTraitValues4$time=transVal$time
      
      
      ######3. Making the trait values over time graph############
      #This makes the 4 graphs detectable by ggplot  
      graphs=NA
      graphs[1:length(myTraitValues1$TraitValue)]=1
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues2$TraitValue))]=2
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues3$TraitValue))]=3
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues4$TraitValue))]=4
      #This puts all the values together for use in ggplot
      full=NA
      full=c(myTraitValues1$TraitValue,myTraitValues2$TraitValue,myTraitValues3$TraitValue,myTraitValues4$TraitValue)
      #This makes sure the x values are coupled to the heights properly
      Timespan=NA
      Timespan=c(myTraitValues1$height,myTraitValues2$height,myTraitValues3$height,myTraitValues4$height)
      #This creates the labels for colours in the graph
      Label=c(rep("run 1",length(myTraitValues1$TraitValue)),rep("run 2",length(myTraitValues2$TraitValue)),rep("run 3",length(myTraitValues3$TraitValue)),rep("run 4",length(myTraitValues4$TraitValue)))
      #Creates a dataframe with all the earlier info
      df2=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
      return(df2)
    }
    
    
    #The plot for all the four lines, forming one graph.
    Plot1=ggplot(data = df2, aes(x=x, y=y,col=Distance))+
      geom_line(size=1)+
      ggtitle(paste("Distance From Shore: ", as.character(wantedDist/10), " km", sep=""))+ #for the title
      xlab("Height (m)")+ # for the x axis label
      ylab("Trait values")+ # for the y axis label
      theme_bw()+ #Makes the background white.
      theme(text = element_text(size = 20),legend.position="none",plot.title = element_text(size=15)) #Changes text size
  }
  
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
        traitValueUncalibrated1=append(traitValueUncalibrated1,allTraitValues1$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue1=traitValueUncalibrated1[-1]
      
      for (i in sameTime2){
        traitValueUncalibrated2=append(traitValueUncalibrated2,allTraitValues2$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue2=traitValueUncalibrated2[-1]
      
      for (i in sameTime3){
        traitValueUncalibrated3=append(traitValueUncalibrated3,allTraitValues3$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue3=traitValueUncalibrated3[-1]
      
      for (i in sameTime4){
        traitValueUncalibrated4=append(traitValueUncalibrated4,allTraitValues4$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue4=traitValueUncalibrated4[-1]
      
      
      #Calculating traitvalues over time and adding time and height to myTraitValues.
      myTraitValues1=c()#Creating the vector so that all other values get inputted correctly
      myTraitValues2=c()
      myTraitValues3=c()
      myTraitValues4=c()
      
      myTraitValues1$TraitValue=traitValue1 #Inputting Brownian drift
      myTraitValues2$TraitValue=traitValue2
      myTraitValues3$TraitValue=traitValue3 
      myTraitValues4$TraitValue=traitValue4 
      
      myTraitValues1$height=myHeightsOfObservations #Adding the respective heights to the list
      myTraitValues2$height=myHeightsOfObservations
      myTraitValues3$height=myHeightsOfObservations
      myTraitValues4$height=myHeightsOfObservations
      
      myTraitValues1$time=transVal$time #adding the respective times to the list
      myTraitValues2$time=transVal$time
      myTraitValues3$time=transVal$time
      myTraitValues4$time=transVal$time
      
      
      ######3. Making the trait values over time graph############
      #This makes the 4 graphs detectable by ggplot  
      graphs=NA
      graphs[1:length(myTraitValues1$TraitValue)]=1
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues2$TraitValue))]=2
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues3$TraitValue))]=3
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues4$TraitValue))]=4
      #This puts all the values together for use in ggplot
      full=NA
      full=c(myTraitValues1$TraitValue,myTraitValues2$TraitValue,myTraitValues3$TraitValue,myTraitValues4$TraitValue)
      #This makes sure the x values are coupled to the heights properly
      Timespan=NA
      Timespan=c(myTraitValues1$height,myTraitValues2$height,myTraitValues3$height,myTraitValues4$height)
      #This creates the labels for colours in the graph
      Label=c(rep("run 1",length(myTraitValues1$TraitValue)),rep("run 2",length(myTraitValues2$TraitValue)),rep("run 3",length(myTraitValues3$TraitValue)),rep("run 4",length(myTraitValues4$TraitValue)))
      #Creates a dataframe with all the earlier info
      df3=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
      return(df3)
    }
    
    
    #The plot for all the four lines, forming one graph.
    Plot2=ggplot(data = df3, aes(x=x, y=y,col=Distance))+
      geom_line(size=1)+
      ggtitle(paste("Distance From Shore: ", as.character(wantedDist/10), " km", sep=""))+ #for the title
      xlab("Height (m)")+ # for the x axis label
      ylab("Trait values")+ # for the y axis label
      theme_bw()+ #Makes the background white.
      theme(text = element_text(size = 20),legend.position="none",plot.title = element_text(size=15)) #Changes text size
  }
  
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
        traitValueUncalibrated1=append(traitValueUncalibrated1,allTraitValues1$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue1=traitValueUncalibrated1[-1]
      
      for (i in sameTime2){
        traitValueUncalibrated2=append(traitValueUncalibrated2,allTraitValues2$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue2=traitValueUncalibrated2[-1]
      
      for (i in sameTime3){
        traitValueUncalibrated3=append(traitValueUncalibrated3,allTraitValues3$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue3=traitValueUncalibrated3[-1]
      
      for (i in sameTime4){
        traitValueUncalibrated4=append(traitValueUncalibrated4,allTraitValues4$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue4=traitValueUncalibrated4[-1]
      
      
      #Calculating traitvalues over time and adding time and height to myTraitValues.
      myTraitValues1=c()#Creating the vector so that all other values get inputted correctly
      myTraitValues2=c()
      myTraitValues3=c()
      myTraitValues4=c()
      
      myTraitValues1$TraitValue=traitValue1 #Inputting Brownian drift
      myTraitValues2$TraitValue=traitValue2
      myTraitValues3$TraitValue=traitValue3 
      myTraitValues4$TraitValue=traitValue4 
      
      myTraitValues1$height=myHeightsOfObservations #Adding the respective heights to the list
      myTraitValues2$height=myHeightsOfObservations
      myTraitValues3$height=myHeightsOfObservations
      myTraitValues4$height=myHeightsOfObservations
      
      myTraitValues1$time=transVal$time #adding the respective times to the list
      myTraitValues2$time=transVal$time
      myTraitValues3$time=transVal$time
      myTraitValues4$time=transVal$time
      
      
      ######3. Making the trait values over time graph############
      #This makes the 4 graphs detectable by ggplot  
      graphs=NA
      graphs[1:length(myTraitValues1$TraitValue)]=1
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues2$TraitValue))]=2
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues3$TraitValue))]=3
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues4$TraitValue))]=4
      #This puts all the values together for use in ggplot
      full=NA
      full=c(myTraitValues1$TraitValue,myTraitValues2$TraitValue,myTraitValues3$TraitValue,myTraitValues4$TraitValue)
      #This makes sure the x values are coupled to the heights properly
      Timespan=NA
      Timespan=c(myTraitValues1$height,myTraitValues2$height,myTraitValues3$height,myTraitValues4$height)
      #This creates the labels for colours in the graph
      Label=c(rep("run 1",length(myTraitValues1$TraitValue)),rep("run 2",length(myTraitValues2$TraitValue)),rep("run 3",length(myTraitValues3$TraitValue)),rep("run 4",length(myTraitValues4$TraitValue)))
      #Creates a dataframe with all the earlier info
      df4=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
      return(df4)
    }
    
    
    #The plot for all the four lines, forming one graph.
    Plot3=ggplot(data = df4, aes(x=x, y=y,col=Distance))+
      geom_line(size=1)+
      ggtitle(paste("Distance From Shore: ", as.character(wantedDist/10), " km", sep=""))+ #for the title
      xlab("Height (m)")+ # for the x axis label
      ylab("Trait values")+ # for the y axis label
      theme_bw()+ #Makes the background white.
      theme(text = element_text(size = 20),legend.position="none",plot.title = element_text(size=15)) #Changes text size
  }
  
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
        traitValueUncalibrated1=append(traitValueUncalibrated1,allTraitValues1$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue1=traitValueUncalibrated1[-1]
      
      for (i in sameTime2){
        traitValueUncalibrated2=append(traitValueUncalibrated2,allTraitValues2$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue2=traitValueUncalibrated2[-1]
      
      for (i in sameTime3){
        traitValueUncalibrated3=append(traitValueUncalibrated3,allTraitValues3$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue3=traitValueUncalibrated3[-1]
      
      for (i in sameTime4){
        traitValueUncalibrated4=append(traitValueUncalibrated4,allTraitValues4$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue4=traitValueUncalibrated4[-1]
      
      
      #Calculating traitvalues over time and adding time and height to myTraitValues.
      myTraitValues1=c()#Creating the vector so that all other values get inputted correctly
      myTraitValues2=c()
      myTraitValues3=c()
      myTraitValues4=c()
      
      myTraitValues1$TraitValue=traitValue1 #Inputting Brownian drift
      myTraitValues2$TraitValue=traitValue2
      myTraitValues3$TraitValue=traitValue3 
      myTraitValues4$TraitValue=traitValue4 
      
      myTraitValues1$height=myHeightsOfObservations #Adding the respective heights to the list
      myTraitValues2$height=myHeightsOfObservations
      myTraitValues3$height=myHeightsOfObservations
      myTraitValues4$height=myHeightsOfObservations
      
      myTraitValues1$time=transVal$time #adding the respective times to the list
      myTraitValues2$time=transVal$time
      myTraitValues3$time=transVal$time
      myTraitValues4$time=transVal$time
      
      
      ######3. Making the trait values over time graph############
      #This makes the 4 graphs detectable by ggplot  
      graphs=NA
      graphs[1:length(myTraitValues1$TraitValue)]=1
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues2$TraitValue))]=2
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues3$TraitValue))]=3
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues4$TraitValue))]=4
      #This puts all the values together for use in ggplot
      full=NA
      full=c(myTraitValues1$TraitValue,myTraitValues2$TraitValue,myTraitValues3$TraitValue,myTraitValues4$TraitValue)
      #This makes sure the x values are coupled to the heights properly
      Timespan=NA
      Timespan=c(myTraitValues1$height,myTraitValues2$height,myTraitValues3$height,myTraitValues4$height)
      #This creates the labels for colours in the graph
      Label=c(rep("run 1",length(myTraitValues1$TraitValue)),rep("run 2",length(myTraitValues2$TraitValue)),rep("run 3",length(myTraitValues3$TraitValue)),rep("run 4",length(myTraitValues4$TraitValue)))
      #Creates a dataframe with all the earlier info
      df5=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
      return(df5)
    }
    
    
    #The plot for all the four lines, forming one graph.
    Plot4=ggplot(data = df5, aes(x=x, y=y,col=Distance))+
      geom_line(size=1)+
      ggtitle(paste("Distance From Shore: ", as.character(wantedDist/10), " km", sep=""))+ #for the title
      xlab("Height (m)")+ # for the x axis label
      ylab("Trait values")+ # for the y axis label
      theme_bw()+ #Makes the background white.
      theme(text = element_text(size = 20),legend.position="none",plot.title = element_text(size=15)) #Changes text size
  }
  
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
        traitValueUncalibrated1=append(traitValueUncalibrated1,allTraitValues1$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue1=traitValueUncalibrated1[-1]
      
      for (i in sameTime2){
        traitValueUncalibrated2=append(traitValueUncalibrated2,allTraitValues2$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue2=traitValueUncalibrated2[-1]
      
      for (i in sameTime3){
        traitValueUncalibrated3=append(traitValueUncalibrated3,allTraitValues3$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue3=traitValueUncalibrated3[-1]
      
      for (i in sameTime4){
        traitValueUncalibrated4=append(traitValueUncalibrated4,allTraitValues4$TraitValue[i])
      }
      #Removing the first value which was only there to allow the input in the for-loop.
      traitValue4=traitValueUncalibrated4[-1]
      
      
      #Calculating traitvalues over time and adding time and height to myTraitValues.
      myTraitValues1=c()#Creating the vector so that all other values get inputted correctly
      myTraitValues2=c()
      myTraitValues3=c()
      myTraitValues4=c()
      
      myTraitValues1$TraitValue=traitValue1 #Inputting Brownian drift
      myTraitValues2$TraitValue=traitValue2
      myTraitValues3$TraitValue=traitValue3 
      myTraitValues4$TraitValue=traitValue4 
      
      myTraitValues1$height=myHeightsOfObservations #Adding the respective heights to the list
      myTraitValues2$height=myHeightsOfObservations
      myTraitValues3$height=myHeightsOfObservations
      myTraitValues4$height=myHeightsOfObservations
      
      myTraitValues1$time=transVal$time #adding the respective times to the list
      myTraitValues2$time=transVal$time
      myTraitValues3$time=transVal$time
      myTraitValues4$time=transVal$time
      
      
      ######3. Making the trait values over time graph############
      #This makes the 4 graphs detectable by ggplot  
      graphs=NA
      graphs[1:length(myTraitValues1$TraitValue)]=1
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues2$TraitValue))]=2
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues3$TraitValue))]=3
      graphs[(length(graphs)+1):(length(graphs)+length(myTraitValues4$TraitValue))]=4
      #This puts all the values together for use in ggplot
      full=NA
      full=c(myTraitValues1$TraitValue,myTraitValues2$TraitValue,myTraitValues3$TraitValue,myTraitValues4$TraitValue)
      #This makes sure the x values are coupled to the heights properly
      Timespan=NA
      Timespan=c(myTraitValues1$height,myTraitValues2$height,myTraitValues3$height,myTraitValues4$height)
      #This creates the labels for colours in the graph
      Label=c(rep("run 1",length(myTraitValues1$TraitValue)),rep("run 2",length(myTraitValues2$TraitValue)),rep("run 3",length(myTraitValues3$TraitValue)),rep("run 4",length(myTraitValues4$TraitValue)))
      #Creates a dataframe with all the earlier info
      df6=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
      return(df6)
    }
    
    
    #The plot for all the four lines, forming one graph.
    Plot5=ggplot(data = df6, aes(x=x, y=y,col=Distance))+
      geom_line(size=1)+
      ggtitle(paste("Distance From Shore: ", as.character(wantedDist/10), " km", sep=""))+ #for the title
      xlab("Height (m)")+ # for the x axis label
      ylab("Trait values")+ # for the y axis label
      theme_bw()+ #Makes the background white.
      theme(text = element_text(size = 20),legend.position="none",plot.title = element_text(size=15)) #Changes text size
  }
  {
    pdf(file = paste("figs/R/Multiplot_",p,".pdf"), width= 10, height= 12)
    
    multiplot(ADM_B,PlotTT,NA,Plot1,Plot2,Plot3,Plot4,Plot5,cols=3) #The multiplot
    dev.off()
  }
}
