#Creates the ADM graphs for both platforms


load("NewAgeModels.Rdata") #loads platform B
load("syntheticAgeModels.RData") #Loads platform A

load("scenariosAandB.Rdata")
ageModelList=scenarioA

#dependencies:
require("ggrepel")   
require(DAIME)
require("ggplot2")
require("RColorBrewer")

#### plot age model with erosion ####
# choose age model
i=20
{
  AMTime=ageModelList[[i]]$time # extract time
  
  AMHeight=ageModelList[[i]]$height # extract strat height
  
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
HeightI=AMHiatusesRemoved$height
i=60
{
  AMTime=ageModelList[[i]]$time # extract time
  
  AMHeight=ageModelList[[i]]$height # extract strat height
  
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
  AMTime=ageModelList[[i]]$time # extract time
  
  AMHeight=ageModelList[[i]]$height # extract strat height
  
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
  AMTime=ageModelList[[i]]$time # extract time
  
  AMHeight=ageModelList[[i]]$height # extract strat height
  
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
AMTime=ageModelList[[i]]$time # extract time

AMHeight=ageModelList[[i]]$height # extract strat height

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
ADM=ggplot(data = df, aes(x=x, y=y,col=Distance))+
  geom_path(size=1)+
  ggtitle("Time found in platform A")+ #for the title
  xlab("Time (Myr)")+ # for the x axis label
  ylab("Stratigraphic Height (m)")+ # for the y axis label
  theme_bw()+ #Makes the background white.
  scale_colour_discrete(breaks=c( "2 km", '6 km', '8 km', "10 km", "12 km"))+
  theme(text = element_text(size = 20),#Changes text size
        plot.title = element_text(size=15),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = c(0.005, .99),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6),
        legend.box.background = element_rect(color="black", size=1)
          ) 
ADM





