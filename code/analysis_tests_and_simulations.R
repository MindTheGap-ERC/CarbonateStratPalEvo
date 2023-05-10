#### load required packages ####
# install.packages(ggtern)
require(DAIME)
require(paleoTS)
require(grid)
require("ggplot2")
require("RColorBrewer")
require("ggrepel")  
require("gridExtra")

source("code/multiplot.R")

#### load test results & platform data####
load("data/R_outputs/results_modes_of_evolution.Rdata")
load("data/R_outputs/age_depth_models.Rdata")


#### Basic Visuals: plot results in strat domain ####
dist <- "12 km" # one of "2 km","6 km","8 km","10 km","12 km"
scenario <- "B" # one of A","B"
mode <- "strong Brownian drift" # one of "stasis","Brownian motion","weak Brownian drift","strong Brownian drift"
run <- 1 # integer between 1 and noOfTests
## Age-depth model
{
  plot(
    x = testResultsStrat[[scenario]][[dist]][[mode]][[run]]$inputData$time,
    y = testResultsStrat[[scenario]][[dist]][[mode]][[run]]$inputData$height,
    type = "l",
    xlab = "Time [Ma]",
    ylab = "Height [m]",
    main = paste("Age-depth model, scenario ", scenario, ", ", dist, " offshore", sep = "")
  )
}
## Trait values in strat domain, standard visualisation
{
  plot(
    x = testResultsStrat[[scenario]][[dist]][[mode]][[run]]$inputData$height,
    y = testResultsStrat[[scenario]][[dist]][[mode]][[run]]$inputData$traitValue,
    type = "l",
    xlab = "Height [m]",
    ylab = "Trait Value",
    main = paste("Trait values in stratigraphic domain, scenario ", scenario, ", ", dist, " offshore \n", mode, ", run no. ", run, sep = "")
  )
  # add AICweights
  legend(
    "topleft",
    legend = c(
      "AICweights",
      paste("Stasis: ", as.character(testResultsStrat[[scenario]][[dist]][[mode]][[run]]$testRes$modelFits["Stasis", "Akaike.wt"]), sep = ""),
      paste("URW: ", as.character(testResultsStrat[[scenario]][[dist]][[mode]][[run]]$testRes$modelFits["URW", "Akaike.wt"]), sep = ""),
      paste("GRW: ", as.character(testResultsStrat[[scenario]][[dist]][[mode]][[run]]$testRes$modelFits["GRW", "Akaike.wt"]), sep = ""),
      paste("OU: ", as.character(testResultsStrat[[scenario]][[dist]][[mode]][[run]]$testRes$modelFits["OU", "Akaike.wt"]), sep = "")
    )
  )
}

## Trait values in strat, paleoTS visualisation
# note that the axis labels are hardcoded in the paleoTS package, and can not be changed via the "xlab"/"ylab" option
{
  plot(
    testResultsStrat[[scenario]][[dist]][[mode]][[run]]$paleots,
    main = paste("Trait values in stratigraphic domain, scenario ", scenario, ", ", dist, " offshore \n", mode, ", run no. ", run, sep = "")
  )
  # manually add axis labels
  mtext(
    text = "Height [m]",
    side = 1,
    line = 2,
    cex = 1.4
  )
  mtext(
    text = "Trait Value",
    side = 2,
    line = 2,
    cex = 1.4
  )
  # add legend
  legend(
    "topleft",
    legend = c(
      "AICweights",
      paste("Stasis: ", as.character(testResultsStrat[[scenario]][[dist]][[mode]][[run]]$testRes$modelFits["Stasis", "Akaike.wt"]), sep = ""),
      paste("URW: ", as.character(testResultsStrat[[scenario]][[dist]][[mode]][[run]]$testRes$modelFits["URW", "Akaike.wt"]), sep = ""),
      paste("GRW: ", as.character(testResultsStrat[[scenario]][[dist]][[mode]][[run]]$testRes$modelFits["GRW", "Akaike.wt"]), sep = ""),
      paste("OU: ", as.character(testResultsStrat[[scenario]][[dist]][[mode]][[run]]$testRes$modelFits["OU", "Akaike.wt"]), sep = "")
    )
  )
}

# trait values in time domain
{
  plot(
    x = testResultsStrat[[scenario]][[dist]][[mode]][[run]]$inputData$time,
    y = testResultsStrat[[scenario]][[dist]][[mode]][[run]]$inputData$traitValue,
    type = "l",
    xlab = "Time [Ma]",
    ylab = "Trait Value",
    main = paste("Trait values in time domain, scenario ", scenario, "\n", mode, ", run no. ", run, sep = "")
  )
}
#### Basic Visuals: Plot Results in Time Domain ####
scenario <- "B" # one of A","B"
nSamp <- "50" # one of "5"   "10"  "15"  "20"  "25"  "35"  "50"  "100" "200"
mode <- "stasis" # one of "stasis","Brownian motion","weak Brownian drift","strong Brownian drift"
run <- 1 # integer between 1 and noOfTests

# trait values in time domain, classic representation
{
  plot(
    x = testResultsTime[[scenario]][[nSamp]][[mode]][[run]]$inputData$time,
    y = testResultsTime[[scenario]][[nSamp]][[mode]][[run]]$inputData$traitValue,
    type = "l",
    xlab = "Time [Ma]",
    ylab = "Trait Value",
    main = paste("Trait values in time domain \n", nSamp, " samples, equidistantly distributed over ", maxTimes[[scenario]], " Ma", "\n", mode, ", run no. ", run, sep = "")
  )
  # add legend
  legend(
    "topleft",
    legend = c(
      "AICweights",
      paste("Stasis: ", as.character(testResultsTime[[scenario]][[nSamp]][[mode]][[run]]$testRes$modelFits["Stasis", "Akaike.wt"]), sep = ""),
      paste("URW: ", as.character(testResultsTime[[scenario]][[nSamp]][[mode]][[run]]$testRes$modelFits["URW", "Akaike.wt"]), sep = ""),
      paste("GRW: ", as.character(testResultsTime[[scenario]][[nSamp]][[mode]][[run]]$testRes$modelFits["GRW", "Akaike.wt"]), sep = ""),
      paste("OU: ", as.character(testResultsTime[[scenario]][[nSamp]][[mode]][[run]]$testRes$modelFits["OU", "Akaike.wt"]), sep = "")
    )
  )
}
# trait values in time domain, paleoTS representation
# note that the axis labels are hardcoded in the paleoTS package, and can not be changed via the "xlab"/"ylab" option
{
  plot(
    x = testResultsTime[[scenario]][[nSamp]][[mode]][[run]]$paleots,
    main = paste("Trait values in time domain \n", nSamp, " samples, equidistantly distributed over ", maxTimes[[scenario]], " Ma", "\n", mode, ", run no. ", run, sep = "")
  )
  # manually add axis labels
  mtext(
    text = "Time [Ma]",
    side = 1,
    line = 2,
    cex = 1.4
  )
  mtext(
    text = "Trait Value",
    side = 2,
    line = 2,
    cex = 1.4
  )
  legend(
    "topleft",
    legend = c(
      "AICweights",
      paste("Stasis: ", as.character(testResultsTime[[scenario]][[nSamp]][[mode]][[run]]$testRes$modelFits["Stasis", "Akaike.wt"]), sep = ""),
      paste("URW: ", as.character(testResultsTime[[scenario]][[nSamp]][[mode]][[run]]$testRes$modelFits["URW", "Akaike.wt"]), sep = ""),
      paste("GRW: ", as.character(testResultsTime[[scenario]][[nSamp]][[mode]][[run]]$testRes$modelFits["GRW", "Akaike.wt"]), sep = ""),
      paste("OU: ", as.character(testResultsTime[[scenario]][[nSamp]][[mode]][[run]]$testRes$modelFits["OU", "Akaike.wt"]), sep = "")
    )
  )
}

#### Extract Akaike Weights from Results ####
ExaminedBasinPositions= c("2","6","8","10","12")

AkaikeWtArrayStrat <- array(
  data = NA,
  dim = c(length(scenarioNames), length(examinedBasinPositions), length(simulatedEvoModes), length(testedEvoModes), noOfTests),
  dimnames = list(
    scenarioNames,
    examinedBasinPositions,
    simulatedEvoModes,
    testedEvoModes,
    NULL
  )
)
for (scenario in scenarioNames) {
  for (dist in examinedBasinPositions) {
    for (trueMode in simulatedEvoModes) {
      for (resMode in testedEvoModes) {
        for (i in 1:noOfTests) {
          AkaikeWtArrayStrat[scenario, dist, trueMode, resMode, i] <- testResultsStrat[[scenario]][[dist]][[trueMode]][[i]]$testRes$modelFits[resMode, "Akaike.wt"]
        }
      }
    }
  }
}

AkaikeWtArrayTime <- array(
  data = NA,
  dim = c(length(scenarioNames), length(noOfSamplingLoc), length(simulatedEvoModes), length(testedEvoModes), noOfTests),
  dimnames = list(
    scenarioNames,
    noOfSamplingLoc,
    simulatedEvoModes,
    testedEvoModes,
    NULL
  )
)
for (scenario in scenarioNames) {
  for (nSamp in noOfSamplingLoc) {
    for (trueMode in simulatedEvoModes) {
      for (resMode in testedEvoModes) {
        for (i in 1:noOfTests) {
          AkaikeWtArrayTime[scenario, nSamp, trueMode, resMode, i] <- testResultsTime[[scenario]][[nSamp]][[trueMode]][[i]]$testRes$modelFits[resMode, "Akaike.wt"]
        }
      }
    }
  }
}

#### Strat Domain: WHich models have strong support? ####
# checks for which proportion of test runs there is strong support for a model, meaning AICweight > acceptanceTreshold
acceptanceTreshold <- 0.9
strongSupportPropStrat <- array(
  data = NA,
  dim = c(length(scenarioNames), length(examinedBasinPositions), length(simulatedEvoModes), length(testedEvoModes)),
  dimnames = list(
    scenarioNames,
    examinedBasinPositions,
    simulatedEvoModes,
    testedEvoModes
  )
)

for (scenario in scenarioNames) {
  for (dist in examinedBasinPositions) {
    for (trueMode in simulatedEvoModes) {
      for (resMode in testedEvoModes) {
        strongSupportPropStrat[scenario, dist, trueMode, resMode] <- sum(AkaikeWtArrayStrat[scenario, dist, trueMode, resMode, ] > acceptanceTreshold) / noOfTests
      }
    }
  }
}

## Which prop of runs provide strong support for the correct mode of evolution?
strongSupportPropStrat[, , "stasis", "Stasis"]
strongSupportPropStrat[, , "Brownian motion", "URW"]
strongSupportPropStrat[, , "weak Brownian drift", "GRW"]
strongSupportPropStrat[, , "strong Brownian drift", "GRW"]
# answer: None

# in scenario X, what is the proportion of support for mode Y, given Z was simulated?
for (scenario in scenarioNames) {
  for (trueMode in simulatedEvoModes) {
    print(paste("In scenario ", scenario, " with simulated ", trueMode, ", the following modes are strongly supported (in proportion of test runs):", sep = ""))
    print(strongSupportPropStrat[scenario, , trueMode, ])
  }
}

#### Strat Domain: Which models have weak support? ####
# check prop of models with weak support, meaning AICweight<ignoranceTreshold so they can be ommited on the ternary diagrams
ignoranceTreshold <- 0.001
weakSupportPropStrat <- array(
  data = NA,
  dim = c(length(scenarioNames), length(examinedBasinPositions), length(simulatedEvoModes), length(testedEvoModes)),
  dimnames = list(
    scenarioNames,
    examinedBasinPositions,
    simulatedEvoModes,
    testedEvoModes
  )
)
for (scenario in scenarioNames) {
  for (dist in examinedBasinPositions) {
    for (trueMode in simulatedEvoModes) {
      for (resMode in testedEvoModes) {
        weakSupportPropStrat[scenario, dist, trueMode, resMode] <- sum(AkaikeWtArrayStrat[scenario, dist, trueMode, resMode, ] < ignoranceTreshold) / noOfTests
      }
    }
  }
}
weakSupportPropStrat["A", , "stasis", ]
weakSupportPropStrat["B", , "stasis", ]
# For Stasis, the GRW axis can be ignored except for scenario A, 12 km offshore

weakSupportPropStrat["A", , "Brownian motion", ]
weakSupportPropStrat["B", , "Brownian motion", ]
# For BM, the stasis axis can be ignored except for scenario A, 12 km offshore

weakSupportPropStrat["A", , "weak Brownian drift", ]
weakSupportPropStrat["B", , "weak Brownian drift", ]
# For weak Brownian drift, the stasis axis can be ignored

weakSupportPropStrat["A", , "strong Brownian drift", ]
weakSupportPropStrat["B", , "strong Brownian drift", ]
# For strong Brownian drift, the stasis axis can be ignored



#### Time Domain: Which models have strong support? ####
# checks for which proportion of test runs there is strong support for a model, meaning AICweight > acceptanceTreshold
acceptanceTreshold <- 0.9
strongSupportPropTime <- array(NA,
  dim = c(length(scenarioNames), length(noOfSamplingLoc), length(simulatedEvoModes), length(testedEvoModes)),
  dimnames = list(
    scenarioNames,
    noOfSamplingLoc,
    simulatedEvoModes,
    testedEvoModes
  )
)

for (scenario in scenarioNames) {
  for (nSamp in noOfSamplingLoc) {
    for (trueMode in simulatedEvoModes) {
      for (resMode in testedEvoModes) {
        strongSupportPropTime[scenario, nSamp, trueMode, resMode] <- sum(AkaikeWtArrayTime[scenario, nSamp, trueMode, resMode, ] > acceptanceTreshold) / noOfTests
      }
    }
  }
}

## Which prop of runs provide strong support for the correct mode of evolution (in dependence of no of sampling locations)
strongSupportPropTime[, , "stasis", "Stasis"]
strongSupportPropTime[, , "Brownian motion", "URW"]
strongSupportPropTime[, , "weak Brownian drift", "GRW"]
strongSupportPropTime[, , "strong Brownian drift", "GRW"]


# in scenario X, what is the proportion of support for mode Y, given Z was simulated?
for (scenario in scenarioNames) {
  for (trueMode in simulatedEvoModes) {
    print(paste("In scenario ", scenario, " with simulated ", trueMode, ", the following modes are strongly supported (in proportion of test runs):", sep = ""))
    print(strongSupportPropTime[scenario, , trueMode, ])
  }
}


#### Time Domain: Which models have weak support? ####
# check prop of models with weak support, meaning AICweight<ignoranceTreshold so they can be ommited on the ternary diagrams
ignoranceTreshold <- 0.01
weakSupportPropTime <- array(NA,
  dim = c(length(scenarioNames), length(noOfSamplingLoc), length(simulatedEvoModes), length(testedEvoModes)),
  dimnames = list(
    scenarioNames,
    noOfSamplingLoc,
    simulatedEvoModes,
    testedEvoModes
  )
)

for (scenario in scenarioNames) {
  for (nSamp in noOfSamplingLoc) {
    for (trueMode in simulatedEvoModes) {
      for (resMode in testedEvoModes) {
        weakSupportPropTime[scenario, nSamp, trueMode, resMode] <- sum(AkaikeWtArrayTime[scenario, nSamp, trueMode, resMode, ] < ignoranceTreshold) / noOfTests
      }
    }
  }
}

weakSupportPropTime["A", , "stasis", ]
weakSupportPropTime["B", , "stasis", ]

weakSupportPropTime["A", , "Brownian motion", ]
weakSupportPropTime["B", , "Brownian motion", ]

weakSupportPropTime["A", , "weak Brownian drift", ]
weakSupportPropTime["B", , "weak Brownian drift", ]


weakSupportPropTime["A", , "strong Brownian drift", ]
weakSupportPropTime["B", , "strong Brownian drift", ]


#### Grouped Boxplots of AICweights ####
simulatedEvoMode <- simulatedEvoModes[1]
scenario <- scenarioNames[1]
dirName <- "figs/R/stratDomainBoxPlots"
dir.create(dirName)
for (scenario in scenarioNames) {
  for (simulatedEvoMode in simulatedEvoModes) {
    df <- data.frame(
      position = NULL,
      testedMode = NULL,
      AIC = NULL
    )
    for (pos in examinedBasinPositions) {
      for (testedEvoMode in testedEvoModes) {
        df <- rbind(
          df,
          data.frame(
            position = as.factor(rep(x = pos, length.out = length(AkaikeWtArrayStrat[1, 1, 1, 1, ]))),
            testedEvoMode = rep(x = testedEvoMode, length.out = length(AkaikeWtArrayStrat[1, 1, 1, 1, ])),
            AIC = AkaikeWtArrayStrat[scenario, pos, simulatedEvoMode, testedEvoMode, ]
          )
        )
      }
    }
    p <- ggplot2::ggplot(
      df,
      aes(
        x = position,
        y = AIC,
        fill = testedEvoMode
      )
    ) +
      geom_boxplot(outlier.shape = NA,lwd=0.1) +
      labs(
        title = paste("Scenario ", scenario, ", simulated: ", simulatedEvoMode, sep = ""),
        y = "AIC weight"
      )

    fileName <- paste("StratDomainScenario", scenario, "simulated", simulatedEvoMode, ".png")
    ggsave(
      filename = paste(dirName, "/", fileName, sep = "")
    )
  }
}
dirName <- "figs/R/timeDomainBoxPlots"
dir.create(dirName)
for (scenario in scenarioNames) {
  for (simulatedEvoMode in simulatedEvoModes) {
    df <- data.frame(
      nsp = NULL,
      testedMode = NULL,
      AIC = NULL
    )
    for (nsp in noOfSamplingLoc) {
      for (testedEvoMode in testedEvoModes) {
        df <- rbind(
          df,
          data.frame(
            nsp = as.factor(rep(x = nsp, length.out = length(AkaikeWtArrayTime[1, 1, 1, 1, ]))),
            testedEvoMode = rep(x = testedEvoMode, length.out = length(AkaikeWtArrayTime[1, 1, 1, 1, ])),
            AIC = AkaikeWtArrayTime[scenario, nsp, simulatedEvoMode, testedEvoMode, ]
          )
        )
      }
    }
    p <- ggplot2::ggplot(
      df,
      aes(
        x = nsp,
        y = AIC,
        fill = testedEvoMode
      )
    ) +
      geom_boxplot(outlier.shape = NA,lwd=0.1) +
      labs(
        title = paste("Scenario ", scenario, ", simulated: ", simulatedEvoMode, sep = ""),
        y = "AIC weight"
      )
    fileName <- paste("TimeDomainScenario", scenario, "simulated", simulatedEvoMode, ".png")
    ggsave(
      filename = paste(dirName, "/", fileName, sep = "")
    )
  }
}

#### Create Tables for Publication ####

## Table 1: Strong support in strat domain 
scenario="A"
sceTabA= cbind(strongSupportPropStrat[scenario,"2 km",,],strongSupportPropStrat[scenario,"6 km",,],strongSupportPropStrat[scenario,"8 km",,],strongSupportPropStrat[scenario,"10 km",,],strongSupportPropStrat[scenario,"12 km",,])
scenario="B"
sceTabB= cbind(strongSupportPropStrat[scenario,"2 km",,],strongSupportPropStrat[scenario,"6 km",,],strongSupportPropStrat[scenario,"8 km",,],strongSupportPropStrat[scenario,"10 km",,],strongSupportPropStrat[scenario,"12 km",,])
sceTab=rbind(sceTabA,sceTabB)

write.csv(100 * sceTab,file = "tables/Table1_Raw.csv")

## Table 2: Strong support in time domain
scenario="A"
sceTabTA=matrix(data=NA,ncol=4*4, nrow=length(noOfSamplingLoc),dimnames = list(noOfSamplingLoc,NULL))
i=1
name=c()
for (simMode in simulatedEvoModes){
  for (resMode in testedEvoModes){
    name=c(name,paste(simMode,resMode))
    sceTabTA[,i]=strongSupportPropTime[scenario,,simMode,resMode]
    i=i+1
  }
}
colnames(sceTabTA)=name
write.csv(100 * sceTabTA,file = "tables/Table2_Raw.csv")

## Supp table 1
scenario="B"
sceTabTB=matrix(data=NA,ncol=4*4, nrow=length(noOfSamplingLoc),dimnames = list(noOfSamplingLoc,NULL))
i=1
name=c()
for (simMode in simulatedEvoModes){
  for (resMode in testedEvoModes){
    name=c(name,paste(simMode,resMode))
    sceTabTB[,i]=strongSupportPropTime[scenario,,simMode,resMode]
    i=i+1
  }
}
colnames(sceTabTB)=name
write.csv(100 * sceTabTB,file = "tables/Supp_Table1_Raw.csv")



#### Figs for publication: Grouped Boxplots of AICweights (Figure 6, 7 & S9)####

#AIC weights over height both scenarios (Figure 6)
{
  require("gridExtra")
  get_only_legend <- function(plot) {
    
    # get tabular interpretation of plot
    plot_table <- ggplot_gtable(ggplot_build(plot)) 
    
    #  Mark only legend in plot
    legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
    
    # extract legend
    legend <- plot_table$grobs[[legend_plot]]
    
    # return legend
    return(legend) 
  }
#Scenario A
{
### Stasis A ###
simulatedEvoMode=simulatedEvoModes[1]
scenario=scenarioNames[1]
    df=data.frame(position=NULL,testedMode=NULL,AIC=NULL)
    for (pos in examinedBasinPositions){
      for (testedEvoMode in testedEvoModes){
        df=rbind(df,data.frame(position=as.factor(rep(pos,length(AkaikeWtArrayStrat[1,1,1,1,]))),
                               testedEvoMode=rep(testedEvoMode,length(AkaikeWtArrayStrat[1,1,1,1,])),
                               AIC=AkaikeWtArrayStrat[scenario,pos,simulatedEvoMode,testedEvoMode,]))
        
      }
    }
 
    Plot1=ggplot2::ggplot(df, aes(x=position, y=AIC, fill=testedEvoMode)) + 
      geom_boxplot(outlier.shape = NA,lwd=0.1) +
       labs(tag = "A")+
      scale_fill_brewer(palette="Spectral")+
      theme(legend.position="none", plot.title =element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),text = element_text(size = 6))+
      labs(title = paste("Stasis"), y="AIC weight", x="Distance from Shore (km)")+
     scale_x_discrete( labels=c("2 km"="2","6 km"="6","8 km"="8","10 km"="10","12 km"="12"))+
      scale_y_continuous(limits=c(0,1))
### BM A ###
    simulatedEvoMode=simulatedEvoModes[2]
    scenario=scenarioNames[1]
    df=data.frame(position=NULL,testedMode=NULL,AIC=NULL)
    for (pos in examinedBasinPositions){
      for (testedEvoMode in testedEvoModes){
        df=rbind(df,data.frame(position=as.factor(rep(pos,length(AkaikeWtArrayStrat[1,1,1,1,]))),
                               testedEvoMode=rep(testedEvoMode,length(AkaikeWtArrayStrat[1,1,1,1,])),
                               AIC=AkaikeWtArrayStrat[scenario,pos,simulatedEvoMode,testedEvoMode,]))
        
      }
    }
    Plot2=ggplot2::ggplot(df, aes(x=position, y=AIC, fill=testedEvoMode)) + 
      geom_boxplot(outlier.shape = NA,lwd=0.1) +
      labs(tag = "B")+
      scale_fill_brewer(palette="Spectral")+
      theme(legend.position="none", plot.title =element_text(size = 8),text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),axis.title.y=element_blank())+
      labs(title = paste("Brownian Motion"), x="Distance from Shore (km)")+
      scale_x_discrete( labels=c("2 km"="2","6 km"="6","8 km"="8","10 km"="10","12 km"="12"))+
      scale_y_continuous(limits=c(0,1))
    
    ### WBD A ###
    simulatedEvoMode=simulatedEvoModes[3]
    scenario=scenarioNames[1]
    df=data.frame(position=NULL,testedMode=NULL,AIC=NULL)
    for (pos in examinedBasinPositions){
      for (testedEvoMode in testedEvoModes){
        df=rbind(df,data.frame(position=as.factor(rep(pos,length(AkaikeWtArrayStrat[1,1,1,1,]))),
                               testedEvoMode=rep(testedEvoMode,length(AkaikeWtArrayStrat[1,1,1,1,])),
                               AIC=AkaikeWtArrayStrat[scenario,pos,simulatedEvoMode,testedEvoMode,]))
        
      }
    }
    Plot3=ggplot2::ggplot(df, aes(x=position, y=AIC, fill=testedEvoMode)) + 
      geom_boxplot(outlier.shape = NA,lwd=0.1) +
      labs(tag = "C")+
      scale_fill_brewer(palette="Spectral")+
      theme(legend.position="none", plot.title =element_text(size = 8),text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98), axis.title.y=element_blank())+
      labs(title = paste("Weak Brownian Drift"), x="Distance from Shore (km)")+
      scale_x_discrete( labels=c("2 km"="2","6 km"="6","8 km"="8","10 km"="10","12 km"="12"))+
      scale_y_continuous(limits=c(0,1))

    
    ### SBD A ###
    simulatedEvoMode=simulatedEvoModes[4]
    scenario=scenarioNames[1]
    df=data.frame(position=NULL,testedMode=NULL,AIC=NULL)
    for (pos in examinedBasinPositions){
      for (testedEvoMode in testedEvoModes){
        df=rbind(df,data.frame(position=as.factor(rep(pos,length(AkaikeWtArrayStrat[1,1,1,1,]))),
                               testedEvoMode=rep(testedEvoMode,length(AkaikeWtArrayStrat[1,1,1,1,])),
                               AIC=AkaikeWtArrayStrat[scenario,pos,simulatedEvoMode,testedEvoMode,]))
        
      }
    }
    Plot4=ggplot2::ggplot(df, aes(x=position, y=AIC, fill=testedEvoMode)) + 
      geom_boxplot(outlier.shape = NA,lwd=0.1) +
      labs(tag = "D")+
      scale_fill_brewer(palette="Spectral")+
      theme(legend.position="none",plot.title =element_text(size = 8),text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),
            legend.text = element_text(size = 5),legend.title = element_text(size = 5), axis.title.y=element_blank())+
      labs(title = paste("Strong Brownian Drift"), x="Distance from Shore (km)", fill= "Tested Mode")+
      scale_x_discrete( labels=c("2 km"="2","6 km"="6","8 km"="8","10 km"="10","12 km"="12"))+
      scale_y_continuous(limits=c(0,1))
}

#Scenario B
{
  ### Stasis B ###
  simulatedEvoMode=simulatedEvoModes[1]
  scenario=scenarioNames[2]
  df=data.frame(position=NULL,testedMode=NULL,AIC=NULL)
  for (pos in examinedBasinPositions){
    for (testedEvoMode in testedEvoModes){
      df=rbind(df,data.frame(position=as.factor(rep(pos,length(AkaikeWtArrayStrat[1,1,1,1,]))),
                             testedEvoMode=rep(testedEvoMode,length(AkaikeWtArrayStrat[1,1,1,1,])),
                             AIC=AkaikeWtArrayStrat[scenario,pos,simulatedEvoMode,testedEvoMode,]))
      
    }
  }
  Plot5=ggplot2::ggplot(df, aes(x=position, y=AIC, fill=testedEvoMode)) + 
    geom_boxplot(outlier.shape = NA,lwd=0.1) +
    labs(tag = "E")+
    scale_fill_brewer(palette="Spectral")+
    theme(legend.position="none", plot.title =element_text(size = 8),text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98))+
    labs(title = paste("Stasis"), y="AIC weight", x="Distance from Shore (km)")+
    scale_x_discrete( labels=c("2 km"="2","6 km"="6","8 km"="8","10 km"="10","12 km"="12"))+
    scale_y_continuous(limits=c(0,1))
  ### BM B ###
  simulatedEvoMode=simulatedEvoModes[2]
  scenario=scenarioNames[2]
  df=data.frame(position=NULL,testedMode=NULL,AIC=NULL)
  for (pos in examinedBasinPositions){
    for (testedEvoMode in testedEvoModes){
      df=rbind(df,data.frame(position=as.factor(rep(pos,length(AkaikeWtArrayStrat[1,1,1,1,]))),
                             testedEvoMode=rep(testedEvoMode,length(AkaikeWtArrayStrat[1,1,1,1,])),
                             AIC=AkaikeWtArrayStrat[scenario,pos,simulatedEvoMode,testedEvoMode,]))
      
    }
  }
  Plot6=ggplot2::ggplot(df, aes(x=position, y=AIC, fill=testedEvoMode)) + 
    geom_boxplot(outlier.shape = NA,lwd=0.1) +
    labs(tag = "F")+
    scale_fill_brewer(palette="Spectral")+
    theme(legend.position="none", plot.title =element_text(size = 8),text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98), axis.title.y=element_blank())+
    labs(title = paste("Brownian Motion"), x="Distance from Shore (km)")+
    scale_x_discrete( labels=c("2 km"="2","6 km"="6","8 km"="8","10 km"="10","12 km"="12"))+
    scale_y_continuous(limits=c(0,1))
  
  ### WBD B ###
  simulatedEvoMode=simulatedEvoModes[3]
  scenario=scenarioNames[2]
  df=data.frame(position=NULL,testedMode=NULL,AIC=NULL)
  for (pos in examinedBasinPositions){
    for (testedEvoMode in testedEvoModes){
      df=rbind(df,data.frame(position=as.factor(rep(pos,length(AkaikeWtArrayStrat[1,1,1,1,]))),
                             testedEvoMode=rep(testedEvoMode,length(AkaikeWtArrayStrat[1,1,1,1,])),
                             AIC=AkaikeWtArrayStrat[scenario,pos,simulatedEvoMode,testedEvoMode,]))
      
    }
  }
  Plot7=ggplot2::ggplot(df, aes(x=position, y=AIC, fill=testedEvoMode)) + 
    geom_boxplot(outlier.shape = NA,lwd=0.1) +
    labs(tag = "G")+
    scale_fill_brewer(palette="Spectral")+
    theme(legend.position="none", plot.title =element_text(size = 8),text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98), axis.title.y=element_blank())+
    labs(title = paste("Weak Brownian Drift"), x="Distance from Shore (km)")+
    scale_x_discrete( labels=c("2 km"="2","6 km"="6","8 km"="8","10 km"="10","12 km"="12"))+
    scale_y_continuous(limits=c(0,1))
  
  
  ### SBD B ###
  simulatedEvoMode=simulatedEvoModes[4]
  scenario=scenarioNames[2]
  df=data.frame(position=NULL,testedMode=NULL,AIC=NULL)
  for (pos in examinedBasinPositions){
    for (testedEvoMode in testedEvoModes){
      df=rbind(df,data.frame(position=as.factor(rep(pos,length(AkaikeWtArrayStrat[1,1,1,1,]))),
                             testedEvoMode=rep(testedEvoMode,length(AkaikeWtArrayStrat[1,1,1,1,])),
                             AIC=AkaikeWtArrayStrat[scenario,pos,simulatedEvoMode,testedEvoMode,]))
      
    }
  }
  Plot8=ggplot2::ggplot(df, aes(x=position, y=AIC, fill=testedEvoMode)) + 
    geom_boxplot(outlier.shape = NA,lwd=0.1) +
    labs(tag = "H")+
    scale_fill_brewer(palette="Spectral")+
    theme(legend.position="none", plot.title =element_text(size = 8),text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98))+
    labs(title = paste("Strong Brownian Drift"), x="Distance from Shore (km)",fill= "Tested Mode")+
    scale_x_discrete( labels=c("2 km"="2","6 km"="6","8 km"="8","10 km"="10","12 km"="12"))+
    scale_y_continuous(limits=c(0,1))

PlotLegend=ggplot2::ggplot(df, aes(x=position, y=AIC, fill=testedEvoMode)) + 
    geom_boxplot(outlier.shape = NA,lwd=0.1)+ 
    scale_fill_brewer(palette="Spectral",name = "Tested Mode")+
    theme(plot.title =element_text(size = 8),text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),
          legend.text = element_text(size = 5),legend.title = element_text(size = 5), axis.title.y=element_blank())+
    theme(legend.position = "bottom" )

Legend=get_only_legend(PlotLegend)
  
  combined_plot=grid.arrange(Plot1,Plot2,Plot3,Plot4,Plot5,Plot6,Plot7,Plot8, ncol=4)
  
  {
  pdf(file = paste("figs/R/fig6_raw.pdf"), width=6.5, height = 3.25)
 
  grid.arrange(combined_plot, Legend, nrow = 2, heights = c(10, 1))  #The multiplot
  dev.off()
}
}
}
#Scenario Time, 2 Ma. (Figure 7)
{
{
scenario=scenarioNames[1]

### Time Stasis ###
simulatedEvoMode=simulatedEvoModes[1]
    df=data.frame(nsp=NULL,testedMode=NULL,AIC=NULL)
    for (nsp in noOfSamplingLoc){
      for (testedEvoMode in testedEvoModes){
        df=rbind(df,data.frame(nsp=as.factor(rep(nsp,length(AkaikeWtArrayTime[1,1,1,1,]))),
                               testedEvoMode=rep(testedEvoMode,length(AkaikeWtArrayTime[1,1,1,1,])),
                               AIC=AkaikeWtArrayTime[scenario,nsp,simulatedEvoMode,testedEvoMode,]))
        
      }
    }
    plotT1=ggplot2::ggplot(df, aes(x=nsp, y=AIC, fill=testedEvoMode)) + 
      geom_boxplot(outlier.shape = NA,lwd=0.1) +
      labs(tag = "A")+
      scale_fill_brewer(palette="Spectral")+
      theme(legend.position="none", plot.title =element_text(size = 10), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98), text = element_text(size = 8))+
      labs(title = paste("Stasis"), y="AIC weight", x="Number of Sampling Points")+
      scale_y_continuous(limits=c(0,1))

    ### Time BM ###
    simulatedEvoMode=simulatedEvoModes[2]
    df=data.frame(nsp=NULL,testedMode=NULL,AIC=NULL)
    for (nsp in noOfSamplingLoc){
      for (testedEvoMode in testedEvoModes){
        df=rbind(df,data.frame(nsp=as.factor(rep(nsp,length(AkaikeWtArrayTime[1,1,1,1,]))),
                               testedEvoMode=rep(testedEvoMode,length(AkaikeWtArrayTime[1,1,1,1,])),
                               AIC=AkaikeWtArrayTime[scenario,nsp,simulatedEvoMode,testedEvoMode,]))
        
      }
    }
    plotT2=ggplot2::ggplot(df, aes(x=nsp, y=AIC, fill=testedEvoMode)) + 
      geom_boxplot(outlier.shape = NA,lwd=0.1) +
      labs(tag = "B")+
      scale_fill_brewer(palette="Spectral")+
      theme(legend.position="none",plot.title =element_text(size = 10),text = element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),
            legend.text = element_text(size = 5),legend.title = element_text(size = 5), axis.title.y=element_blank())+
      labs(title = paste("Brownian Motion"),  x="Number of Sampling Points", fill= "Tested Mode")+
      scale_y_continuous(limits=c(0,1))
    
    ### Time WBM ###
    simulatedEvoMode=simulatedEvoModes[3]
    df=data.frame(nsp=NULL,testedMode=NULL,AIC=NULL)
    for (nsp in noOfSamplingLoc){
      for (testedEvoMode in testedEvoModes){
        df=rbind(df,data.frame(nsp=as.factor(rep(nsp,length(AkaikeWtArrayTime[1,1,1,1,]))),
                               testedEvoMode=rep(testedEvoMode,length(AkaikeWtArrayTime[1,1,1,1,])),
                               AIC=AkaikeWtArrayTime[scenario,nsp,simulatedEvoMode,testedEvoMode,]))
        
      }
    }
    plotT3=ggplot2::ggplot(df, aes(x=nsp, y=AIC, fill=testedEvoMode)) + 
      geom_boxplot(outlier.shape = NA,lwd=0.1) +
      labs(tag = "C")+
      scale_fill_brewer(palette="Spectral")+
      theme(legend.position="none", plot.title =element_text(size = 10), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98), text = element_text(size = 8))+
      labs(title = paste("Weak Brownian Drift"), y="AIC weight", x="Number of Sampling Points")+
      scale_y_continuous(limits=c(0,1))

    
    ### Time SBM ###
    simulatedEvoMode=simulatedEvoModes[4]
    df=data.frame(nsp=NULL,testedMode=NULL,AIC=NULL)
    for (nsp in noOfSamplingLoc){
      for (testedEvoMode in testedEvoModes){
        df=rbind(df,data.frame(nsp=as.factor(rep(nsp,length(AkaikeWtArrayTime[1,1,1,1,]))),
                               testedEvoMode=rep(testedEvoMode,length(AkaikeWtArrayTime[1,1,1,1,])),
                               AIC=AkaikeWtArrayTime[scenario,nsp,simulatedEvoMode,testedEvoMode,]))
        
      }
    }
    plotT4=ggplot2::ggplot(df, aes(x=nsp, y=AIC, fill=testedEvoMode)) + 
      geom_boxplot(outlier.shape = NA,lwd=0.1) +
      labs(tag = "D")+
      scale_fill_brewer(palette="Spectral")+
      theme(legend.position="none",plot.title =element_text(size = 10),text = element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),
            legend.text = element_text(size = 5),legend.title = element_text(size = 5), axis.title.y=element_blank())+
      labs(title = paste("Strong Brownian Drift"), x="Number of Sampling Points", fill= "Tested Mode")+
      scale_y_continuous(limits=c(0,1))
}
  PlotLegend=ggplot2::ggplot(df, aes(x=nsp, y=AIC, fill=testedEvoMode)) + 
    geom_boxplot(outlier.shape = NA,lwd=0.1)+ 
    scale_fill_brewer(palette="Spectral",name = "Tested Mode")+
    theme(plot.title =element_text(size = 10),text = element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),
          legend.text = element_text(size = 5),legend.title = element_text(size = 5), axis.title.y=element_blank())+
    theme(legend.position = "bottom" )
  
  Legend=get_only_legend(PlotLegend)
  
  combined_plot=grid.arrange(plotT1,plotT2,plotT3,plotT4, ncol=2)
  
{
  pdf(file = paste("figs/R/fig7_raw.pdf"), width=6.5, height = 3.25)
  grid.arrange(combined_plot, Legend, nrow = 2, heights = c(10, 1))  #The multiplot
  dev.off()
}
}

#Scenario Time, 2.58 Ma. (Figure S9)
{
{
  scenario=scenarioNames[2]
  
  ### Time Stasis ###
  simulatedEvoMode=simulatedEvoModes[1]
  df=data.frame(nsp=NULL,testedMode=NULL,AIC=NULL)
  for (nsp in noOfSamplingLoc){
    for (testedEvoMode in testedEvoModes){
      df=rbind(df,data.frame(nsp=as.factor(rep(nsp,length(AkaikeWtArrayTime[1,1,1,1,]))),
                             testedEvoMode=rep(testedEvoMode,length(AkaikeWtArrayTime[1,1,1,1,])),
                             AIC=AkaikeWtArrayTime[scenario,nsp,simulatedEvoMode,testedEvoMode,]))
      
    }
  }
  plotT1=ggplot2::ggplot(df, aes(x=nsp, y=AIC, fill=testedEvoMode)) + 
    geom_boxplot(outlier.shape = NA,lwd=0.1) +
    labs(tag = "A")+
    scale_fill_brewer(palette="Spectral")+
    theme(legend.position="none", plot.title =element_text(size = 10), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98), text = element_text(size = 8))+
    labs(title = paste("Stasis"), y="AIC weight", x="Number of Sampling Points")+
    scale_y_continuous(limits=c(0,1))
  
  ### Time BM ###
  simulatedEvoMode=simulatedEvoModes[2]
  df=data.frame(nsp=NULL,testedMode=NULL,AIC=NULL)
  for (nsp in noOfSamplingLoc){
    for (testedEvoMode in testedEvoModes){
      df=rbind(df,data.frame(nsp=as.factor(rep(nsp,length(AkaikeWtArrayTime[1,1,1,1,]))),
                             testedEvoMode=rep(testedEvoMode,length(AkaikeWtArrayTime[1,1,1,1,])),
                             AIC=AkaikeWtArrayTime[scenario,nsp,simulatedEvoMode,testedEvoMode,]))
    }
  }
  plotT2=ggplot2::ggplot(df, aes(x=nsp, y=AIC, fill=testedEvoMode)) + 
    geom_boxplot(outlier.shape = NA,lwd=0.1) +
    labs(tag = "B")+
    scale_fill_brewer(palette="Spectral")+
    theme(legend.position="none",plot.title =element_text(size = 10),text = element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),
          legend.text = element_text(size = 5),legend.title = element_text(size = 5), axis.title.y=element_blank())+
    labs(title = paste("Brownian Motion"),  x="Number of Sampling Points", fill= "Tested Mode")+
    scale_y_continuous(limits=c(0,1))
  
  ### Time WBM ###
  simulatedEvoMode=simulatedEvoModes[3]
  df=data.frame(nsp=NULL,testedMode=NULL,AIC=NULL)
  for (nsp in noOfSamplingLoc){
    for (testedEvoMode in testedEvoModes){
      df=rbind(df,data.frame(nsp=as.factor(rep(nsp,length(AkaikeWtArrayTime[1,1,1,1,]))),
                             testedEvoMode=rep(testedEvoMode,length(AkaikeWtArrayTime[1,1,1,1,])),
                             AIC=AkaikeWtArrayTime[scenario,nsp,simulatedEvoMode,testedEvoMode,]))
      
    }
  }
  plotT3=ggplot2::ggplot(df, aes(x=nsp, y=AIC, fill=testedEvoMode)) + 
    geom_boxplot(outlier.shape = NA,lwd=0.1) +
    labs(tag = "C")+
    scale_fill_brewer(palette="Spectral")+
    theme(legend.position="none", plot.title =element_text(size = 10), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98), text = element_text(size = 8))+
    labs(title = paste("Weak Brownian Drift"), y="AIC weight", x="Number of Sampling Points")+
    scale_y_continuous(limits=c(0,1))
  
  
  ### Time SBM ###
  simulatedEvoMode=simulatedEvoModes[4]
  df=data.frame(nsp=NULL,testedMode=NULL,AIC=NULL)
  for (nsp in noOfSamplingLoc){
    for (testedEvoMode in testedEvoModes){
      df=rbind(df,data.frame(nsp=as.factor(rep(nsp,length(AkaikeWtArrayTime[1,1,1,1,]))),
                             testedEvoMode=rep(testedEvoMode,length(AkaikeWtArrayTime[1,1,1,1,])),
                             AIC=AkaikeWtArrayTime[scenario,nsp,simulatedEvoMode,testedEvoMode,]))
      
    }
  }
  plotT4=ggplot2::ggplot(df, aes(x=nsp, y=AIC, fill=testedEvoMode)) + 
    geom_boxplot(outlier.shape = NA,lwd=0.1) +
    labs(tag = "D")+
    scale_fill_brewer(palette="Spectral")+
    theme(legend.position="none",plot.title =element_text(size = 10),text = element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),
          legend.text = element_text(size = 5),legend.title = element_text(size = 5), axis.title.y=element_blank())+
    labs(title = paste("Strong Brownian Drift"), x="Number of Sampling Points", fill= "Tested Mode")+
    scale_y_continuous(limits=c(0,1))
}
  PlotLegend=ggplot2::ggplot(df, aes(x=nsp, y=AIC, fill=testedEvoMode)) + 
    geom_boxplot(outlier.shape = NA,lwd=0.1)+ 
    scale_fill_brewer(palette="Spectral",name = "Tested Mode")+
    theme(plot.title =element_text(size = 10),text = element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),
          legend.text = element_text(size = 5),legend.title = element_text(size = 5), axis.title.y=element_blank())+
    theme(legend.position = "bottom" )
  
  Legend=get_only_legend(PlotLegend)
  
  combined_plot=grid.arrange(plotT1,plotT2,plotT3,plotT4, ncol=2)
  
  {
    pdf(file = paste("figs/R/figS9_raw.pdf"), width=6.5, height = 3.25)
    grid.arrange(combined_plot, Legend, nrow = 2, heights = c(10, 1))  #The multiplot
    dev.off()
}
}

#### Figs for publication: Trait value over height graphs ####


# Dependencies:
{
  #Set the seed (This currently does not seem to work)
  set.seed(99)
  
  #Load required functions for trait simulation:
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
  
  myNormStasis <- function(t,mean = 0,sd = 1) {
    traitValues <- rnorm(
      n = length(t),
      mean = mean,
      sd = sd
    )
    traitList <- list(
      time = t,
      TraitValue = traitValues
    )
    return(traitList)
  } #The formula for Stasis.

}

#### Age depth model grapher (required for figures 4, 5 and S3-S8) ####

load("data/R_outputs/age_depth_models.Rdata")


#Creates the ADM for platform A
{
  # plot age model with erosion  for Basin A#
  # choose age model
  i=20
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

#Creates the ADM for platform B
{
  # plot age model with erosion for Basin B#
  # choose age model
  i=20
  {
    AMTime=ageDepthModels$B[[i]]$time # extract time
    
    AMHeight=ageDepthModels$B[[i]]$height # extract strat height
    
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
    AMTime=ageDepthModels$B[[i]]$time # extract time
    
    AMHeight=ageDepthModels$B[[i]]$height # extract strat height
    
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
    AMTime=ageDepthModels$B[[i]]$time # extract time
    
    AMHeight=ageDepthModels$B[[i]]$height # extract strat height
    
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
    AMTime=ageDepthModels$B[[i]]$time # extract time
    
    AMHeight=ageDepthModels$B[[i]]$height # extract strat height
    
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
    AMTime=ageDepthModels$B[[i]]$time # extract time
    
    AMHeight=ageDepthModels$B[[i]]$height # extract strat height
    
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
  ADM_B=ggplot(data = df, aes(x=x, y=y,col=Distance))+
    geom_path(size=1)+
    labs(tag = "A")+
    ggtitle("Age-Depth Model")+ #for the title
    xlab("Time (Myr)")+ # for the x axis label
    ylab("Height (m)")+ # for the y axis label
    theme_bw()+ #Makes the background white.
    scale_colour_brewer(breaks=c( "2 km", '6 km', '8 km', "10 km", "12 km"),palette="Set1")+
    theme(text = element_text(size = 8), plot.tag = element_text(face = "bold"),#Changes text size
          plot.title = element_text(size=9), 
          plot.tag.position = c(0.01, 0.98),
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
  ADM_B
}


#### Figs for publication: Trait value over height Stasis (Figures S5 & S8)####
{
  #Inputs for the correct simulated mode of evolution:
  Mode=myNormStasis
  simulatedmode="Stasis"
  Mean=0
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
      
      SmallallTraitValues1=approx(allTraitValues1$TraitValue,allTraitValues1$time, n=200)

      
      #2. Making the trait values over time graph#
      #This makes the 5 graphs detectable by ggplot
      graphs=NA
      graphs[1:length(SmallallTraitValues1$x)]=1

      #This puts all the values together for use in ggplot
      full=NA
      full=c(SmallallTraitValues1$x)
      #This makes sure the x values are coupled to the heights properly
      Timespan=NA
      Timespan=c(rep(SmallallTraitValues1$y,1))
      #This creates the labels for colours in the graph
      Label=c(rep("run 1",length(SmallallTraitValues1$x)))
      #Creates a dataframe with all the earlier info
    
      
      df=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
      
      #The plot for all the four lines, forming one graph.
      PlotTT=ggplot(data = df, aes(x=x, y=y,col=Distance))+
        geom_line(size=0.25)+
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
        
        
        # Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        geom_line(size=0.25)+
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
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        geom_line(size=0.25)+
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
        
        
        #Making the trait values over time graph#
        #This makes the 5 graphs detectable by ggplot  
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
        geom_line(size=0.25)+
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
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        geom_line(size=0.25)+
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
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        labs(tag = "G")+
        ggtitle(paste("Distance From Shore: ", as.character(wantedDist/10), " km", sep=""))+ #for the title
        xlab("Height (m)")+ # for the x axis label
        ylab("Trait Value")+ # for the y axis label
        theme_bw()+ #Makes the background white.
        theme(text = element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),legend.position="none",plot.title = element_text(size=9)) #Changes text size
    }
    {
      pdf(file = paste("figs/R/figS5_raw.pdf"), width=6.5, height = 7.5)
      
      multiplot(ADM_A,Plot1,NA,PlotTT,Plot2,Plot4,NA,Plot3,Plot5,cols=3) #The multiplot
      dev.off()
    }
  }
  
  #Basin B:
  p = "B"
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
      
      SmallallTraitValues1=approx(allTraitValues1$TraitValue,allTraitValues1$time, n=200)
      
      
      #2. Making the trait values over time graph#
      #This makes the 5 graphs detectable by ggplot
      graphs=NA
      graphs[1:length(SmallallTraitValues1$x)]=1
      
      #This puts all the values together for use in ggplot
      full=NA
      full=c(SmallallTraitValues1$x)
      #This makes sure the x values are coupled to the heights properly
      Timespan=NA
      Timespan=c(rep(SmallallTraitValues1$y,1))
      #This creates the labels for colours in the graph
      Label=c(rep("run 1",length(SmallallTraitValues1$x)))
      #Creates a dataframe with all the earlier info
      
      
      df=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
      
      #The plot for all the four lines, forming one graph.
      PlotTT=ggplot(data = df, aes(x=x, y=y,col=Distance))+
        geom_line(size=0.25)+
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
        
        
        # Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        geom_line(size=0.25)+
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
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        geom_line(size=0.25)+
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
        
        
        #Making the trait values over time graph#
        #This makes the 5 graphs detectable by ggplot  
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
        geom_line(size=0.25)+
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
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        geom_line(size=0.25)+
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
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        geom_line(size=0.25)+
        labs(tag = "G")+
        ggtitle(paste("Distance From Shore: ", as.character(wantedDist/10), " km", sep=""))+ #for the title
        xlab("Height (m)")+ # for the x axis label
        ylab("Trait Value")+ # for the y axis label
        theme_bw()+ #Makes the background white.
        theme(text = element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),legend.position="none",plot.title = element_text(size=9)) #Changes text size
    }
    {
      pdf(file = paste("figs/R/figS8_raw.pdf"), width=6.5, height = 7.5)
      multiplot(ADM_B,Plot1,NA,PlotTT,Plot2,Plot4,NA,Plot3,Plot5,cols=3) #The multiplot
      
      dev.off()
    }
  }
}

#### Figs for publication: Trait value over height Brownian motion (Figures S4 & S7)####
{
  #Inputs for the correct simulated mode of evolution:
  Mode=myBM
  simulatedmode="Brownian motion"
  Mean=0
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
        
        
        # Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        
        
        #Making the trait values over time graph#
        #This makes the 5 graphs detectable by ggplot  
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
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        labs(tag = "G")+
        ggtitle(paste("Distance From Shore: ", as.character(wantedDist/10), " km", sep=""))+ #for the title
        xlab("Height (m)")+ # for the x axis label
        ylab("Trait Value")+ # for the y axis label
        theme_bw()+ #Makes the background white.
        theme(text = element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),legend.position="none",plot.title = element_text(size=9)) #Changes text size
    }
    {
      pdf(file = paste("figs/R/figS4_raw.pdf"), width=6.5, height = 7.5)
      
      multiplot(ADM_A,Plot1,NA,PlotTT,Plot2,Plot4,NA,Plot3,Plot5,cols=3) #The multiplot
      dev.off()
    }
  }
  
  #Basin B:
  p = "B"
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
        
        
        # Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        
        
        #Making the trait values over time graph#
        #This makes the 5 graphs detectable by ggplot  
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
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        labs(tag = "G")+
        ggtitle(paste("Distance From Shore: ", as.character(wantedDist/10), " km", sep=""))+ #for the title
        xlab("Height (m)")+ # for the x axis label
        ylab("Trait Value")+ # for the y axis label
        theme_bw()+ #Makes the background white.
        theme(text = element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),legend.position="none",plot.title = element_text(size=9)) #Changes text size
    }
    {
      pdf(file = paste("figs/R/figS7_raw.pdf"), width=6.5, height = 7.5)
      
      multiplot(ADM_B,Plot1,NA,PlotTT,Plot2,Plot4,NA,Plot3,Plot5,cols=3) #The multiplot
      dev.off()
    }
  }
}

#### Figs for publication: Trait value over height Weak Brownian drift (Figures S3 & S6)####
{
  #Inputs for the correct simulated mode of evolution:
  Mode=myBM
  simulatedmode="Weak Brownian drift"
  Mean=5
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
        
        
        # Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        
        
        #Making the trait values over time graph#
        #This makes the 5 graphs detectable by ggplot  
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
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        labs(tag = "G")+
        ggtitle(paste("Distance From Shore: ", as.character(wantedDist/10), " km", sep=""))+ #for the title
        xlab("Height (m)")+ # for the x axis label
        ylab("Trait Value")+ # for the y axis label
        theme_bw()+ #Makes the background white.
        theme(text = element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),legend.position="none",plot.title = element_text(size=9)) #Changes text size
    }
    {
      pdf(file = paste("figs/R/figS3_raw.pdf"), width=6.5, height = 7.5)
      
      multiplot(ADM_A,Plot1,NA,PlotTT,Plot2,Plot4,NA,Plot3,Plot5,cols=3) #The multiplot
      dev.off()
    }
  }
  
  #Basin B:
  p = "B"
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
        
        
        # Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        
        
        #Making the trait values over time graph#
        #This makes the 5 graphs detectable by ggplot  
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
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        labs(tag = "G")+
        ggtitle(paste("Distance From Shore: ", as.character(wantedDist/10), " km", sep=""))+ #for the title
        xlab("Height (m)")+ # for the x axis label
        ylab("Trait Value")+ # for the y axis label
        theme_bw()+ #Makes the background white.
        theme(text = element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),legend.position="none",plot.title = element_text(size=9)) #Changes text size
    }
    {
      pdf(file = paste("figs/R/figS6_raw.pdf"), width=6.5, height = 7.5)
      
      multiplot(ADM_B,Plot1,NA,PlotTT,Plot2,Plot4,NA,Plot3,Plot5,cols=3) #The multiplot
      dev.off()
    }
  }
}

#### Figs for publication: Trait value over height Strong Brownian drift (Figures 4 & 5)####
{
  Mode=myBM
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
        
        
        # Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        
        
        #Making the trait values over time graph#
        #This makes the 5 graphs detectable by ggplot  
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
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        labs(tag = "G")+
        ggtitle(paste("Distance From Shore: ", as.character(wantedDist/10), " km", sep=""))+ #for the title
        xlab("Height (m)")+ # for the x axis label
        ylab("Trait Value")+ # for the y axis label
        theme_bw()+ #Makes the background white.
        theme(text = element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),legend.position="none",plot.title = element_text(size=9)) #Changes text size
    }
    {
      pdf(file = paste("figs/R/fig4_raw.pdf"), width=6.5, height = 7.5)
      
      multiplot(ADM_A,Plot1,NA,PlotTT,Plot2,Plot4,NA,Plot3,Plot5,cols=3) #The multiplot
      dev.off()
    }
  }
  
  #Basin B:
  p = "B"
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
        
        
        # Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        
        
        #Making the trait values over time graph#
        #This makes the 5 graphs detectable by ggplot  
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
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        
        
        #Making the trait values over time graph
        #This makes the 5 graphs detectable by ggplot  
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
        labs(tag = "G")+
        ggtitle(paste("Distance From Shore: ", as.character(wantedDist/10), " km", sep=""))+ #for the title
        xlab("Height (m)")+ # for the x axis label
        ylab("Trait Value")+ # for the y axis label
        theme_bw()+ #Makes the background white.
        theme(text = element_text(size = 8), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98),legend.position="none",plot.title = element_text(size=9)) #Changes text size
    }
    {
      pdf(file = paste("figs/R/fig5_raw.pdf"), width=6.5, height = 7.5)
      
      multiplot(ADM_B,Plot1,NA,PlotTT,Plot2,Plot4,NA,Plot3,Plot5,cols=3) #The multiplot
      dev.off()
    }
  }
}

#### Figs for publication: Trait value over time all modes (Figure S2)####
{
  #Choosing the basin:
  p = "B"
  #Creating the stasis graph:
  {
    Mode=myNormStasis
    simulatedmode="Stasis"
    Mean=0
    Deviation=1
    {
      #1.1 The forming of a full evolutionary record over time at all sampled locations for stasis.#
      {
          #Retrieving necessary values:
          AMTime=seq(0.01,2.58,by = 0.0129)
          adjustAllTimes=AMTime
        #Simulating a evolution through all found times
        allTraitValues1=Mode(AMTime,Mean,Deviation)
        allTraitValues2=Mode(AMTime,Mean,Deviation)
        allTraitValues3=Mode(AMTime,Mean,Deviation)
        allTraitValues4=Mode(AMTime,Mean,Deviation)
        allTraitValues5=Mode(AMTime,Mean,Deviation)
      
      
      
      #1.2 Making the trait values over time graph for stasis#
      
      #This makes the 5 graphs detectable by ggplot
      graphs=NA
      graphs[1:length(allTraitValues1$TraitValue)]=1
      graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues2$TraitValue))]=2
      graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues3$TraitValue))]=3
      graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues4$TraitValue))]=4
      graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues5$TraitValue))]=5
      #This puts all the values together for use in ggplot
      full=NA
      full=c(allTraitValues1$TraitValue,allTraitValues2$TraitValue,allTraitValues3$TraitValue,allTraitValues4$TraitValue,allTraitValues5$TraitValue)
      #This makes sure the x values are coupled to the heights properly
      Timespan=NA
      Timespan=c(rep(allTraitValues1$time,5))
      #This creates the labels for colours in the graph
      Label=c(rep("run 1",length(adjustAllTimes)),rep("run 2",length(adjustAllTimes)),rep("run 3",length(adjustAllTimes)),rep("run 4",length(adjustAllTimes)),rep("run 5",length(adjustAllTimes)))
      #Creates a dataframe with all the earlier info
      df=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
      
      #The plot for all the four lines, forming one graph.
      PlotST=ggplot(data = df, aes(x=x, y=y,col=Distance))+
        geom_line(size=0.25)+
        labs(tag = "A")+
        ggtitle("Stasis over time")+ #for the title
        xlab("Time (Myr)")+ # for the x axis label
        ylab("Trait Value")+ # for the y axis label
        theme_bw()+ #Makes the background white.
        theme(text = element_text(size = 8), plot.tag = element_text(face = "bold"), legend.position="none",plot.title = element_text(size=9), plot.tag.position = c(0.01, 0.98)) #Changes text size
    }
  }
  }
  #Creating the Brownian motion graph:
  {
    Mode=myBM
    simulatedmode="Brownian motion"
    Mean=0
    Deviation=1
    
    #1.1 The forming of a full evolutionary record over time at all sampled locations for stasis.#
    {
      #Retrieving necessary values:
      AMTime=seq(0.01,2.58,by = 0.0129)
      adjustAllTimes=AMTime
      #Simulating a evolution through all found times
      allTraitValues1=Mode(AMTime,Mean,Deviation)
      allTraitValues2=Mode(AMTime,Mean,Deviation)
      allTraitValues3=Mode(AMTime,Mean,Deviation)
      allTraitValues4=Mode(AMTime,Mean,Deviation)
      allTraitValues5=Mode(AMTime,Mean,Deviation)
      
      
      #2. Making the trait values over time graph#
      #This makes the 5 graphs detectable by ggplot
      graphs=NA
      graphs[1:length(allTraitValues1$TraitValue)]=1
      graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues2$TraitValue))]=2
      graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues3$TraitValue))]=3
      graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues4$TraitValue))]=4
      graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues5$TraitValue))]=5
      #This puts all the values together for use in ggplot
      full=NA
      full=c(allTraitValues1$TraitValue,allTraitValues2$TraitValue,allTraitValues3$TraitValue,allTraitValues4$TraitValue,allTraitValues5$TraitValue)
      #This makes sure the x values are coupled to the heights properly
      Timespan=NA
      Timespan=c(rep(allTraitValues1$time,5))
      #This creates the labels for colours in the graph
      Label=c(rep("run 1",length(adjustAllTimes)),rep("run 2",length(adjustAllTimes)),rep("run 3",length(adjustAllTimes)),rep("run 4",length(adjustAllTimes)),rep("run 5",length(adjustAllTimes)))
      #Creates a dataframe with all the earlier info
      df=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
      
      #The plot for all the four lines, forming one graph.
      PlotBMT=ggplot(data = df, aes(x=x, y=y,col=Distance))+
        geom_line(size=1)+
        labs(tag = "B")+
        ggtitle("Brownian motion over time")+ #for the title
        xlab("Time (Myr)")+ # for the x axis label
        ylab("Trait Value")+ # for the y axis label
        theme_bw()+ #Makes the background white.
        theme(text = element_text(size = 8), plot.tag = element_text(face = "bold"), legend.position="none",plot.title = element_text(size=9), plot.tag.position = c(0.01, 0.98)) #Changes text size 
    }
  }
  
  #Creating the weak Brownian drift graph:
  {
    Mode=myBM
    simulatedmode="Weak Brownian drift"
    Mean=5
    Deviation=1
    
    #1.1 The forming of a full evolutionary record over time at all sampled locations for stasis.#
    {
      #Retrieving necessary values:
      AMTime=seq(0.01,2.58,by = 0.0129)
      adjustAllTimes=AMTime
      #Simulating a evolution through all found times
      allTraitValues1=Mode(AMTime,Mean,Deviation)
      allTraitValues2=Mode(AMTime,Mean,Deviation)
      allTraitValues3=Mode(AMTime,Mean,Deviation)
      allTraitValues4=Mode(AMTime,Mean,Deviation)
      allTraitValues5=Mode(AMTime,Mean,Deviation)
      
      
      #2. Making the trait values over time graph#
      #This makes the 5 graphs detectable by ggplot
      graphs=NA
      graphs[1:length(allTraitValues1$TraitValue)]=1
      graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues2$TraitValue))]=2
      graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues3$TraitValue))]=3
      graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues4$TraitValue))]=4
      graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues5$TraitValue))]=5
      #This puts all the values together for use in ggplot
      full=NA
      full=c(allTraitValues1$TraitValue,allTraitValues2$TraitValue,allTraitValues3$TraitValue,allTraitValues4$TraitValue,allTraitValues5$TraitValue)
      #This makes sure the x values are coupled to the heights properly
      Timespan=NA
      Timespan=c(rep(allTraitValues1$time,5))
      #This creates the labels for colours in the graph
      Label=c(rep("run 1",length(adjustAllTimes)),rep("run 2",length(adjustAllTimes)),rep("run 3",length(adjustAllTimes)),rep("run 4",length(adjustAllTimes)),rep("run 5",length(adjustAllTimes)))
      #Creates a dataframe with all the earlier info
      df=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
      
      #The plot for all the four lines, forming one graph.
      PlotWBDT=ggplot(data = df, aes(x=x, y=y,col=Distance))+
        geom_line(size=1)+
        labs(tag = "C")+
        ggtitle("Weak Brownian drift over time")+ #for the title
        xlab("Time (Myr)")+ # for the x axis label
        ylab("Trait Value")+ # for the y axis label
        theme_bw()+ #Makes the background white.
        theme(text = element_text(size = 8), plot.tag = element_text(face = "bold"), legend.position="none",plot.title = element_text(size=9), plot.tag.position = c(0.01, 0.98)) #Changes text size 
    }
  }
  
  #Creating the strong Brownian drift graph:
  {
    Mode=myBM
    simulatedmode="Strong Brownian drift"
    Mean=10
    Deviation=1
    
    #1.1 The forming of a full evolutionary record over time at all sampled locations for stasis.#
    {
      #Retrieving necessary values:
      AMTime=seq(0.01,2.58,by = 0.0129)
      adjustAllTimes=AMTime
      #Simulating a evolution through all found times
      allTraitValues1=Mode(AMTime,Mean,Deviation)
      allTraitValues2=Mode(AMTime,Mean,Deviation)
      allTraitValues3=Mode(AMTime,Mean,Deviation)
      allTraitValues4=Mode(AMTime,Mean,Deviation)
      allTraitValues5=Mode(AMTime,Mean,Deviation)
      
      #2. Making the trait values over time graph#
      #This makes the 5 graphs detectable by ggplot
      graphs=NA
      graphs[1:length(allTraitValues1$TraitValue)]=1
      graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues2$TraitValue))]=2
      graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues3$TraitValue))]=3
      graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues4$TraitValue))]=4
      graphs[(length(graphs)+1):(length(graphs)+length(allTraitValues5$TraitValue))]=5
      #This puts all the values together for use in ggplot
      full=NA
      full=c(allTraitValues1$TraitValue,allTraitValues2$TraitValue,allTraitValues3$TraitValue,allTraitValues4$TraitValue,allTraitValues5$TraitValue)
      #This makes sure the x values are coupled to the heights properly
      Timespan=NA
      Timespan=c(rep(allTraitValues1$time,5))
      #This creates the labels for colours in the graph
      Label=c(rep("run 1",length(adjustAllTimes)),rep("run 2",length(adjustAllTimes)),rep("run 3",length(adjustAllTimes)),rep("run 4",length(adjustAllTimes)),rep("run 5",length(adjustAllTimes)))
      #Creates a dataframe with all the earlier info
      df=data.frame(x=Timespan,y=full,variable=graphs,Distance=Label)
      
      #The plot for all the four lines, forming one graph.
      PlotSBDT=ggplot(data = df, aes(x=x, y=y,col=Distance))+
        geom_line(size=1)+
        labs(tag = "D")+
        ggtitle("Strong Brownian drift over time")+ #for the title
        xlab("Time (Myr)")+ # for the x axis label
        ylab("Trait Value")+ # for the y axis label
        theme_bw()+ #Makes the background white.
        theme(text = element_text(size = 8), plot.tag = element_text(face = "bold"), legend.position="none",plot.title = element_text(size=9), plot.tag.position = c(0.01, 0.98)) #Changes text size 
    }
  }
  
  {
    pdf(file = paste("figs/R/figS2_raw.pdf"), width=6.5, height = 5)
    
    multiplot(PlotST,PlotWBDT,PlotBMT,PlotSBDT,cols=2) #The multiplot
    dev.off()
  }
}

