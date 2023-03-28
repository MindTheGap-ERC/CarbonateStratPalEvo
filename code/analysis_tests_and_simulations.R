#### load required packages ####
# install.packages(ggtern)
require(paleoTS)
require(grid)

source("code/multiplot.R")

#### load test results ####
load("data/R_outputs/results_modes_of_evolution.Rdata")


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
    ylab = "Trait value",
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
    text = "Trait value",
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

# trait vlaues in time domain
{
  plot(
    x = testResultsStrat[[scenario]][[dist]][[mode]][[run]]$inputData$time,
    y = testResultsStrat[[scenario]][[dist]][[mode]][[run]]$inputData$traitValue,
    type = "l",
    xlab = "Time [Ma]",
    ylab = "Trait value",
    main = paste("Trait values in time domain, scenario ", scenario, "\n", mode, ", run no. ", run, sep = "")
  )
}
#### Basic Visuals: Plot Results in Time Domain ####
scenario <- "B" # one of A","B"
nSamp <- "50" # one of "5"   "10"  "15"  "20"  "25"  "35"  "50"  "100" "200"
mode <- "stasis" # one of "stasis","Brownian motion","weak Brownian drift","strong Brownian drift"
run <- 1 # integer between 1 and noOfTests

# trait vlaues in time domain, classic representation
{
  plot(
    x = testResultsTime[[scenario]][[nSamp]][[mode]][[run]]$inputData$time,
    y = testResultsTime[[scenario]][[nSamp]][[mode]][[run]]$inputData$traitValue,
    type = "l",
    xlab = "Time [Ma]",
    ylab = "Trait value",
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
# trait vlaues in time domain, paleoTS representation
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
    text = "Trait value",
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



#### Time Domain: WHich models have strong support? ####
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
      geom_boxplot(outlier.shape = NA) +
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
      geom_boxplot(outlier.shape = NA) +
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



#### Figs for publication: Grouped Boxplots of AICweights ####

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
      geom_boxplot(outlier.shape = NA) +
      scale_fill_brewer(palette="Spectral")+
      theme(legend.position="none", plot.title =element_text(face="bold"),text = element_text(size = 20))+
      labs(title = paste("A. ", simulatedEvoMode,sep=""), y="AIC weight", x="Distance from shore")
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
      geom_boxplot(outlier.shape = NA) +
      scale_fill_brewer(palette="Spectral")+
      theme(legend.position="none", plot.title =element_text(face="bold"),text = element_text(size = 20),axis.title.y=element_blank())+
      labs(title = paste("B. ", simulatedEvoMode,sep=""), x="Distance from shore")
    
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
      geom_boxplot(outlier.shape = NA) +
      scale_fill_brewer(palette="Spectral")+
      theme(legend.position="none", plot.title =element_text(face="bold"),text = element_text(size = 20), axis.title.y=element_blank())+
      labs(title = paste("C. ", simulatedEvoMode,sep=""), x="Distance from shore")

    
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
      geom_boxplot(outlier.shape = NA) +
      scale_fill_brewer(palette="Spectral")+
      theme(plot.title =element_text(face="bold"),text = element_text(size = 20),
            legend.text = element_text(size = 10),legend.title = element_text(size = 10), axis.title.y=element_blank())+
      labs(title = paste("D. ", simulatedEvoMode,sep=""), x="Distance from shore", fill= "Tested Mode")
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
    geom_boxplot(outlier.shape = NA) +
    scale_fill_brewer(palette="Spectral")+
    theme(legend.position="none", plot.title =element_text(face="bold"),text = element_text(size = 20))+
    labs(title = paste("E. ", simulatedEvoMode,sep=""), y="AIC weight", x="Distance from shore")
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
    geom_boxplot(outlier.shape = NA) +
    scale_fill_brewer(palette="Spectral")+
    theme(legend.position="none", plot.title =element_text(face="bold"),text = element_text(size = 20), axis.title.y=element_blank())+
    labs(title = paste("F. ", simulatedEvoMode,sep=""), x="Distance from shore")
  
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
    geom_boxplot(outlier.shape = NA) +
    scale_fill_brewer(palette="Spectral")+
    theme(legend.position="none", plot.title =element_text(face="bold"),text = element_text(size = 20), axis.title.y=element_blank())+
    labs(title = paste("G. ", simulatedEvoMode,sep=""), x="Distance from shore")
  
  
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
    geom_boxplot(outlier.shape = NA) +
    scale_fill_brewer(palette="Spectral")+
    theme(plot.title =element_text(face="bold"),text = element_text(size = 20),
          legend.text = element_text(size = 10),legend.title = element_text(size = 10), axis.title.y=element_blank())+
    labs(title = paste("H. ", simulatedEvoMode,sep=""), x="Distance from shore",fill= "Tested Mode")
}

multiplot(Plot1,Plot5,Plot2,Plot6,Plot3,Plot7,Plot4,Plot8,cols=4) #The multiplot



#Scenario Time
{
Scenario="time"

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
      geom_boxplot(outlier.shape = NA) +
      scale_fill_brewer(palette="Spectral")+
      theme(legend.position="none", plot.title =element_text(face="bold"),text = element_text(size = 20))+
      labs(title = paste("A. ", simulatedEvoMode,sep=""), y="AIC weight", x="Number of Sampling Points")

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
      geom_boxplot(outlier.shape = NA) +
      scale_fill_brewer(palette="Spectral")+
      theme(plot.title =element_text(face="bold"),text = element_text(size = 20),
            legend.text = element_text(size = 10),legend.title = element_text(size = 10), axis.title.y=element_blank())+
      labs(title = paste("B. ", simulatedEvoMode,sep=""),  x="Number of Sampling Points", fill= "Tested Mode")
    
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
      geom_boxplot(outlier.shape = NA) +
      scale_fill_brewer(palette="Spectral")+
      theme(legend.position="none", plot.title =element_text(face="bold"),text = element_text(size = 20))+
      labs(title = paste("C. ", simulatedEvoMode,sep=""), y="AIC weight", x="Number of Sampling Points")

    
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
      geom_boxplot(outlier.shape = NA) +
      scale_fill_brewer(palette="Spectral")+
      theme(plot.title =element_text(face="bold"),text = element_text(size = 20),
            legend.text = element_text(size = 10),legend.title = element_text(size = 10), axis.title.y=element_blank())+
      labs(title = paste("D. ", simulatedEvoMode,sep=""), x="Number of Sampling Points", fill= "Tested Mode")
}

multiplot(plotT1,plotT3,plotT2,plotT4,cols=2) #The multiplot
