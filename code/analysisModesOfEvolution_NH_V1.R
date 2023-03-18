#### set your wd correctly! ####

#### load required packages ####
#install.packages(ggtern)
require(ggtern)
require(paleoTS)

#### load test results ####
load("resultsTestModesOfEvolution.Rdata")


#### plot results in strat domain ####
dist="2 km" # one of "2 km","6 km","8 km","10 km","12 km"
scenario="A" # one of A","B"
mode="strong Brownian drift" # one of "stasis","Brownian motion","weak Brownian drift","strong Brownian drift"
run=1 # integer between 1 and noOfTests
## Age-depth model
{
  plot(x=testResultsStrat[[scenario]][[dist]][[mode]][[run]]$inputData$time,
       y=testResultsStrat[[scenario]][[dist]][[mode]][[run]]$inputData$height,
       type="l",
       xlab="Time [Ma]",
       ylab="Height [m]",
       main = paste("Age-depth model, scenario ", scenario, ", ", dist, " offshore", sep=""))
}
## Trait values in strat domain, standard visualisation
{
  plot(x=testResultsStrat[[scenario]][[dist]][[mode]][[run]]$inputData$height,
       y=testResultsStrat[[scenario]][[dist]][[mode]][[run]]$inputData$traitValue,
       type="l",
       xlab="Height [m]",
       ylab="Trait value",
       main = paste("Trait values in stratigraphic domain, scenario ", scenario, ", ", dist, " offshore \n", mode, ", run no. ", run, sep=""))
  # add AICweights
  legend("topleft",legend=c("AICweights",
                            paste("Stasis: ", as.character(testResultsStrat[[scenario]][[dist]][[mode]][[run]]$testRes$modelFits["Stasis","Akaike.wt"]), sep = ""),
                            paste("URW: ", as.character(testResultsStrat[[scenario]][[dist]][[mode]][[run]]$testRes$modelFits["URW","Akaike.wt"]), sep = ""),
                            paste("GRW: ", as.character(testResultsStrat[[scenario]][[dist]][[mode]][[run]]$testRes$modelFits["GRW","Akaike.wt"]), sep = ""),
                            paste("OU: ", as.character(testResultsStrat[[scenario]][[dist]][[mode]][[run]]$testRes$modelFits["OU","Akaike.wt"]), sep = "")))
}

## Trait values in strat, paleoTS visualisation
#note that the axis labels are hardcoded in the paleoTS package, and can not be changed via the "xlab"/"ylab" option
{
  plot(testResultsStrat[[scenario]][[dist]][[mode]][[run]]$paleots,
       main=paste("Trait values in stratigraphic domain, scenario ", scenario, ", ", dist, " offshore \n", mode, ", run no. ", run, sep=""))
  # manually add axis labels
  mtext("Height [m]",side=1,line=2,cex=1.4)
  mtext("Trait value",side=2,line=2,cex = 1.4)
  # add legend
  legend("topleft",legend=c("AICweights",
                            paste("Stasis: ", as.character(testResultsStrat[[scenario]][[dist]][[mode]][[run]]$testRes$modelFits["Stasis","Akaike.wt"]), sep = ""),
                            paste("URW: ", as.character(testResultsStrat[[scenario]][[dist]][[mode]][[run]]$testRes$modelFits["URW","Akaike.wt"]), sep = ""),
                            paste("GRW: ", as.character(testResultsStrat[[scenario]][[dist]][[mode]][[run]]$testRes$modelFits["GRW","Akaike.wt"]), sep = ""),
                            paste("OU: ", as.character(testResultsStrat[[scenario]][[dist]][[mode]][[run]]$testRes$modelFits["OU","Akaike.wt"]), sep = "")))
}

# trait vlaues in time domain
{
  plot(x=testResultsStrat[[scenario]][[dist]][[mode]][[run]]$inputData$time,
       y=testResultsStrat[[scenario]][[dist]][[mode]][[run]]$inputData$traitValue,
       type="l",
       xlab="Time [Ma]",
       ylab="Trait value",
       main = paste("Trait values in time domain, scenario ", scenario, "\n", mode, ", run no. ", run, sep=""))
}
#### Plot Results of Tests in Time Domain ####
scenario="B" # one of A","B"
nSamp="50" # one of "5"   "10"  "15"  "20"  "25"  "35"  "50"  "100" "200"
mode="stasis" # one of "stasis","Brownian motion","weak Brownian drift","strong Brownian drift"
run=1 # integer between 1 and noOfTests

# trait vlaues in time domain, classic representation
{
  plot(x=testResultsTime[[scenario]][[nSamp]][[mode]][[run]]$inputData$time,
       y=testResultsTime[[scenario]][[nSamp]][[mode]][[run]]$inputData$traitValue,
       type="l",
       xlab="Time [Ma]",
       ylab="Trait value",
       main = paste("Trait values in time domain \n", nSamp, " samples, equidistantly distributed over ", maxTimes[[scenario]], " Ma", "\n" , mode, ", run no. ", run, sep=""))
  # add legend
  legend("topleft",legend=c("AICweights",
                            paste("Stasis: ", as.character(testResultsTime[[scenario]][[nSamp]][[mode]][[run]]$testRes$modelFits["Stasis","Akaike.wt"]), sep = ""),
                            paste("URW: ", as.character(testResultsTime[[scenario]][[nSamp]][[mode]][[run]]$testRes$modelFits["URW","Akaike.wt"]), sep = ""),
                            paste("GRW: ", as.character(testResultsTime[[scenario]][[nSamp]][[mode]][[run]]$testRes$modelFits["GRW","Akaike.wt"]), sep = ""),
                            paste("OU: ", as.character(testResultsTime[[scenario]][[nSamp]][[mode]][[run]]$testRes$modelFits["OU","Akaike.wt"]), sep = "")))
}
# trait vlaues in time domain, paleoTS representation
#note that the axis labels are hardcoded in the paleoTS package, and can not be changed via the "xlab"/"ylab" option
{
  plot(x=testResultsTime[[scenario]][[nSamp]][[mode]][[run]]$paleots,
        main = paste("Trait values in time domain \n", nSamp, " samples, equidistantly distributed over ", maxTimes[[scenario]], " Ma", "\n" , mode, ", run no. ", run, sep=""))
    # manually add axis labels
    mtext("Time [Ma]",side=1,line=2,cex=1.4)
    mtext("Trait value",side=2,line=2,cex = 1.4)
    legend("topleft",legend=c("AICweights",
                              paste("Stasis: ", as.character(testResultsTime[[scenario]][[nSamp]][[mode]][[run]]$testRes$modelFits["Stasis","Akaike.wt"]), sep = ""),
                              paste("URW: ", as.character(testResultsTime[[scenario]][[nSamp]][[mode]][[run]]$testRes$modelFits["URW","Akaike.wt"]), sep = ""),
                              paste("GRW: ", as.character(testResultsTime[[scenario]][[nSamp]][[mode]][[run]]$testRes$modelFits["GRW","Akaike.wt"]), sep = ""),
                              paste("OU: ", as.character(testResultsTime[[scenario]][[nSamp]][[mode]][[run]]$testRes$modelFits["OU","Akaike.wt"]), sep = "")))
}

#### Extract Akaike Weights from Results ####
AkaikeWtArrayStrat=array(NA,
                        dim=c(length(scenarioNames),length(examinedBasinPositions),length(simulatedEvoModes),length(testedEvoModes),noOfTests),
                        dimnames = list(scenarioNames,
                                        examinedBasinPositions,
                                        simulatedEvoModes,
                                        testedEvoModes,
                                        NULL))
for (scenario in scenarioNames){
  for (dist in examinedBasinPositions) {
    for (trueMode in simulatedEvoModes){
      for (resMode in testedEvoModes){
        for (i in 1:noOfTests){
          AkaikeWtArrayStrat[scenario,dist,trueMode,resMode,i]=testResultsStrat[[scenario]][[dist]][[trueMode]][[i]]$testRes$modelFits[resMode,"Akaike.wt"]
        }
      }
    }
  }
}

AkaikeWtArrayTime=array(NA,
                        dim=c(length(scenarioNames),length(noOfSamplingLoc),length(simulatedEvoModes),length(testedEvoModes),noOfTests),
                         dimnames = list(scenarioNames,
                                         noOfSamplingLoc,
                                         simulatedEvoModes,
                                         testedEvoModes,
                                         NULL))
for (scenario in scenarioNames){
  for (nSamp in noOfSamplingLoc) {
    for (trueMode in simulatedEvoModes){
      for (resMode in testedEvoModes){
        for (i in 1:noOfTests){
          AkaikeWtArrayTime[scenario,nSamp,trueMode,resMode,i]=testResultsTime[[scenario]][[nSamp]][[trueMode]][[i]]$testRes$modelFits[resMode,"Akaike.wt"]
        }
      }
    }
  }
}


#### Strat Domain: WHich models have strong support? ####
# checks for which proportion of test runs there is strong support for a model, meaning AICweight > acceptanceTreshold
acceptanceTreshold=0.9
strongSupportPropStrat=array(NA,
                             dim=c(length(scenarioNames),length(examinedBasinPositions),length(simulatedEvoModes),length(testedEvoModes)),
                             dimnames = list(scenarioNames,
                                             examinedBasinPositions,
                                             simulatedEvoModes,
                                             testedEvoModes))

for (scenario in scenarioNames){
  for (dist in examinedBasinPositions) {
    for (trueMode in simulatedEvoModes){
      for (resMode in testedEvoModes){
        strongSupportPropStrat[scenario,dist,trueMode,resMode]=sum(AkaikeWtArrayStrat[scenario,dist,trueMode,resMode,]>acceptanceTreshold)/noOfTests
      }
    }
  }
}

## Which prop of runs provide strong support for the correct mode of evolution?                    
strongSupportPropStrat[,,"stasis","Stasis"]
strongSupportPropStrat[,,"Brownian motion","URW"]
strongSupportPropStrat[,,"weak Brownian drift","GRW"]
strongSupportPropStrat[,,"strong Brownian drift","GRW"]
# answer: None

# in scenario X, what is the proportion of support for mode Y, given Z was simulated?
for (scenario in scenarioNames){
  for (trueMode in simulatedEvoModes){
    print(paste("In scenario ", scenario, " with simulated ", trueMode, ", the following modes are strongly supported (in proportion of test runs):", sep=""))
    print(strongSupportPropStrat[scenario,,trueMode,])
  }
}


#### Strat Domain: Which models have weak support? ####
# check prop of models with weak support, meaning AICweight<ignoranceTreshold so they can be ommited on the ternary diagrams
ignoranceTreshold=0.001
weakSupportPropStrat=array(NA,
                             dim=c(length(scenarioNames),length(examinedBasinPositions),length(simulatedEvoModes),length(testedEvoModes)),
                             dimnames = list(scenarioNames,
                                             examinedBasinPositions,
                                             simulatedEvoModes,
                                             testedEvoModes))

for (scenario in scenarioNames){
  for (dist in examinedBasinPositions) {
    for (trueMode in simulatedEvoModes){
      for (resMode in testedEvoModes){
        weakSupportPropStrat[scenario,dist,trueMode,resMode]=sum(AkaikeWtArrayStrat[scenario,dist,trueMode,resMode,]<ignoranceTreshold)/noOfTests
      }
    }
  }
}

weakSupportPropStrat["A",,"stasis",]
weakSupportPropStrat['B',,"stasis",]
# For Stasis, the GRW axis can be ignored except for scenario A, 12 km offshore

weakSupportPropStrat['A',,"Brownian motion",]
weakSupportPropStrat['B',,"Brownian motion",]
# For BM, the stasis axis can be ignored except for scenario A, 12 km offshore

weakSupportPropStrat['A',,"weak Brownian drift",]
weakSupportPropStrat['B',,"weak Brownian drift",]
# For weak Brownian drift, the stasis axis can be ignored

weakSupportPropStrat['A',,"strong Brownian drift",]
weakSupportPropStrat['B',,"strong Brownian drift",]
# For strong Brownian drift, the stasis axis can be ignored

#### Strat Domain: Ternary plots ####
## stasis
# !WARNING! Ternary plot for scenario A, 12 km offshore under stasis is an approximation bc the GRW coordinate is > 0
# see section "Strat domain: which models have weak support"

dir.create(paste(getwd(),"/stratDomain",sep=""))
{
for (scenario in scenarioNames){
  for (distFromShore in examinedBasinPositions){
    for (modeOfEvo in c("stasis")){
      df=data.frame(OU=AkaikeWtArrayStrat[scenario,distFromShore,modeOfEvo,"OU",],
                    Stasis=AkaikeWtArrayStrat[scenario,distFromShore,modeOfEvo,"Stasis",],
                    URW=AkaikeWtArrayStrat[scenario,distFromShore,modeOfEvo,"URW",])
      
      p=ggtern(data=df, aes(x=OU,y=Stasis, z=URW)) + geom_point() + labs(title=paste("Stratigraphic domain \n scenario ", scenario, " ", distFromShore, " offshore, ", modeOfEvo))
      
      ggsave(filename = paste("stratDomain/0StratDomain",modeOfEvo,"Scenario",scenario,distFromShore,".jpg",sep=""), plot=p )
    }
  }
}

## Brownian motion, weak & strong Brwonian drift
# !WARNING! Ternary plot for scenario A, 12 km offshore under Brownian motion is an approximation bc the Stasis coordinate is > 0
# see section "weak support for models"
for (scenario in scenarioNames){
  for (distFromShore in examinedBasinPositions){
    for (modeOfEvo in  c("Brownian motion","weak Brownian drift","strong Brownian drift")){
      df=data.frame(OU=AkaikeWtArrayStrat[scenario,distFromShore,modeOfEvo,"OU",],
                    GRW=AkaikeWtArrayStrat[scenario,distFromShore,modeOfEvo,"GRW",],
                    URW=AkaikeWtArrayStrat[scenario,distFromShore,modeOfEvo,"URW",])
      
      p=ggtern(data=df, aes(x=OU,y=GRW, z=URW)) + geom_point() + labs(title=paste("Stratigraphic domain \n scenario ", scenario, " ", distFromShore, " offshore, ", modeOfEvo))
      
      ggsave(filename = paste("stratDomain/0StratDomain",modeOfEvo,"Scenario",scenario,distFromShore,".jpg",sep=""), plot=p )
    }
  }
}
}

#### Time Domain: WHich models have strong support? ####
# checks for which proportion of test runs there is strong support for a model, meaning AICweight > acceptanceTreshold
acceptanceTreshold=0.9
strongSupportPropTime=array(NA,
                             dim=c(length(scenarioNames),length(noOfSamplingLoc),length(simulatedEvoModes),length(testedEvoModes)),
                             dimnames = list(scenarioNames,
                                             noOfSamplingLoc,
                                             simulatedEvoModes,
                                             testedEvoModes))

for (scenario in scenarioNames){
  for (nSamp in noOfSamplingLoc) {
    for (trueMode in simulatedEvoModes){
      for (resMode in testedEvoModes){
        strongSupportPropTime[scenario,nSamp,trueMode,resMode]=sum(AkaikeWtArrayTime[scenario,nSamp,trueMode,resMode,]>acceptanceTreshold)/noOfTests
      }
    }
  }
}

## Which prop of runs provide strong support for the correct mode of evolution (in dependence of no of sampling locations)                    
strongSupportPropTime[,,"stasis","Stasis"]
strongSupportPropTime[,,"Brownian motion","URW"]
strongSupportPropTime[,,"weak Brownian drift","GRW"]
strongSupportPropTime[,,"strong Brownian drift","GRW"]


# in scenario X, what is the proportion of support for mode Y, given Z was simulated?
for (scenario in scenarioNames){
  for (trueMode in simulatedEvoModes){
    print(paste("In scenario ", scenario, " with simulated ", trueMode, ", the following modes are strongly supported (in proportion of test runs):", sep=""))
    print(strongSupportPropTime[scenario,,trueMode,])
  }
}


#### Time Domain: Which models have weak support? ####
# check prop of models with weak support, meaning AICweight<ignoranceTreshold so they can be ommited on the ternary diagrams
ignoranceTreshold=0.01
weakSupportPropTime=array(NA,
                           dim=c(length(scenarioNames),length(noOfSamplingLoc),length(simulatedEvoModes),length(testedEvoModes)),
                           dimnames = list(scenarioNames,
                                           noOfSamplingLoc,
                                           simulatedEvoModes,
                                           testedEvoModes))

for (scenario in scenarioNames){
  for (nSamp in noOfSamplingLoc) {
    for (trueMode in simulatedEvoModes){
      for (resMode in testedEvoModes){
        weakSupportPropTime[scenario,nSamp,trueMode,resMode]=sum(AkaikeWtArrayTime[scenario,nSamp,trueMode,resMode,]<ignoranceTreshold)/noOfTests
      }
    }
  }
}

weakSupportPropTime["A",,"stasis",]
weakSupportPropTime['B',,"stasis",]

weakSupportPropTime['A',,"Brownian motion",]
weakSupportPropTime['B',,"Brownian motion",]

weakSupportPropTime['A',,"weak Brownian drift",]
weakSupportPropTime['B',,"weak Brownian drift",]


weakSupportPropTime['A',,"strong Brownian drift",]
weakSupportPropTime['B',,"strong Brownian drift",]


#### Time Domain: Ternary plots ####
## stasis
# !WARNING! Ternary plot are approximations! We need to use 3d ternary plots for this case
dir.create(paste(getwd(),"/timeDomain",sep=""))
for (scenario in scenarioNames){
  for (nSamp in noOfSamplingLoc){
    for (modeOfEvo in c("stasis")){
      df=data.frame(OU=AkaikeWtArrayTime[scenario,nSamp,modeOfEvo,"OU",],
                    Stasis=AkaikeWtArrayTime[scenario,nSamp,modeOfEvo,"Stasis",],
                    URW=AkaikeWtArrayTime[scenario,nSamp,modeOfEvo,"URW",])
      
      p=ggtern(data=df, aes(x=OU,y=Stasis, z=URW)) + geom_point() + labs(title=paste("Time domain \n scenario ", scenario, " ", nSamp, " equidistant sampling points, ", modeOfEvo))
      
      ggsave(filename = paste("timeDomain/1TimeDomain",modeOfEvo,"Scenario",scenario,nSamp,"samplingPoints.jpg",sep=""), plot=p )
    }
  }
}

## Brownian motion, weak & strong Brwonian drift
# !WARNING! Ternary plot for scenario A, 12 km offshore under Brownian motion is an approximation bc the Stasis coordinate is > 0
# see section "weak support for models"
for (scenario in scenarioNames){
  for (nSamp in noOfSamplingLoc){
    for (modeOfEvo in  c("Brownian motion","weak Brownian drift","strong Brownian drift")){
      df=data.frame(OU=AkaikeWtArrayTime[scenario,nSamp,modeOfEvo,"OU",],
                    GRW=AkaikeWtArrayTime[scenario,nSamp,modeOfEvo,"GRW",],
                    URW=AkaikeWtArrayTime[scenario,nSamp,modeOfEvo,"URW",])
      
      p=ggtern(data=df, aes(x=OU,y=GRW, z=URW)) + geom_point() + scale_fill_manual(values=grey(AkaikeWtArrayTime[scenario,nSamp,modeOfEvo,"Stasis",])) + labs(title=paste("Time domain \n scenario ", scenario, " ", nSamp, " equidistant sampling points, ", modeOfEvo))

      ggsave(filename = paste("timeDomain/1TimeDomain",modeOfEvo,"Scenario",scenario,nSamp,"samplingPoints.jpg",sep=""), plot=p )
    }
  }
}




#### Experiments with ternary plots ####
require("Ternary")
par(mar=c(1,1,2,1))
for (scenario in scenarioNames){
  for (nSamp in noOfSamplingLoc){
    for (modeOfEvo in  c("Brownian motion","weak Brownian drift","strong Brownian drift")){
      jpeg(filename = paste("timeDomain/1TimeDomain",modeOfEvo,"Scenario",scenario,nSamp,"samplingPoints.jpg",sep="") )
      TernaryPlot(atip = "OU",
                  btip="GRW",
                  ctip="URW",
                  main=paste("Time domain \n scenario ", scenario, " ", nSamp, " equidistant sampling points, ", modeOfEvo))
      df=data.frame(OU=AkaikeWtArrayTime[scenario,nSamp,modeOfEvo,"OU",],
                    GRW=AkaikeWtArrayTime[scenario,nSamp,modeOfEvo,"GRW",],
                    URW=AkaikeWtArrayTime[scenario,nSamp,modeOfEvo,"URW",])
      TernaryPoints(df,col=grey(AkaikeWtArrayTime[scenario,nSamp,modeOfEvo,"Stasis",]),pch=16)
      dev.off()
      
      #p=ggtern(data=df, aes(x=OU,y=GRW, z=URW)) + geom_point() + scale_fill_manual(values=grey(AkaikeWtArrayTime[scenario,nSamp,modeOfEvo,"Stasis",])) + labs(title=paste("Time domain \n scenario ", scenario, " ", nSamp, " equidistant sampling points, ", modeOfEvo))
      
      #ggsave(filename = paste("1TimeDomain",modeOfEvo,"Scenario",scenario,nSamp,"samplingPoints.jpg",sep=""), plot=p )
    }
  }
}
for (scenario in scenarioNames){
  for (nSamp in noOfSamplingLoc){
    for (modeOfEvo in c("stasis")){
      df=data.frame(OU=AkaikeWtArrayTime[scenario,nSamp,modeOfEvo,"OU",],
                    Stasis=AkaikeWtArrayTime[scenario,nSamp,modeOfEvo,"Stasis",],
                    URW=AkaikeWtArrayTime[scenario,nSamp,modeOfEvo,"URW",])
      jpeg(filename = paste("timeDomain/1TimeDomain",modeOfEvo,"Scenario",scenario,nSamp,"samplingPoints.jpg",sep="") )
      TernaryPlot(atip = "OU",
                  btip="Stasis",
                  ctip="URW",
                  main=paste("Time domain \n scenario ", scenario, " ", nSamp, " equidistant sampling points, ", modeOfEvo))
      TernaryPoints(df,col=grey(AkaikeWtArrayTime[scenario,nSamp,modeOfEvo,"GRW",]),pch=16)
      dev.off()
      }
  }
}


#### Plot Mean AICweights ####
modeCol=c("GRW"="red","URW"="blue","Stasis"=grey(0.5),"OU"="black")
modeLwd=c("GRW"=4,"URW"=4,"Stasis"=4,"OU"=4)
modeLty=c("GRW"=1,"URW"=1,"Stasis"=1,"OU"=1)
for (scenario in scenarioNames){
  for (simMode in simulatedEvoModes){
    png(filename=paste("Simulated", simMode, "TimeDomainScenario",scenario,".png",sep=""))
    plot(NULL,
         xlim=c(1,length(noOfSamplingLoc)),
         ylim=c(0,1),
         ylab="mean AICweight",
         xlab="",
         main=paste("Simulated", simMode, "\nTimeScenario",scenario),
         xaxt="n")
    axis(side=1,
         at=c(1:length(noOfSamplingLoc)),
          labels=noOfSamplingLoc)
    mtext("Time Series Length",side=1, line=2)
    yval=rep(NA,length(noOfSamplingLoc))
    for (retMode in testedEvoModes){
      meanAICwt=c()
      for (nsp in 1:length(noOfSamplingLoc) ){
        meanAICwt[nsp]=mean(AkaikeWtArrayTime[scenario,noOfSamplingLoc[nsp],simMode,retMode,])
      }
      lines(x=seq(1,length(noOfSamplingLoc)),
             y=meanAICwt,
            col=modeCol[retMode],
            lwd=modeLwd[retMode],
            lty=modeLty[retMode])
    }
    legend("topright",
           legend=testedEvoModes,
           lwd = modeLwd,
           col=modeCol,
           lty=modeLty)
    dev.off()
  }
}

for (scenario in scenarioNames){
  for (simMode in simulatedEvoModes){
    png(filename=paste("Simulated", simMode, "StratScenario",scenario,".png",sep=""))
    plot(NULL,
         xlim=c(1,length(examinedBasinPositions)),
         ylim=c(0,1),
         ylab="mean AICweight",
         xlab="",
         main=paste("Simulated", simMode, "\nScenario",scenario),
         xaxt="n")
    axis(side=1,
         at=c(1:length(examinedBasinPositions)),
         labels=examinedBasinPositions)
    mtext("Distance from Shore",side=1, line=2)
    yval=rep(NA,length(examinedBasinPositions))
    for (retMode in testedEvoModes){
      meanAICwt=c()
      for (nsp in 1:length(examinedBasinPositions) ){
        meanAICwt[nsp]=mean(AkaikeWtArrayStrat[scenario,examinedBasinPositions[nsp],simMode,retMode,])
      }
      lines(x=seq(1,length(examinedBasinPositions)),
            y=meanAICwt,
            col=modeCol[retMode],
            lwd=modeLwd[retMode],
            lty=modeLty[retMode])
    }
    legend("topright",
           legend=testedEvoModes,
           lwd = modeLwd,
           col=modeCol,
           lty=modeLty)
    dev.off()
  }
}

#### Grouped Boxplots of AICweights ####
simulatedEvoMode=simulatedEvoModes[1]
scenario=scenarioNames[1]
dirName="stratDomainBoxPlots"
dir.create(dirName)
for (scenario in scenarioNames){
  for (simulatedEvoMode in simulatedEvoModes){
    df=data.frame(position=NULL,testedMode=NULL,AIC=NULL)
    for (pos in examinedBasinPositions){
      for (testedEvoMode in testedEvoModes){
        df=rbind(df,data.frame(position=as.factor(rep(pos,length(AkaikeWtArrayStrat[1,1,1,1,]))),
                               testedEvoMode=rep(testedEvoMode,length(AkaikeWtArrayStrat[1,1,1,1,])),
                               AIC=AkaikeWtArrayStrat[scenario,pos,simulatedEvoMode,testedEvoMode,]))
        
      }
    }
    p=ggplot(df, aes(x=position, y=AIC, fill=testedEvoMode)) + 
      geom_boxplot(outlier.shape = NA) +
      labs(title = paste("Scenario ", scenario, ", simulated: ", simulatedEvoMode,sep=""), y="AIC weight")
    fileName=paste("StratDomainScenario", scenario, "simulated", simulatedEvoMode,".png")
    ggsave(filename = paste(dirName,"/",fileName,sep=""))
  }
}

dirName="timeDomainBoxPlots"
dir.create(dirName)
for (scenario in scenarioNames){
  for (simulatedEvoMode in simulatedEvoModes){
    df=data.frame(nsp=NULL,testedMode=NULL,AIC=NULL)
    for (nsp in noOfSamplingLoc){
      for (testedEvoMode in testedEvoModes){
        df=rbind(df,data.frame(nsp=as.factor(rep(nsp,length(AkaikeWtArrayTime[1,1,1,1,]))),
                               testedEvoMode=rep(testedEvoMode,length(AkaikeWtArrayTime[1,1,1,1,])),
                               AIC=AkaikeWtArrayTime[scenario,nsp,simulatedEvoMode,testedEvoMode,]))
        
      }
    }
    p=ggplot(df, aes(x=nsp, y=AIC, fill=testedEvoMode)) + 
      geom_boxplot(outlier.shape = NA) +
      labs(title = paste("Scenario ", scenario, ", simulated: ", simulatedEvoMode,sep=""), y="AIC weight")
    fileName=paste("TimeDomainScenario", scenario, "simulated", simulatedEvoMode,".png")
    ggsave(filename = paste(dirName,"/",fileName,sep=""))
  }
}

####

# require(klaR)
# 
# df=data.frame(OU=AkaikeWtArrayTime[scenario,nSamp,modeOfEvo,"OU",],
#               GRW=AkaikeWtArrayTime[scenario,nSamp,modeOfEvo,"GRW",],
#               URW=AkaikeWtArrayTime[scenario,nSamp,modeOfEvo,"URW",],
#               Stasis=AkaikeWtArrayTime[scenario,nSamp,modeOfEvo,"Stasis",])
# nSamp="25"
# modeOfEvo="Brownian motion"
# quadplot(AkaikeWtArrayTime[scenario,nSamp,modeOfEvo,"OU",],
#           AkaikeWtArrayTime[scenario,nSamp,modeOfEvo,"GRW",],
#           AkaikeWtArrayTime[scenario,nSamp,modeOfEvo,"URW",],
#            AkaikeWtArrayTime[scenario,nSamp,modeOfEvo,"Stasis",])


#### Create Tables ####
dir.create(paste(getwd(),"/Tables",sep=""))
## Table 1: Strong support in strat domain 
scenario="A"
sceTabA= cbind(strongSupportPropStrat[scenario,"2 km",,],strongSupportPropStrat[scenario,"6 km",,],strongSupportPropStrat[scenario,"8 km",,],strongSupportPropStrat[scenario,"10 km",,],strongSupportPropStrat[scenario,"12 km",,])
scenario="B"
sceTabB= cbind(strongSupportPropStrat[scenario,"2 km",,],strongSupportPropStrat[scenario,"6 km",,],strongSupportPropStrat[scenario,"8 km",,],strongSupportPropStrat[scenario,"10 km",,],strongSupportPropStrat[scenario,"12 km",,])
sceTab=rbind(sceTabA,sceTabB)

write.csv(100 * sceTab,file = "Tables/Table1_Raw.csv")

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
write.csv(100 * sceTabTA,file = "Tables/Table2_Raw.csv")

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
write.csv(100 * sceTabTB,file = "Tables/Supp_Table1_Raw.csv")
