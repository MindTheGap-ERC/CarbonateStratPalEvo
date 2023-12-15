## Visualize Data ##

#### Load R outputs ####
load("data/R_outputs/results_modes_of_evolution.Rdata")
load("data/R_outputs/ageDepthModelsScenariosAandB.Rdata")
load("data/R_outputs/parameters_for_tests.Rdata")

#### Age-Depth Models ####
scenario = "A"
dist = "1 km"

stopifnot(scenario %in% scenarioNames)
stopifnot( dist %in% all_dist )
plot(x = ageDepthModels[[scenario]][[dist]]$time,
     y = ageDepthModels[[scenario]][[dist]]$height,
     xlab = "Time [Myr]",
     ylab = "Height [m]",
     main = paste0("Age-Depth Model, ", dist, " from shore, scenario ", scenario, sep = ""),
     type = "l")

#### Simulated lineages sampled in strat. domain ####
scenario = "A"
dist = "2 km"
evo_mode = "Brownian motion"
run_no = 10

stopifnot(scenario %in% scenarioNames)
stopifnot(dist %in% examinedBasinPositions)
stopifnot(evo_mode %in% simulatedEvoModes)
stopifnot(run_no <= noOfTests)

## plot strat expression of trait evolution
plot(x = testResultsStrat[[scenario]][[dist]][[evo_mode]][[run_no]]$inputData$height,
     y = testResultsStrat[[scenario]][[dist]][[evo_mode]][[run_no]]$inputData$traitValue,
     type = "l",
     xlab = "Height [m]",
     ylab = "Trait Value",
     main = paste(evo_mode, "scenario", scenario, dist, "offshore ; run no. ", run_no))

## plot time expression of trait evolution
plot(x = testResultsStrat[[scenario]][[dist]][[evo_mode]][[run_no]]$inputData$time,
     y = testResultsStrat[[scenario]][[dist]][[evo_mode]][[run_no]]$inputData$traitValue,
     type = "l",
     xlab = "Time [Myr]",
     ylab = "Trait Value",
     main = paste(evo_mode, "scenario", scenario, dist, "offshore ; run no. ", run_no))

## plot paleoTS object derived from trait evolution in the section
paleoTS:::plot.paleoTS(testResultsStrat[[scenario]][[dist]][[evo_mode]][[run_no]]$paleots)

#### Simulated lineages sampled only in the time domain ####
scenario = "A"
no_of_loc = "100"
evo_mode = "Brownian motion"
run_no = 100

stopifnot(scenario %in% scenarioNames)
stopifnot(dist %in% noOfSamplingLoc)
stopifnot(evo_mode %in% simulatedEvoModes)
stopifnot(run_no <= noOfTests)

## Trait simulations
plot(x = testResultsTime[[scenario]][[no_of_loc]][[evo_mode]][[run_no]]$inputData$time,
     y = testResultsTime[[scenario]][[no_of_loc]][[evo_mode]][[run_no]]$inputData$traitValue,
     xlab = "Time [Myr]",
     ylab = "Trait Value",
     main = paste0(evo_mode, "scenario", scenario, no_of_loc, "sampling locations, run no. ", run_no),
     type = "l")
## paleoTS objects
paleoTS:::plot.paleoTS(testResultsTime[[scenario]][[no_of_loc]][[evo_mode]][[run_no]]$paleots)
