#clears every plot and existing variable
graphics.off()
rm(list = ls())


source("C:/Users/Johanson Onyegbula/Documents/Masters in NRES/Fall 2021/CS 790K/Assignments/TSP/Population.R")


initializeGA <- function(){
  gen <<- 0
  parents <- initializePopulation()
  parentFitness <- evaluatePopFitness(parents)
  parentStats <- populationStats(parents, gen)
  parentReport <- populationReport(parentStats)
  
  return(list(parents, parentFitness, parentStats))
}


runGA <- function(){
  set.seed(randomSeed)
  Parents <<- initializeGA()
  for (i in 1:maxGeneration){
    XChildren <- PMXover2Pt(Parents[[1]], probCrossover, Parents[[2]], Parents[[3]])
    MutateChildren <<- invertMutate(XChildren, probMutation)
    offspringFitness <- evaluatePopFitness(MutateChildren)
    chosenChildren <<- CHCSelection(Parents[[1]], MutateChildren, Parents[[2]], offspringFitness)
    gen <<- gen + 1
    childrenFitness <- evaluatePopFitness(chosenChildren)
    childrenStats <- populationStats(chosenChildren, gen)
    childrenReport <- populationReport(childrenStats)
    
    Parents <<- list(chosenChildren, childrenFitness, childrenStats)
  }
  writeReport(report)
}


GAgridSearch <- function(pops = c(500, 1000), xovers = seq(0.95, 1, 0.01), mutations = seq(0.05, 0.1, 0.01)){
  checkParams <- data.frame(Generations = 0, Population = 0, Crossover = 0, Mutation = 0,
                            AvgFitness = 0, MaxFitness = 0)
  counter <- 1
  for (j in pops){
    populationSize <<- j
    i <- j*1.5
    maxGeneration <<- j
    for (k in xovers){
      probCrossover <<- k
      for (m in mutations){
        probMutation <<- m
        runGA()
        checkParams[counter,] <<- data.frame(Generations = i, Population = j, Crossover = k, Mutation = m,
                                AvgFitness = report$Mean[which.max(report$Mean)], MaxFitness = report$Max)
        counter <- counter + 1
      }
    }
  }
  # prints parameters that maximizes the average fitness (maximum fitness would be stable much earlier)
  print(checkParams[which.max(checkParams$AvgFitness),])
  print(checkParams[which.max(checkParams$MaxFitness),])
}


multipleGARuns <- function(numSeeds = 30){
  checkParams <- data.frame(Generation = 0, Minimum = 0, Mean = 0, Maximum = 0, Sum = 0, Population = 0, Crossover = 0, Mutation = 0)
  for (i in 1:numSeeds){
    randomSeed <<- i
    runGA()
    startI <- 1
    endI <- dim(report)[1]
    if (i > 1){
      checkParams[c(startI:endI), c(2:5)] <- checkParams[c(startI:endI), c(2:5)] + report[,c(2:5)]/numSeeds
    } else{
      checkParams[c(startI:endI),] <- report
      checkParams[c(startI:endI), c(2:5)] <- report[, c(2:5)]/numSeeds
    }
  }
  # runs GA over different seeds and presents the data in csv and graphical format
  checkParams <<- checkParams[!is.na(checkParams$Minimum), ]
  writeReport(checkParams)
  par(mfrow = c(2,2))
  plot(checkParams$Mean ~ checkParams$Generation, main = "Average Fitnesses", xlab = "Generation", ylab = "Average Fitness", pch=19, col="Purple")
  plot(checkParams$Maximum ~ checkParams$Generation, main = "Maximum Fitnesses", xlab = "Generation", ylab = "Maximum Fitness", pch=19, col="Red")
  plot(maxImposFitness - checkParams$Mean ~ checkParams$Generation, main = "Average Objective Function", xlab = "Generation", ylab = "Average Tour Length", pch=19, col="Blue")
  plot(maxImposFitness - checkParams$Maximum ~ checkParams$Generation, main = "Maximum Objective Function", xlab = "Generation", ylab = "Maximum Tour Length Function", pch=19, col="Red")
  par(mfrow = c(1,1))
}


runGA()
#GAgridSearch()
#multipleGARuns()
