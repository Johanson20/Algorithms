#clears every plot and existing variable
graphics.off()
rm(list = ls())


source("C:/Users/Johanson Onyegbula/Documents/Masters in NRES/Fall 2021/CS 790K/Assignments/Population.R")


initializeGA <- function(){
  gen <<- 0
  parents <- initializePopulation()
  parentFitness <- evaluatePop(parents)
  parentStats <- populationStats(parents, gen)
  parentReport <- populationReport(parentStats)
  
  return(list(parents, parentFitness, parentStats))
}


runGA <- function(){
  set.seed(randomSeed)
  Parents <- initializeGA()
  for (i in 1:maxGeneration){
    XChildren <- xover1Pt(Parents[[1]], probCrossover, Parents[[2]], Parents[[3]])
    MutateChildren <- mutate(XChildren, probMutation)
    gen <<- gen + 1
    childrenFitness <- evaluatePop(MutateChildren)
    childrenStats <- populationStats(MutateChildren, gen)
    childrenReport <- populationReport(childrenStats)
    
    Parents <- list(MutateChildren, childrenFitness, childrenStats)
  }
  writeReport(report)
}


GAgridSearch <- function(gens = seq(100, 500, 100), pops = seq(50,200,50),
                         xovers = seq(1, 0.7, -0.1), mutations = c(0.001,0.01,0.1)){
  checkParams <- data.frame(Generations = 0, Population = 0, Crossover = 0, Mutation = 0, AvgFitness = 0, MaxFitness = 0)
  counter <- 1
  for (i in gens){
    maxGeneration <- i
    for (j in pops){
      populationSize <- j
      for (k in xovers){
        probCrossover <- k
        for (m in mutations){
          probMutation <- m
          runGA()
          checkParams[counter,] <- data.frame(Generations = i, Population = j, Crossover = k, Mutation = m,
                                  AvgFitness = report$Mean[which.max(report$Mean)], MaxFitness = report$Max)
          counter <- counter + 1
        }
      }
    }
  }
  # prints parameters that maximizes the average fitness (maximum fitness would be stable much earlier)
  print(checkParams[which.max(checkParams$AvgFitness),])
}


multipleGARuns <- function(numSeeds = 30){
  checkParams <- data.frame(Generation = 0, Minimum = 0, Mean = 0, Maximum = 0, Sum = 0, Population = 0, Crossover = 0, Mutation = 0)
  for (i in 1:numSeeds){
    randomSeed <- i+randomSeed
    runGA()
    index <- dim(report[1])
    startI <- 1+index*(i-1)
    endI <- i*index
    if (i > 1){
      checkParams[c(startI:endI), c(2:5)] <- checkParams[c(startI:endI), c(2:5)] + report[,c(2:5)]/numSeeds
    } else{
      checkParams[c(startI:endI),] <- report
      checkParams[c(startI:endI), c(2:5)] <- report[, c(2:5)]/numSeeds
    }
  }
  # runs GA over different seeds and presents the data in csv and graphical format
  writeReport(checkParams[!is.na(checkParams$Minimum),])
  par(mfrow = c(3,1))
  plot(checkParams$Minimum ~ checkParams$Generation, main = "Minimum Fitnesses", xlab = "Generation", ylab = "Minimum Fitness", pch=19, col="Blue")
  plot(checkParams$Mean ~ checkParams$Generation, main = "Average Fitnesses", xlab = "Generation", ylab = "Average Fitness", pch=19, col="Purple")
  plot(checkParams$Maximum ~ checkParams$Generation, main = "Maximum Fitnesses", xlab = "Generation", ylab = "Maximum Fitness", pch=19, col="Red")
  par(mfrow = c(1,1))
}


runGA()
#GAgridSearch()
#multipleGARuns()
