#clear every plot and existing variable
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
  status <- sprintf("Generations = %d, Population = %d, Crossover = %.2f, Mutation = %.4f, AvgFitness = %.5f",
                    gen, populationSize, probCrossover, probMutation, report$Mean[length(report$Mean)])
  print(status)
}


checkParams <- data.frame(Generations = 0, Population = 0, Crossover = 0, Mutation = 0, AvgFitness = 0, MaxFitness = 0)
counter <- 1
for (i in seq(100, 1000, 100)){
  maxGeneration <- i
  for (j in seq(50,500,50)){
    populationSize <- j
    for (k in seq(1, 0.7, -0.1)){
      probCrossover <- k
      for (m in c(0.001,0.01,0.05,0.1,0.5)){
        probMutation <- m
        runGA()
        checkParams[counter,] <- data.frame(Generations = i, Population = j, Crossover = k, Mutation = m,
                                AvgFitness = report$Mean, MaxFitness = report$Max)
        counter <- counter + 1
      }
    }
  }
}
print(checkParams[which.max(checkParams$AvgFitness),])

