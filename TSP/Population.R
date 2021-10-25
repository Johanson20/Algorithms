source("C:/Users/Johanson Onyegbula/Documents/Masters in NRES/Fall 2021/CS 790K/Assignments/TSP/Individual.R")
source("C:/Users/Johanson Onyegbula/Documents/Masters in NRES/Fall 2021/CS 790K/Assignments/TSP/Eval.R")


initializePopulation <- function(popSize = populationSize, chromLen = chromosomeLength){
  population <- matrix(ncol = chromLen, nrow = popSize)
  for (i in 1:popSize){
    population[i,] <- initializeIndividual(chromLen)
  }
  startGA <- sprintf("%s %11s %11s %11s %11s", "Generation", "Minimum", "Mean", "Maximum", "Sum")
  print(startGA)
  return(population)
}


evaluatePopFitness <- function(population, popSize = populationSize){
  popFitness <- matrix(nrow = popSize)
  for (i in 1:popSize){
    popFitness[i,] <- evaluateTSP(population[i,])
  }
  return(popFitness)
}


PMXover1Pt <- function(population, pXover = probCrossover, popFitness, popStats){
  popSize <- dim(population)
  newParents <- matrix(ncol = popSize[2], nrow = popSize[1])
  
  for (i in seq(1, popSize[1], 2)){
    parents <- nextParents(population, popFitness, popStats)
    parent1 <- parents[1,]
    parent2 <- parents[2,]
    child1 <- parent1
    child2 <- parent2
    parentSize <- length(parent1)
    if (flip(pXover)){
      xp <- sample(1:parentSize, 1)
      subBits <- seq(xp, parentSize)
      child1[subBits] <- parent2[subBits]
      child2[subBits] <- parent1[subBits]
      child1 <- adjustTSPXover(parent1, child1, subBits)
      child2 <- adjustTSPXover(parent2, child2, subBits)
    }
    newParents[i, ] <- child1
    newParents[i+1, ] <- child2
  }
  return(newParents)
}


PMXover2Pt <- function(population, pXover, popFitness, popStats){
  popSize <- dim(population)
  newParents <- matrix(ncol = popSize[2], nrow = popSize[1])
  
  for (i in seq(1, popSize[1], 2)){
    parents <- nextParents(population, popFitness, popStats)
    parent1 <- parents[1,]
    parent2 <- parents[2,]
    child1 <- parent1
    child2 <- parent2
    if (flip(pXover)){
      xp <- sample(1:chromosomeLength, 2, replace = F)
      subBits <- seq(xp[1], xp[2])
      child1[subBits] <- parent2[subBits]
      child2[subBits] <- parent1[subBits]
      child1 <- adjustTSPXover(parent1, child1, subBits)
      child2 <- adjustTSPXover(parent2, child2, subBits)
    }
    newParents[i, ] <- child1
    newParents[i+1, ] <- child2
  }
  return(newParents)
}


adjustTSPXover <- function(parent, child, subBits){
  for (i in subBits){
    element <- child[i]
    if (length(which(child[-subBits] == element)) > 0){
      disjointBitId <- which(!(parent %in% child))[1]
      redunId <- which(child[-subBits] == element)
      child[-subBits][redunId] <- parent[disjointBitId]
    }
  }
  return(child)
}


populationStats <- function(population, gen){
  PopFitness <<- evaluatePopFitness(population)
  PopMean <- mean(PopFitness)
  PopMin <- min(PopFitness)
  PopMax <- max(PopFitness)
  PopSum <- sum(PopFitness)
  PopStats <<- data.frame(Gen = gen, Min = PopMin, Mean = PopMean, Max = PopMax, Sum = PopSum, 
                          Population = populationSize, Crossover = probCrossover, Mutation = probMutation)
  return(PopStats)
}


CHCSelection <- function(parents, children, parentFitness, childrenFitness, popSize = populationSize){
  allPotentParents <- rbind(parents, children)
  allFitnesses <- rbind(parentFitness, childrenFitness)
  chcIds <- match(rev(sort(allFitnesses)), allFitnesses)
  chosenNextGen <- allPotentParents[chcIds[1:popSize], ]
  return(chosenNextGen)
}


populationReport <- function(popStats){
  report[gen+1, ] <<- popStats[1,]
  popStats[2:4] <- maxImposFitness - popStats[2:4]
  output <- sprintf("%6d %15.0f %11.0f %11.0f %11.0f", popStats$Gen, popStats$Min,
                    popStats$Mean, popStats$Max, popStats$Sum)
  print(output)
}


writeReport <- function(report = report, file = outfile){
  write.csv(report, file = outfile, row.names = FALSE)
  
  optId <- which.max(PopFitness)
  optChild <- Parents[[1]][optId, ]
  optDist <- maxImposFitness - evaluateTSP(optChild)
  finalResult <- sprintf("\nOptimal Solution for TSP is: %.0f\n\nOptimal Chromosome is:\n", optDist)
  cat(finalResult)
  print(optChild)
}


findCoefficient <- function(popStats, scaleFactor = selectionPressure){
  popMin <- popStats$Min
  popMax <- popStats$Max
  popAvg <- popStats$Mean
  if (popMin > (scaleFactor*popAvg - popMax)/(scaleFactor - 1) ){
    d <- popMax - popAvg
    constA <- (scaleFactor - 1) * popAvg/d
    constB <- popAvg * (popMax - (scaleFactor*popAvg))/d
  } else {
    d <- popAvg - popMin
    constA <- popAvg/d
    constB <- -popMin * popAvg/d
  }
  return(c(constA, constB))
}


linearScaling <- function(popFitness, coeffs){
  scaledFitness <- coeffs[1]*popFitness + coeffs[2]
  return(scaledFitness)
}


proportionalSelector <- function(popFitness, popStats){
  tempFitness <- ceiling(rank(popFitness))
  parentSize <- length(tempFitness)
  selectionProb <- tempFitness/sum(tempFitness)
  chosenIndex <- sample(c(1:parentSize), 1, prob = selectionProb)
  return(chosenIndex)
}


nextParents <- function(population, popFitness, popStats){
  parents <- matrix(nrow = 2, ncol = dim(population)[2])
  index1 <- proportionalSelector(popFitness, popStats)
  index2 <- proportionalSelector(popFitness, popStats)
  parents[1,] <- population[index1, ]
  parents[2,] <- population[index2, ]
  return(parents)
}
