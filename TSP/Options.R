populationSize <- 1500
maxGeneration <- populationSize*1.5
probCrossover <- 0.98
probMutation <- 0.09
randomSeed <- 5


gen <- 0
infile <- "berlin52.txt"
outfile <- "berlin52_GA_report.csv"
mydir <- "C:/Users/Johanson Onyegbula/Documents/Masters in NRES/Fall 2021/CS 790K/Assignments/TSP"
setwd(mydir)


dataSource <- read.table(infile)
chromosomeLength <- dim(dataSource)[1]


dist.mat <- matrix(nrow = chromosomeLength, ncol = chromosomeLength)
for (i in 1:chromosomeLength){
  for (j in 1:chromosomeLength){
    if (!is.na(dist.mat[i,j]) ){
      dist.mat[j,i] <- dist.mat[i,j]
    }
    dist.mat[i,j] <- round(sqrt(sum((dataSource[i,2:3] - dataSource[j,2:3])^2)))
  }
}


PopFitness <- 0
maxImposFitness <- sum( rev(sort(dist.mat))[1:chromosomeLength] )
selectionPressure <- 2


report <- data.frame(Generation = 0, MinimumFitness = 0, MeanFitness = 0, MaximumFitness = 0,
                    SumofFitness = 0, PopulationSize = 0, Crossover = 0, Mutation = 0)
