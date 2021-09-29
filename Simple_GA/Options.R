chromosomeLength <- 33    #36 #270 #55 #26 #33
populationSize <- 10
maxGeneration <- 150
probCrossover <- 0.9
probMutation <- 0.001
randomSeed <- 200
chcLambda <- 2

gen <- 0
infile <- ""
outfile <- "GA_report.csv"
mydir <- "C:/Users/Johanson Onyegbula/Documents/Masters in NRES/Fall 2021/CS 790K/Assignments"
setwd(mydir)

fitness <- 0
objective <- 0

report <- data.frame(Generation = 0, Minimum = 0, Mean = 0, Maximum = 0, Sum = 0, Population = 0, Crossover = 0, Mutation = 0)