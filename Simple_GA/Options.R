chromosomeLength <- 36    #Dejong1 = 36  #Dejong2 = 270  #Dejong3 = 55  #Dejong4 =26  #Dejong5 = 33
populationSize <- 100
maxGeneration <- 1000
probCrossover <- 0.9
probMutation <- 0.001
randomSeed <- 200

gen <- 0
infile <- ""
outfile <- "GA_report.csv"
mydir <- "C:/Users/Johanson Onyegbula/Documents/Masters in NRES/Fall 2021/CS 790K/Assignments"
setwd(mydir)

fitness <- 0
objective <- 0

report <- data.frame(Generation = 0, Minimum = 0, Mean = 0, Maximum = 0, Sum = 0, Population = 0, Crossover = 0, Mutation = 0)