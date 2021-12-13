#clears every plot and existing variable
graphics.off()
rm(list = ls())


# load in relevant libraries (must be pre-installed)
library(lidR)
library(lidRplugins)
library(rgdal)
library(GA)
library(rgeos)
library(logr)


chromosomeLength <- 132    # Watershed, Dalponte2016, Silva2016, Li2012, Ptrees, Hamraz2016
populationSize <- 200
maxGeneration <- populationSize * 1.5
probCrossover <- 0.95
probMutation <- 0.05
randomSeed <- 77
startIndex <- 1 
grid_canopyAlgo <- c('p2r', 'dsmtin', 'pitfree')

outfile <- "../output/GA_report.csv"
logfile <- "../output/test.log"
mydir <- "C:/Users/Johanson Onyegbula/Documents/Masters in NRES/Fall 2021/CS 790K/Project/code"
setwd(mydir)
options(digits = 10)
maxfitness <- 1000

Las <- dir(path = "../data/als_data", pattern = "*.las")
Shp <- dir(path = "../data/ground", pattern = "*.shp")
noPlots <- length(Las)
groundTruth <- readOGR(paste0("../data/ground/", Shp[1]), verbose = F)
las <- readLAS(paste0("../data/als_data/", Las[1]), select = "x")
aa <- terra::intersect(groundTruth, las)
ab <- aa@bbox
poi <- paste("-inside", ab[1,1], ab[2,1], ab[1,2], ab[2,2])
groundTruth <- subset(groundTruth, X_Location >= ab[1,1] & X_Location <= ab[1,2] & 
                        Y_Location >= ab[2,1] & Y_Location <= ab[2,2])
actualTrees <- dim(groundTruth)[1]
las <- readLAS(paste0("../data/als_data/", Las[1]), select = "xyzr", filter = poi)

# This is a chromosome encoded with default values for all parameters
binaryVals <- c(0,1,0,0,0,1,0,0,0,0,0,0,1,1,0,1,1,1,0,1,0,0,1,0,1,1,0,0,1,0,0,
                0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,0,0,0,1,0,1,0,0,1,0,1,1,1,0,1,0,1,1,
                0,1,0,1,0,1,0,1,0,1,0,1,0,1,1,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,0,
                1,0,0,0,0,0,0,0,1,1,1,0,1,0,0,0,0,0,1,1,0,0,0,1,1,0,0,0,0,1,1)

allShp <- list()
for (i in 1:noPlots){
  allShp[[i]] <- readOGR(paste0("../data/ground/", Shp[i]), verbose = F)
}


# This is the integrated (and computationally expensive) fitness evaluation function
evaluateTreeSegmentation <- function(chromosome){
  tryCatch({
    log_print(chromosome, console = F)
    # watershed (some apply to others too)
    evaluateAlgorithmValues(chromosome)
    log_print("Watershed about to run...", console = F)
    chm <- grid_canopy(las, reso, algoVal)
    kern <- matrix(1, ker, ker)
    chm <- raster::focal(chm, w = kern, fun = mean, na.rm = TRUE)	
    tree.las <- segment_trees(las, lidR::watershed(chm, th_tree, tol, exte))
    waterShedTrees <<- length(na.omit(unique(tree.las@data$treeID)))
    waterShedFitness <<- maxfitness - evaluateFit(tree.las, waterShedTrees)
    
    # dalponte2016 (some apply to silva)
    log_print("Dalponte2016 about to run...", console = F)
    treetops <- find_trees(chm, lmf(ws, hmin))
    tree1.las   <- segment_trees(las, dalponte2016(chm, treetops, th_tree, th_seed, th_cr, max_cr))
    dalponteTrees <<- length(na.omit(unique(tree1.las@data$treeID)))
    dalponteFitness <<- maxfitness - evaluateFit(tree1.las, dalponteTrees)
    
    # silva2016
    log_print("Silva2016 about to run...", console = F)
    tree2.las <- segment_trees(las, silva2016(chm, treetops, max_cr_factor, exclu))
    silvaTrees <<- length(na.omit(unique(tree2.las@data$treeID)))
    silvaFitness <<- maxfitness - evaluateFit(tree2.las, silvaTrees)
    
    # li2012
    log_print("Li2012 about to run...", console = F)
    tree3.las <- segment_trees(las, li2012(dt1, dt2, R, Zu, hmin))
    li2012Trees <<- length(na.omit(unique(tree3.las@data$treeID)))
    li2012Fitness <<- maxfitness - evaluateFit(tree3.las, li2012Trees)
    
    # ptrees
    log_print("Ptrees about to run...", console = F)
    k <- c(k1, k2) 
    tree4.las <- segment_trees(las, ptrees(k))
    PTrees <<- length(na.omit(unique(tree4.las@data$treeID)))
    ptreesFitness <<- maxfitness - evaluateFit(tree4.las, PTrees)
    
    # hamraz
    log_print("Hamraz2016 about to run...", console = F)
    tree5.las <- segment_trees(las, hamraz2016(nps, th, MDCW, epsilon, CLc, Oc, CLs, Os, gap_sens))
    hamrazTrees <<- length(na.omit(unique(tree5.las@data$treeID)))
    hamrazFitness <<- maxfitness - evaluateFit(tree5.las, hamrazTrees)
    
    totalFitness <<- (waterShedFitness + dalponteFitness + silvaFitness + li2012Fitness
                      + ptreesFitness + hamrazFitness)
  },
  error = function(e) {
    totalFitness <<- 600
    log_print(e, console = F)
  }
  )
  return(totalFitness)
}


# This function evaluates a subset of a chromosome to its representative value
# given its precision and range 
evaluateValue <- function(chromosome, minimum, maximum, nbits, start = startIndex){
  chromBits <- chromosome[start : (nbits+start-1)]
  startIndex <<- startIndex + nbits
  x <- binary2decimal(chromBits)
  precision <- (maximum - minimum)/(2^nbits - 1)
  value <- minimum + precision * x
  return(value)
}


# This function decodes part of a chromosome into decimal values 
# -- not necessarily its representative value
evaluateDecimal <- function(chromosome, nbits, start = startIndex){
  chromBits <- chromosome[start : (nbits+start-1)]
  startIndex <<- startIndex + nbits
  value <- binary2decimal(chromBits)
  return(value)
}


# This function decodes a chromosome into the representative values of all
# parameters -- it calls some functions above it
evaluateAlgorithmValues <- function(chromosome){
  ker <<- 2 * (evaluateDecimal(chromosome, 2) + 1) - 1
  algo <<- evaluateDecimal(chromosome, 2) + 1
  th_tree <<- evaluateDecimal(chromosome, 2) + 1
  tol <<- evaluateDecimal(chromosome, 2) + 1
  exte <<- evaluateDecimal(chromosome, 2) + 1
  ws <<- evaluateDecimal(chromosome, 4) + 1
  hmin <<- evaluateDecimal(chromosome, 2) + 1
  k1 <<- evaluateDecimal(chromosome, 5) + 4
  k2 <<- evaluateDecimal(chromosome, 5) + 4
  gap_sens <<- evaluateDecimal(chromosome, 5) + 2
  
  reso <<- evaluateValue(chromosome, 0.1, 3, 5)
  subc <<- evaluateValue(chromosome, 0, 3, 5)
  maxedge2 <<- evaluateValue(chromosome, 0, 7, 7)
  th_cr <<- evaluateValue(chromosome, 0, 1, 5)
  max_cr <<- evaluateValue(chromosome, 1, 30, 5)
  th_seed <<- evaluateValue(chromosome, 0, 1, 5)
  max_cr_factor <<- evaluateValue(chromosome, 0.05, 0.8, 4)
  exclu <<- evaluateValue(chromosome, 0.05, 0.8, 4)
  dt1 <<- evaluateValue(chromosome, 1, 2.5, 4)
  dt2 <<- evaluateValue(chromosome, 1.5, 3, 4)
  R <<- evaluateValue(chromosome, 0, 5, 4)
  Zu <<- evaluateValue(chromosome, 3, 30, 4)
  
  th <<- evaluateValue(chromosome, 0.1, 10, 7)
  nps <<- evaluateValue(chromosome, 0.05, 5, 7)
  epsilon <<- evaluateValue(chromosome, 0.2, 10, 6)
  MDCW <<- evaluateValue(chromosome, 0.2, 6, 5)
  CLc <<- evaluateValue(chromosome, 0.1, 3, 5)
  Oc <<- evaluateValue(chromosome, 0.1, 3, 5)
  CLs <<- evaluateValue(chromosome, 0.1, 3, 5)
  Os <<- evaluateValue(chromosome, 0.1, 3, 5)
  
  if (algo == 1){
    algoVal <<- get(grid_canopyAlgo[algo])(subc)
  } else if (algo == 2){
    algoVal <<- get(grid_canopyAlgo[algo])(0)
  } else if (algo > 2){
    algoVal <<- get(grid_canopyAlgo[3])(max_edge = c(0, maxedge2), subcircle = subc)
  }
  
  startIndex <<- 1
}


# This function evaluates the fitness of each segmentation algorithm
evaluateFit <- function(treelas, noTrees){
  if (noTrees < 1){
    return(maxfitness)
  }
  treeError <- abs(actualTrees - noTrees)
  ad <<- aggregate(.~treeID, treelas@data, mean)
  coordinates(ad) <<- cbind(ad$X, ad$Y)
  proj4string(ad) <<- proj4string(groundTruth)
  
  dist <- c()
  for (i in 1:actualTrees){
    dist[i] <- gDistance(groundTruth[i,], ad)
  }
  avgError <- mean(dist) + treeError/5
  return(avgError)
}


# This is a function that displays the desired results: 
# chromosome, evaluates algorithm values and fitness
writeReport <- function(GA_obj, id = 1){
  optChild <<- as.numeric(GA_obj@solution[id,])
  idealFitness <- evaluateTreeSegmentation(optChild)
  
  algoNames <- c('Watershed', 'Dalponte2016', 'Silva2016', 'Li2012', 'Ptrees', 'Hamraz2016')
  optFitness <- c(waterShedFitness, dalponteFitness, silvaFitness, li2012Fitness, 
                  ptreesFitness, hamrazFitness)
  optTreeValues <- c(waterShedTrees, dalponteTrees, silvaTrees, li2012Trees, PTrees, hamrazTrees)
  
  results <<- data.frame(Algorithm = 0, Detected_trees = 0, Fitness = 0)
  for (i in 1:length(algoNames)){
    results[i,1] <- algoNames[i]
    results[i,2] <- optTreeValues[i]
    results[i,3] <- optFitness[i]
  }
  results <- results[order(results$Fitness, decreasing = T), ]
  
  cat("Optimal Chromosome is:\n")
  print(optChild)
  print(results)
}


# This runs separate GAs for every plot and saves them to an output file
watersh <- 0; dalpon <- 0; silva <- 0; ptre <- 0; hamraz <- 0; li20 <- 0
fullGArun <- function(){
  for (i in 1:noPlots){
    groundTruth <<- allShp[[i]]
    las <- readLAS(paste0("../data/als_data/", Las[i]), select = "x")
    aa <- terra::intersect(groundTruth, las)
    ab <- aa@bbox
    poi <- paste("-inside", ab[1,1], ab[2,1], ab[1,2], ab[2,2])
    groundTruth <<- subset(groundTruth, X_Location >= ab[1,1] & X_Location <= ab[1,2] & 
                            Y_Location >= ab[2,1] & Y_Location <= ab[2,2])
    actualTrees <- dim(groundTruth)[1]
    las <<- readLAS(paste0("../data/als_data/", Las[i]), select = "xyzr", filter = poi)
    
    cat(sprintf("\nGenetic algorithm for Plot %.0f: \n", i))
    GAfile <- paste0("GA", i)
    log_open(logfile)
    
    GA1 <- ga(type = "binary", fitness = evaluateTreeSegmentation, nBits = chromosomeLength,
              popSize = populationSize, maxiter = maxGeneration, run = 50, maxFitness = 6*maxfitness,
              seed = randomSeed, pcrossover = probCrossover, pmutation = probMutation, parallel = 100,
              crossover = gabin_spCrossover, mutation = gabin_raMutation, selection = gabin_lrSelection,
              keepBest = T, elitism = 5, suggestions = binaryVals)
    
    saveRDS(GA1, paste0("../output/", GAfile, ".rds"))
    
    optChild <- as.numeric(GA1@solution[1,])
    idealFitness <- evaluateTreeSegmentation(optChild)
    watersh <<- watersh + waterShedFitness/noPlots
    dalpon <<- dalpon + dalponteFitness/noPlots
    silva <<- silva + silvaFitness/noPlots
    ptre <<- ptre + ptreesFitness/noPlots
    hamraz <<- hamraz + hamrazFitness/noPlots
    li20 <<- li20 + li2012Fitness/noPlots
    log_close()
  }
}

fullGArun()



# This is the actual code for a single GA run
log_open(logfile)
GA1 <- ga(type = "binary", fitness = evaluateTreeSegmentation, nBits = chromosomeLength,
          popSize = populationSize, maxiter = maxGeneration, run = 50, maxFitness = 6*maxfitness,
          seed = randomSeed, pcrossover = probCrossover, pmutation = probMutation, parallel = 100,
          crossover = gabin_spCrossover, mutation = gabin_raMutation, selection = gabin_lrSelection,
          keepBest = T, elitism = 5, suggestions = binaryVals)

writeReport(GA1)
log_close()



# This iterates through all saved GA variables and averages the fitness for all
# segmentation algorithms
watersh <- 0; dalpon <- 0; silva <- 0; ptre <- 0; hamraz <- 0; li20 <- 0
allGAs <- dir(path = "../output/", pattern = "*.rds")
dataIds <- c(1,2,3,4,6,8,9,10,11,12)
lenGA <- length(allGAs)
evaluateGAs <- function(){
  log_open(logfile)
  for (i in 1:lenGA){
    groundTruth <- readOGR(paste0("../data/ground/", Shp[dataIds[i]]), verbose = F)
    las <- readLAS(paste0("../data/als_data/", Las[dataIds[i]]), select = "x")
    aa <- terra::intersect(groundTruth, las)
    ab <- aa@bbox
    poi <- paste("-inside", ab[1,1], ab[2,1], ab[1,2], ab[2,2])
    groundTruth <<- subset(groundTruth, X_Location >= ab[1,1] & X_Location <= ab[1,2] & 
                             Y_Location >= ab[2,1] & Y_Location <= ab[2,2])
    actualTrees <<- dim(groundTruth)[1]
    las <<- readLAS(paste0("../data/als_data/", Las[dataIds[i]]), select = "xyzr", filter = poi)
    cat(sprintf("\nGenetic algorithm %.0f: \n", i))
    GA1 <- readRDS(paste0("../output/", allGAs[i]))
    
    optChild <- as.numeric(GA1@solution[1,])
    idealFitness <- evaluateTreeSegmentation(optChild)
    watersh <<- watersh + waterShedFitness/lenGA
    dalpon <<- dalpon + dalponteFitness/lenGA
    silva <<- silva + silvaFitness/lenGA
    ptre <<- ptre + ptreesFitness/lenGA
    hamraz <<- hamraz + hamrazFitness/lenGA
    li20 <<- li20 + li2012Fitness/lenGA
  }
  log_close()
}

evaluateGAs()



# This reads in a GA variable previously saved as a .rds file and displays the results
testGA <- function(id = 1){
  gaVal <- paste0("GA", id)
  assign(gaVal, readRDS(paste0("../output/", gaVal, ".rds")))
  writeReport(get(gaVal))
}
testGA(1)


