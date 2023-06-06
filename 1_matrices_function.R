# Function: LIEB.NETWORK
# Creator: Emanuelle Brito
# Description: Calculate network metrics from bipartite data files
#
# Arguments:
#   - diretorio: Directory path where the data files are located.
#   - ext: File extension of the data files.
#   
# Returns:
#   - ResuMat: Matrix containing the calculated network metrics for each data file.
#
# Required Packages:
#   - bipartite: This function requires the 'bipartite' package for network calculations.


LIEB.NETWORK <- function(diretorio, ext) {
  library(bipartite)  # Load bipartite package
  setwd(diretorio)
  dados <- list.files(diretorio, pattern = ext, full.names = TRUE)
  n <- length(dados)
  web <- vector("list", n)
  
  size <- numeric(n)
  C <- numeric(n)
  AS <- numeric(n)
  NODF <- numeric(n)
  Plants <- numeric(n)
  Animals <- numeric(n)
  
  for (i in 1:n) {
    web[[i]] <- read.csv(dados[i], header = TRUE, sep = ";", dec = ",")
    
    # Calculate network properties
    binaryMat <- ifelse(web[[i]] > 0, 1, 0)
    num.net <- ncol(web[[i]]) + nrow(web[[i]])
    size[i] <- num.net - 1
    
    connectance <- networklevel(binaryMat, index = "connectance")
    C[i] <- round(connectance, 3)
    
    asymmetry <- networklevel(binaryMat, index = "web asymmetry")
    AS[i] <- round(asymmetry, 3)
    
    nestedness <- networklevel(binaryMat, index = "NODF")
    NODF[i] <- round(nestedness, 3)
    
    # Calculate species richness
    richness_p <- ncol(web[[i]])
    richness_a <- nrow(web[[i]])
    Plants[i] <- richness_p - 1
    Animals[i] <- richness_a
  }
  
  ResuMat <- web(nrow = n, ncol = 6)  
  rownames(ResuMat) <- basename(dados)
  colnames(ResuMat) <- c("size", "C", "AS", "NODF", "Plants", "Animals")
  ResuMat[, 1] <- size
  ResuMat[, 2] <- C
  ResuMat[, 3] <- AS
  ResuMat[, 4] <- NODF
  ResuMat[, 5] <- Plants
  ResuMat[, 6] <- Animals
  
  return(ResuMat)
}

cap3_richness <- LIEB.NETWORK(diretorio = "C:/Users/emanu/Dropbox (Personal)/Doutorado - Emanuelle/Cap 1 - Scientometric/data/pollination_webs" , ext = ".csv")
write.csv(cap3_richness, "results_networks.csv", row.names=T)


#Since modularity takes a long time to process its better do it apart

LIEB.NETWORK <- function(diretorio, ext) {
  library(bipartite)  # Load bipartite package
  setwd(diretorio)
  dados <- list.files(diretorio, pattern = ext, full.names = TRUE)
  n <- length(dados)
  matriz <- vector("list", n)
  
  for (i in 1:n) {
    matriz[[i]] <- read.csv(dados[i], header = TRUE, sep = ";", dec = ",")
    matriz[[i]] <- matriz[[i]][, -1]  # Remove the first column
    matriz[[i]] <- as.matrix(matriz[[i]])
  }
  
  modularity <- numeric(n)
  
  for (i in 1:n) {
    print(paste("Processing matrix:", i))
    print(paste("Class:", class(matriz[[i]])))
    print(paste("Dimensions:", dim(matriz[[i]])))
    QuanBiMo <- computeModules(matriz[[i]], method = "Beckett", steps = 10)
    modularity[i] <- round(QuanBiMo@likelihood, 3)
    print(paste("Modularity value:", modularity[i]))
  }
  
  compartments <- numeric(n)
  
  for (i in 1:n) {
    comp <- networklevel(matriz[[i]], index = "number of compartments")
    compartments[i] <- comp
  }
  
  ResuMat <- matrix(nrow = n, ncol = 2)  
  rownames(ResuMat) <- basename(dados)
  colnames(ResuMat) <- c("modularity", "compartments")
  ResuMat[, 1] <- modularity
  ResuMat[, 2] <- compartments
  
  return(ResuMat)
}


#Improved function to mapping possible errors reading the matrices

LIEB.NETWORK <- function(diretorio, ext) {
  library(bipartite)  # Load bipartite package
  setwd(diretorio)
  dados <- list.files(diretorio, pattern = ext, full.names = TRUE)
  n <- length(dados)
  matriz <- vector("list", n)
  
  results <- list(modularity = numeric(), compartments = numeric()) #matrix_name = character())
  
  for (i in 1:n) {
    print(paste("Processing matrix:", i))
    
    tryCatch({
      matriz[[i]] <- read.csv(dados[i], header = TRUE, sep = ";", dec = ",")
      matriz[[i]] <- matriz[[i]][, -1]  # Remove the first column
      matriz[[i]] <- as.matrix(matriz[[i]])
      
      print(paste("Class:", class(matriz[[i]])))
      print(paste("Dimensions:", dim(matriz[[i]])))
      
      QuanBiMo <- computeModules(matriz[[i]], method = "Beckett", steps = 10)
      modularity <- round(QuanBiMo@likelihood, 3)
      print(paste("Modularity value:", modularity))
      
      comp <- networklevel(matriz[[i]], index = "number of compartments")
      compartments <- comp
      
      # Store the processed results in the 'results' list
      results$matrix_name <- c(results$matrix_name, basename(dados[i]))
      results$modularity <- c(results$modularity, modularity)
      results$compartments <- c(results$compartments, compartments)
    }, error = function(e) {
      warning_message <- paste("Error processing matrix:", i)
      warning(warning_message)
    })
  }
  
  # Save the 'results' list in a separate object
  save(results, file = "processed_results.RData")
}


#function to calculate the connections number in each matrix
LIEB.NETWORK <- function(diretorio, ext) {
  library(bipartite)  # Load bipartite package
  setwd(diretorio)
  dados <- list.files(diretorio, pattern = ext, full.names = TRUE)
  n <- length(dados)
  data <- vector("list", n)
  
  Connections <- numeric(n)
  
  for (i in 1:n) {
    data[[i]] <- read.csv(dados[i], header = TRUE, sep = ";", dec = ",")
    
    # Convert weighted matrix to binary matrix
    data[[i]] <- data[[i]][, -1]  # Remove the first column
    data[[i]] <- ifelse(data[[i]] > 0, 1, 0)
    
    # Calculate network properties
    Connections[i] <- sum(data[[i]])
  }
  
  ResuMat <- matrix(nrow = n, ncol = 1)  
  rownames(ResuMat) <- basename(dados)
  colnames(ResuMat) <- "Connections"
  ResuMat[, 1] <- Connections
  
  return(ResuMat)
}

subset1 <- LIEB.NETWORK(diretorio = "C:/Users/emanu/Dropbox (Personal)/Doutorado - Emanuelle/Cap 1 - Scientometric/data/pollination_webs" , ext = ".csv")