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
  matrix <- vector("list", n)
  
  size <- numeric(n)
  C <- numeric(n)
  AS <- numeric(n)
  NODF <- numeric(n)
  Plants <- numeric(n)
  Animals <- numeric(n)
  
  for (i in 1:n) {
    matrix[[i]] <- read.csv(dados[i], header = TRUE, sep = ";", dec = ",")
    
    # Calculate network properties
    binaryMat <- ifelse(matrix[[i]] > 0, 1, 0)
    num.net <- ncol(matrix[[i]]) + nrow(matrix[[i]])
    size[i] <- num.net - 1
    
    connectance <- networklevel(binaryMat, index = "connectance")
    C[i] <- round(connectance, 3)
    
    asymmetry <- networklevel(binaryMat, index = "web asymmetry")
    AS[i] <- round(asymmetry, 3)
    
    nestedness <- networklevel(binaryMat, index = "NODF")
    NODF[i] <- round(nestedness, 3)
    
    # Calculate species richness
    richness_p <- ncol(matrix[[i]])
    richness_a <- nrow(matrix[[i]])
    Plants[i] <- richness_p - 1
    Animals[i] <- richness_a
  }
  
  ResuMat <- matrix(nrow = n, ncol = 6)  
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
