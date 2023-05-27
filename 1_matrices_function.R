LIEB.NETWORK <- function(diretorio, ext, aleats = 999) {
  library(bipartite)  # Load bipartite package
  setwd(diretorio)
  dados <- list.files(diretorio, pattern = ext, full.names = TRUE)
  n <- length(dados)
  matriz <- vector("list", n)
  
  for (i in 1:n) {
    matriz[[i]] <- read.csv(dados[i], header = TRUE, sep = ";", dec = ",")
  }
  
  size <- numeric(n)
  for (i in 1:n) {
    num.net <- ncol(matriz[[i]]) + nrow(matriz[[i]])
    size[i] <- num.net
  }
  
  C <- numeric(n)
  AS <- numeric(n)
  NODF <- numeric(n)
  
  for (i in 1:n) {
    binaryMat <- ifelse(matriz[[i]] > 0, 1, 0)  # Convert to binary incidence matrix
    
    connectance <- networklevel(binaryMat, index = "connectance")  # Connectance from bipartite
    C[i] <- round(connectance, 3)
    
    asymmetry <- networklevel(binaryMat, index = "web asymmetry")  # Asymmetry from bipartite
    AS[i] <- round(asymmetry, 3)
    
    nestedness <- networklevel(binaryMat, index = "NODF")  # NODF from bipartite
    NODF[i] <- round(nestedness, 3)
  }
  
  ResuMat <- matrix(nrow = n, ncol = 4)  
  rownames(ResuMat) <- basename(dados)
  colnames(ResuMat) <- c("size", "C", "AS", "NODF") 
  ResuMat[, 1] <- size
  ResuMat[, 2] <- C
  ResuMat[, 3] <- AS
  ResuMat[, 4] <- NODF  
  
  return(ResuMat)
}


LIEB.NETWORK(diretorio = "C:/Users/emanu/Dropbox (Personal)/Doutorado - Emanuelle/Cap 1 - Scientometric/data/pollination_webs" , ext = ".csv")

cap3 <- LIEB.NETWORK(diretorio = "C:/Users/emanu/Dropbox (Personal)/Doutorado - Emanuelle/Cap 1 - Scientometric/data/pollination_webs" , ext = ".csv")

write.csv(cap3,"results_networks.csv",row.names=T)


