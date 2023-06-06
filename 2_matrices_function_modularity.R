#' LIEB.NETWORK Function for modularity index
#'
#' This function computes the modularity values for a set of bipartite networks
#' based on the "DormannStrauss" method using the bipartite R package.
#'
#' @param diretorio The directory path containing the network files.
#' @param ext The file extension of the network files.
#' @return A matrix containing the modularity values for each network.
#' @creator Emanuelle Brito
#'
#' @examples
#' LIEB.NETWORK("path/to/directory", "txt")


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
    QuanBiMo <- computeModules(matriz[[i]], method = "DormannStrauss", steps = 100)
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


subset1 <- LIEB.NETWORK(diretorio = "C:/Users/emanu/Dropbox (Personal)/Doutorado - Emanuelle/Cap 1 - Scientometric/data/subset" , ext = ".csv")
write.csv(subset1,"results_networks.csv",row.names=T)


LIEB.NETWORK <- function(diretorio, ext) {
  library(bipartite)  # Load bipartite package
  setwd(diretorio)
  dados <- list.files(diretorio, pattern = ext, full.names = TRUE)
  n <- length(dados)
  matriz <- vector("list", n)
  
  results <- list(modularity = numeric(), compartments = numeric(), matrix_name = character())
  
  for (i in 1:n) {
    print(paste("Processing matrix:", i))
    
    tryCatch({
      matriz[[i]] <- read.csv(dados[i], header = TRUE, sep = ";", dec = ",")
      matriz[[i]] <- matriz[[i]][, -1]  # Remove the first column
      matriz[[i]] <- as.matrix(matriz[[i]])
      
      print(paste("Class:", class(matriz[[i]])))
      print(paste("Dimensions:", dim(matriz[[i]])))
      
      QuanBiMo <- computeModules(matriz[[i]], method = "DormannStrauss", steps = 10)
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


