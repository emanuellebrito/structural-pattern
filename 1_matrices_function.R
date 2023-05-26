########## função para automatizar cálculos de matrizes de interações #######

LIEB.NETWORK <- function(diretorio, ext, aleats = 999)#(diretorio de arquivos, ext(extencao usada. ex= txt ou csv))
{
  setwd(diretorio)
  dados <- list.files(diretorio)
  n <- length(dados)
  matriz <- list()
  for (i in 1:n)
  {
    matriz[[i]] <- read.csv(dados[i], h=T, sep = ";", dec = ",")
  }
  
  C <- numeric(n)
  for (i in 1:n)
  {
    ip <- nrow(matriz[[i]])*ncol(matriz[[i]])
    iobs1 <- ifelse(matriz[[i]] > 0,1,0)
    iobs <- sum(iobs1)
    resuC <- ip/iobs
    C[i] <- resuC
  }
  
  p.C <- matrix( ,ncol = aleats, nrow = n)
  for (i in 1:n)
  {
    mattemp <- matriz[[i]]
    
    for (k in 1:aleats)
    {
      aleat <- matrix(sample(rep(c(0,1),ncol(mattemp)*nrow(mattemp)), replace=T), nrow=nrow(mattemp), ncol=ncol(mattemp))
      ip <- nrow(aleat)*ncol(aleat)
      iobs <- sum(aleat)
      resuCR <- ip/iobs
      p.C[i,k] <- resuCR
    }
  }
  
  C.abs <- abs(C)
  p.C.abs <- abs(p.C)
  p <- numeric(n)
  
  for (i in 1:n)
  {
    extremos <- sum(p.C.abs[i, ] >= C.abs[i])
    p[i] <- (extremos+1) / (aleats+1)
  }
  
  AI <- numeric(n)
  for (i in 1:n)
  {
    num.dif <- ncol(matriz[[i]]) - nrow(matriz[[i]])
    den.somas <- sum(dim(matriz[[i]]))
    resuAI <- num.dif/den.somas
    AI[i] <- resuAI
  }
  
  size <- numeric(n)
  for (i in 1:n)
  {
    num.net <- ncol(matriz[[i]]) + nrow(matriz[[i]])
    size[i] <- num.net
  }
  
  ResuMat <- matrix( , nrow = n, ncol = 4)
  rownames(ResuMat) <- dados
  colnames(ResuMat) <- c("C", "p", "AI", "size")
  ResuMat[ , 1] <- C
  ResuMat[ , 2] <- p
  ResuMat[ , 3] <- AI
  ResuMat[ , 4] <- size
  
  return(ResuMat)
}

LIEB.NETWORK(diretorio = "C:/Users/emanu/Dropbox (Personal)/Doutorado - Emanuelle/Cap 1 - Scientometric/data/pollination_webs" , ext = ".csv")

cap3 <- LIEB.NETWORK(diretorio = "C:/Users/emanu/Dropbox (Personal)/Doutorado - Emanuelle/Cap 1 - Scientometric/data/pollination_webs" , ext = ".csv")

write.csv(cap3,"results_networks.csv",row.names=T)


