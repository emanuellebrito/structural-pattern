matriz_bi <- read.csv("kato_2000.csv", header = TRUE, sep = ";", dec = ",")

matriz_bi <- matriz_bi[,-1]  # Remove the first column

matriz_bi <- as.matrix(matriz_bi)

mo <- computeModules(matriz_bi, method="DormannStrauss", steps = 100)
modularity <- round(mo@likelihood, 3)
mo2 <- computeModules(matriz_bi, method="Beckett")
modularity2 <- round(res2@likelihood, 3)
res <- DIRT_LPA_wb_plus(matriz_bi)

networklevel(matriz_bi, index = "number of compartments")
networklevel(matriz_bi)

index <- networklevel(matriz_bi)
index <- as.data.frame(index)

