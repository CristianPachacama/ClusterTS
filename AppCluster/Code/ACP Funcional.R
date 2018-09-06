library(fpca)
library(fdapace)
data(medfly25)

# Turn the original data into a list of paired amplitude and timing lists
Flies <- MakeFPCAInputs(medfly25$ID, medfly25$Days, medfly25$nEggs)
fpcaObjFlies <- FPCA(Flies$Ly, Flies$Lt, list(plot = TRUE, methodMuCovEst = 'smooth', userBwCov = 2))
