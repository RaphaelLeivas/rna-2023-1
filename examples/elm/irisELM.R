rm(list = ls())

source("C:\\dev\\rna-2023-1\\examples\\elm\\YELM.R")
source("C:\\dev\\rna-2023-1\\examples\\elm\\trainELM.R")

library("RSNNS")
data(iris)

xseq <- sample(100)
xall <- as.matrix(iris[xseq, 1:4])
yall <- (1 * (iris$Species[xseq] == "versicolor") - 0.5) * 2
xyall <- splitForTrainingAndTest(xall, yall, ratio = 0.3)
xin <- xyall$inputsTrain
yd <- xyall$targetsTrain
xinteste <- xyall$inputsTest
yteste <- xyall$targetsTest
retlist <- trainELM(xin, yd, 2, 1)
w <- retlist[[1]]
H <- retlist[[2]]
Z <- retlist[[3]]
yt <- YELM(xinteste, Z, w, 1)
acuracia <- 1 - (t(yteste - yt) %*% (yteste - yt)) / 30
print(acuracia)
