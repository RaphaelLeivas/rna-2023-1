rm(list = ls())
dev.off()
library("plot3D")

source("C:\\dev\\rna-2023-1\\examples\\trainperceptron.R")
source("C:\\dev\\rna-2023-1\\examples\\yperceptron.R")

s1 <- 0.4
s2 <- 0.4
nc <- 200
xc1 <- matrix(rnorm(nc * 2), ncol = 2) * s1 + t(matrix((c(2, 2)), ncol = nc, nrow = 2))
xc2 <- matrix(rnorm(nc * 2), ncol = 2) * s2 + t(matrix((c(4, 4)), ncol = nc, nrow = 2))
plot(xc1[, 1], xc1[, 2], col = "red", xlim = c(0, 6), ylim = c(0, 6), xlab = "x_1", ylab = "x_2")
par(new = T)
plot(xc2[, 1], xc2[, 2], col = "blue", xlim = c(0, 6), ylim = c(0, 6), xlab = "x_1", ylab = "x_2")
par(new = T)

x1_reta <- seq(6 / 100, 6, 6 / 100)
x2_reta <- -x1_reta + 6
plot(x1_reta, x2_reta, type = "l", col = "orange", xlim = c(0, 6), ylim = c(0, 6), xlab = "", ylab = "")
par(new = T)

# chamar a função trainperceptron

seqi <- seq(0, 6, 0.1)
seqj <- seq(0, 6, 0.1)
M <- matrix(0, nrow = length(seqi), ncol = length(seqj))
ci <- 0

for (i in seqi) {
    ci <- ci + 1
    cj <- 0
    for (j in seqj) {
        cj <- cj + 1
        x <- c(i, j)
        M[ci, cj] <- yperceptron(x, w)
    }
}

plot(xc1[, 1], xc1[, 2], col = "red", xlim = c(0, 6), ylim = c(0, 6), xlab = "x_1", ylab = "x_2")
par(new = T)
plot(xc2[, 1], xc2[, 2], col = "blue", xlim = c(0, 6), ylim = c(0, 6), xlab = "", ylab = "")
par(new = T)

contour(seqi, seqj, M, xlim = c(0, 6), ylim = c(0, 6), xlab = "", ylab = "")
persp3D(seqi, seqj, counter = TRUE, theta = 55, phi = 30, r = 40, d =  0.1, expand = 0.5, ltheta = 90, lphi = 180, shade = 0.4, ticktype = "detailed", nticks = 5)

