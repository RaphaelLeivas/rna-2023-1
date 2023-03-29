rm(list = ls())
library("plot3D")

xc1 <- matrix(0.3 * rnorm(60) + 2, ncol = 2)
xc2 <- matrix(0.3 * rnorm(60) + 4, ncol = 2)

plot(xc1[, 1], xc1[, 2], xlim = c(0, 6), ylim = c(0, 6), xlab = "x1", ylab = "x2", col = "blue")
par(new = TRUE)
plot(xc2[, 1], xc2[, 2], xlim = c(0, 6), ylim = c(0, 6), xlab = "x1", ylab = "x2", col = "red")

seqx1x2 <- seq(0, 6, 0.2)
npgrid <- length(seqx1x2)
M <- matrix(nrow = npgrid, ncol = npgrid)
ci <- 0
w <- as.matrix(c(6, 1, 1))

for (x1 in seqx1x2) {
    ci <- ci + 1
    cj <- 0
    for (x2 in seqx1x2) {
        cj <- cj + 1
        xin <- as.matrix(cbind(-1, x1, x2))
        M[ci, cj] <- 1.0 * ((xin %*% w) >= 0) # yperceptron (xin, c (1.5,1,1),1)
    }
}

ribbon3D(seqx1x2, seqx1x2, xlim = c(0, 6), ylim = c(0, 6), M, colkey = FALSE)
scatter3D(xc1[, 1], xc1[, 2], matrix(0, nrow = dim(xc1)[1]), add = TRUE, col = "blue", colkey = FALSE)
scatter3D(xc2[, 1], xc2[, 2], matrix(0, nrow = dim(xc1)[1]), add = TRUE, col = "red", colkey = FALSE)
