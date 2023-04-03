rm(list = ls())
dev.off()

library("plot3D")
source("C:\\dev\\rna-2023-1\\examples\\trainperceptron.R")

xc1 <- 0.3 * rnorm(60) + 2
xc2 <- 0.3 * rnorm(60) + 4

# xc1 <- matrix(0.3 * rnorm(60) + 2, ncol = 2)
# xc2 <- matrix(0.3 * rnorm(60) + 4, ncol = 2)

plot(
    NULL,
    main = "Treinamento Perceptron",
    xlab = "x1",
    ylab = "x2",
    ylim = c(-6, 12),
    xlim = c(0, 5)
)

points(xc1, col = "blue")
points(xc2, col = "red")

xall <- as.matrix(rbind(xc1, xc2))
yall <- rbind(matrix(0, nrow = 30), matrix(1, nrow = 30))

retlist <- trainperceptron(xall, yall, 0.01, 0.01, 1000, 1)
wfinal <- retlist[[1]]
erro <- retlist[[2]]

# ci <- 0
# for (x1 in seqx1x2) {
#     ci <- ci + 1
#     cj <- 0
#     for (x2 in seqx1x2) {
#         cj <- cj + 1
#         xin <- as.matrix(cbind(-1, x1, x2))
#         M[ci, cj] <- 1.0 * ((xin %*% wfinal) >= 0) # yperceptron (xin, c (1.5,1,1),1)
#     }
# }

w0 <- wfinal[1]
w1 <- wfinal[2]
w2 <- wfinal[3]

a <- -(w1 / w2)
b <- w0 / w2

print(a)
print(b)

t <- seq(0, 5, 0.1)

lines(t, a * t + b, col = "orange")

# # par(new = TRUE)
# # plot(xc1[, 1], a * xc1[, 1] + b, type = "l", xlim = c(0, 6), ylim = c(0, 6), xlab = "x1", ylab = "x2", col = "black")

# x1_reta <- seq(0, 6, 0.2)
# x2_reta <- a * x1_reta + b
# lines(x1_reta, x2_reta, col = "black")

# ribbon3D(seqx1x2, seqx1x2, xlim = c(0, 6), ylim = c(0, 6), M, colkey = FALSE)
# scatter3D(xc1[, 1], xc1[, 2], matrix(0, nrow = dim(xc1)[1]), add = TRUE, col = "blue", colkey = FALSE)
# scatter3D(xc2[, 1], xc2[, 2], matrix(0, nrow = dim(xc1)[1]), add = TRUE, col = "red", colkey = FALSE)
