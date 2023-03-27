rm(list = ls())
dev.off()

source("C:\\dev\\rna-2023-1\\examples\\trainperceptron.R")

n <- 2
N <- 30

xc1 <- matrix(rnorm(N, sd = 0.2) + 1, ncol = n, nrow = N)
xc2 <- matrix(rnorm(N, sd = 0.2) + 4, ncol = n, nrow = N)

plot(
    xc1[, 1],
    xc1[, 2],
    xlim = c(0, 6),
    ylim = c(0, 6),
    xlab = "x1",
    ylab = "x2",
    col = "red"
)
par(new = TRUE)
plot(
    xc2[, 1],
    xc2[, 2],
    xlim = c(0, 6),
    ylim = c(0, 6),
    xlab = "x1",
    ylab = "x2",
    col = "blue"
)

xall <- rbind(xc1, xc2)
yall <- rbind(matrix(0, nrow = N), matrix(1, nrow = N))

retlist <- trainperceptron(xall, yall, 0.1, 0.01, 1000, 1)
wfinal <- as.matrix(retlist[[1]])
erro <- retlist[[2]]

t <- seq(0, 10, 0.1)

lines(t, w[3] * t^2 + w[2] * t^1 + w[1] * t^0)

# y <- w[3] * x^2

# x <- rnorm(30, 0, 4)
# y <- rnorm(30, 50, 4)

# plot(
#     NULL,
#     main = "Perceptron",
#     xlab = "Eixo X",
#     ylab = "Eixo Y",
#     ylim = c(-10, 100),
#     xlim = c(0, 30)
# )

# points(x)
# points(y)
# points(Z)
