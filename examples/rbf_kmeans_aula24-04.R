rm(list = ls())

# cria o dataset
load("C:\\dev\\rna-2023-1\\data2classXOR.txt")

#  nao coloca cores, pq nao sabemos o rotulo

# N <- 100

# xc1p1 <- cbind(rnorm(N / 4) + 12, rnorm(N / 4))
# xc1p2 <- cbind(rnorm(N / 4), rnorm(N / 4) + 12)
# xc1 <- rbind(xc1p1, xc1p2)

# xc2p1 <- cbind(rnorm(N / 4), rnorm(N / 4))
# xc2p2 <- cbind(rnorm(N / 4) + 12, rnorm(N / 4) + 12)
# xc2 <- rbind(xc2p1, xc2p2)

# plot(
#     NULL,
#     main = "RBF - Kmeans",
#     xlab = "x1",
#     ylab = "x2",
#     ylim = c(-10, 20),
#     xlim = c(-5, 15)
# )

# points(xc1[, 1], xc1[, 2], col = "red")
# points(xc2[, 1], xc2[, 2], col = "blue")

plot(X[1:60, 1], X[1:60, 2], type = "p", xlim = c(0, 6), ylim = c(0, 6), xlab = "x1", ylab = "x2")
par(new = TRUE)
plot(X[61:120, 1], X[61:120, 2], type = "p", xlim = c(0, 6), ylim = c(0, 6), xlab = "x1", ylab = "x2")

mykmeans <- function(Xin, k) {
    N <- nrow(Xin)
    n <- ncol(Xin)

    Mmedias <- matrix(ncol = n, nrow = k)

    kindex <- sample(N)
    Mmedias <- Xin[kindex[1:k], ]
    
    # agora varre cada x e calcula a distancia
    
    for (i in 1:N) {
      xt <-Xin[i,]
      auxmat <- matrix(xt, ncol = 2, nrow = 3, byrow = TRUE)
      dmat <- rowSums((auxmat - Mmedias)^2)
    }

    return (Mmedias)
}

retlist <- mykmeans(X, 3)

xt <- X[1,]

# print(dist(xt, retlist[1,]))
# print(matrix(xt, nrow = 3, byrow = TRUE))
