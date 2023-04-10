rm(list = ls())
# dev.off()

library("corpcor") # usado para função da pseudoinversa

N <- 60
p <- 5

# xc1: x da classe 1

xc1 <- matrix(rnorm(N), ncol = 2) + matrix(c(7, 4), nrow = N/2, ncol = 2, byrow = TRUE)
xc2 <- matrix(rnorm(N), ncol = 2) + matrix(c(9, 5.5), nrow = N/2, ncol = 2, byrow = TRUE)

Y <- rbind(matrix(0, nrow = N/2), matrix(1, nrow = N/2))

plot(
  NULL,
  main = "Treinamento ELM",
  xlab = "x1",
  ylab = "x2",
  ylim = c(0, 10),
  xlim = c(0, 15)
)

points (xc1[,1], xc1[,2], col = "red")
points (xc2[,1], xc2[,2], col = "blue")

Z <- replicate(p, runif(3, -0.5, 0.5))

X <- as.matrix(rbind(xc1, xc2))

Xaug <- cbind(replicate(N, 1), X)

H <- tanh(Xaug %*% Z)

W <- pseudoinverse(H) %*% Y

Yhat_train <- sign(H %*% W)
e_train <- sum((Y - Yhat_train)^2)/4

## testes

xc1_t <- matrix(rnorm(N), ncol = 2) + matrix(c(7, 4), nrow = N/2, ncol = 2, byrow = TRUE)
xc2_t <- matrix(rnorm(N), ncol = 2) + matrix(c(9, 5.5), nrow = N/2, ncol = 2, byrow = TRUE)
X_t <- as.matrix(rbind(xc1, xc2))

Xaug_t <- cbind(replicate(N, 1), X_t)

H_t <- tanh(Xaug_t %*% Z)
Yhat_t <- sign(H_t %*% W)
e_t <- sum((Y - Yhat_t)^2)/4
print(e_t)

# falta printar aqui a curva



