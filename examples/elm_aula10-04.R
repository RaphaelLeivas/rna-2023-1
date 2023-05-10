# Y = HW (pagina 76 notas de aula)
# onde H = tanh(x * Z)

rm(list = ls())
# dev.off()

library("corpcor")

N <- 50
p <- 10

xc1p1 <- cbind(rnorm(N / 2) + 12, rnorm(N / 2))
xc1p2 <- cbind(rnorm(N / 2), rnorm(N / 2) + 12)
xc1 <- rbind(xc1p1, xc1p2)

xc2p1 <- cbind(rnorm(N / 2), rnorm(N / 2))
xc2p2 <- cbind(rnorm(N / 2) + 12, rnorm(N / 2) + 12)
xc2 <- rbind(xc2p1, xc2p2)

plot(
  NULL,
  main = "Treinamento ELM",
  xlab = "x1",
  ylab = "x2",
  ylim = c(-10, 20),
  xlim = c(-15, 15)
)

points(xc1[, 1], xc1[, 2], col = "red")
points(xc2[, 1], xc2[, 2], col = "blue")

Y <- rbind(matrix(-1, nrow = N), matrix(1, nrow = N))

Z <- replicate(p, runif(3, -0.5, 0.5))

X <- as.matrix(rbind(xc1, xc2))

Xaug <- cbind(replicate(N, 1), X)

H <- tanh(Xaug %*% Z) # tanh é a função de ativação da camada intermediária

W <- pseudoinverse(H) %*% Y

Yhat_train <- sign(H %*% W)
e_train <- sum((Y - Yhat_train)^2) / 4
print(e_train)

## achei o W, agora vamos aplicar esse paramentro aprendido sobre o grid

seqx1x2 <- seq(-15, 15, 0.2)
npgrid <- length(seqx1x2)
M <- matrix(nrow = npgrid, ncol = npgrid)
ci <- 0

for (x1 in seqx1x2) {
  ci <- ci + 1
  cj <- 0
  for (x2 in seqx1x2) {
    cj <- cj + 1
    xin <- as.matrix(cbind(1, x1, x2))
    H_in <- tanh(xin %*% Z)
    M[ci, cj] <- 1.0 * (H_in %*% W >= 0)
    # a saida final da rede é Y(x) = H * W, onde H = tanh(x * Z)
    # a rede tem 3 entradas (recebe um vetor de 3 posições), e retorna
    # 1 ou -1 na saida, que plotamos com o contour no R
  }
}

contour(
  seqx1x2,
  seqx1x2,
  M,
  add = TRUE,
  lwd = 1,
  drawlabels = FALSE,
  col = "orange"
)

## testes

# xc1p1_t <- cbind(rnorm(N / 2), rnorm(N / 2))
# xc1p2_t <- cbind(rnorm(N / 2), rnorm(N / 2) + 6)
# xc1_t <- rbind(xc1p1_t, xc1p2_t)

# xc2p1_t <- cbind(rnorm(N / 2), rnorm(N / 2))
# xc2p2_t <- cbind(rnorm(N / 2), rnorm(N / 2) + 6)
# xc2_t <- rbind(xc2p1_t, xc2p2_t)

# X_t <- rbind(xc1_t, xc2_t)

# Xaug_t <- cbind(replicate(N, 1), X_t)

# H_t <- tanh(Xaug_t %*% Z)
# Yhat_t <- sign(H_t %*% W)
# e_t <- sum((Y - Yhat_t)^2) / 4

# print(e_t)

# o erro e_t está dando muito alto, deve ser a forma que estou gerando os dados de teste
# que está incorreta
