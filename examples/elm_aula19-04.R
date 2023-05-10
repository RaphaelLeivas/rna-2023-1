# Y = HW (pagina 76 notas de aula)
# onde H = tanh(x * Z)

# cria as funções que geram A e P, vamos precisar deles no LOO no final da aula

rm(list = ls())
# dev.off()

library("corpcor")

# usa N divisivel por 4 para evitar problema, pois contruo os dados de um quarto
# em um quarto
# Note que N é o numero total de dados
N <- 100 
p <- 50
lambda <- 0.2

xc1p1 <- cbind(rnorm(N / 4) + 12, rnorm(N / 4))
xc1p2 <- cbind(rnorm(N / 4), rnorm(N / 4) + 12)
xc1 <- rbind(xc1p1, xc1p2)

xc2p1 <- cbind(rnorm(N / 4), rnorm(N / 4))
xc2p2 <- cbind(rnorm(N / 4) + 12, rnorm(N / 4) + 12)
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

Y <- rbind(matrix(-1, nrow = N / 2), matrix(1, nrow = N / 2))

Z <- replicate(p, runif(3, -0.5, 0.5))

X <- as.matrix(rbind(xc1, xc2))

Xaug <- cbind(replicate(N, 1), X)

H <- tanh(Xaug %*% Z) # tanh é a função de ativação da camada intermediária

# define as matrizes A e P
# L = lambda * diag(p)
A <- (t(H) %*% H) + (lambda * diag(p))
A_inv <- solve(A)
W <- A_inv %*% t(H) %*% Y

P <- (diag(N) - H %*% solve(A) %*% t(H))

Yhat_train <- H %*% W
e_train <- sum((Y - Yhat_train)^2) / 4
# print(e_train)

# com A e P, podemos calcular os erros

Je <- t(Y) %*% (P %*% P) %*% Y

Jew <- t(Y - Yhat_train) %*% (Y - Yhat_train)
print(cbind(Je, Jew))

Jw <- t(Y) %*% (P - P %*% P) %*% Y
Jww <- t(W) %*% (lambda * diag(p)) %*% W
print(cbind(Jw, Jww))

J <- t(Y) %*% P %*% Y
Jsum <- Jew + Jww
print(cbind(J, Jsum))

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
