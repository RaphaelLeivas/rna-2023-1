rm(list = ls())
dev.off()

sech2 <- function(u) {
  return(((2 / (exp(u) + exp(-u)) * 2 / (exp(u) + exp(-u)))))
}

N <- 45
x <- seq(0, 2 * pi, 2 * pi / (N - 1))
y <- sin(x) + runif(N, min = -0.1, max = 0.1)
xtest <- seq(0, 2 * pi, 0.01)
ytest <- sin(xtest)

xtrain <- cbind(x, y)
ytrain <- cbind(xtest, ytest)

plot(
  NULL,
  main = "Ex9",
  xlab = "x1",
  ylab = "x2",
  xlim = c(0, 2 * pi),
  ylim = c(-1.5, 1.5)
)

points(x, y, col = "blue", lwd = 2)
lines(xtest, ytest, col = "red", lwd = 2)

maxepocas <- 1000
nepocas <- 0
tol <- 0.01
eepoca <- tol + 1
eta <- 0.1
evec <- matrix(nrow = maxepocas, ncol = 1)

i1 <- 1
i4 <- 1
i5 <- 1
i8 <- 1

# inicializa os pesos

w95 <- runif(1) - 0.5
w96 <- runif(1) - 0.5
w97 <- runif(1) - 0.5

w106 <- runif(1) - 0.5
w107 <- runif(1) - 0.5
w108 <- runif(1) - 0.5

w61 <- runif(1) - 0.5
w62 <- runif(1) - 0.5
w63 <- runif(1) - 0.5

w72 <- runif(1) - 0.5
w73 <- runif(1) - 0.5
w74 <- runif(1) - 0.5

while ((nepocas < maxepocas) && (eepoca > tol)) {
  ei2 <- 0 # erro quadrado
  xseq <- sample(N) # define sequencia
  yhat <- c()
  
  for (i in 1:N) {
    irand <- xseq[i] # pega uma amostra aleatoria

    xin <- xtrain[irand, ] # da linha irand com todas as colunas
    yd <- ytrain[irand, ] # y desejado
    i2 <- xin[2]
    i3 <- xin[1]

    yd9 <- yd[2]

    u6 <- i1 * w61 + i2 * w62 + i3 * w63
    u7 <- i2 * w72 + i3 * 732 + i4 * w74

    i6 <- tanh(u6)
    i7 <- tanh(u7)

    u9 <- i5 * w95 + i6 * w96 + i7 * w97
    u10 <- i6 * w106 + i7 * w107 + i8 * w108

    i9 <- tanh(u9)
    i10 <- tanh(u10)

    e9 <- yd9 - i9

    d9 <- e9 * sech2(u9) # delta9, simbolo do impulso unitario

    # agora vem o ajuste
    dw95 <- eta * d9 * i5
    dw96 <- eta * d9 * i6
    dw97 <- eta * d9 * i7

    w95 <- w95 + dw95 # ajuste
    w96 <- w96 + dw96
    w97 <- w97 + dw97

    # todos os pesos ajustados, agora calcula o erro

    ei2 <- ei2 + (e9 * e9)
    yhat <- c(yhat, as.numeric(i9))
  }

  # incrementa numero de epocas
  nepocas <- nepocas + 1
  evec[nepocas] <- ei2 / N

  # armazena erro por epocas
  eepoca <- evec[nepocas]
}

lines(x, yhat, col = "orange", lwd = 2)


par(new = TRUE)
plot(evec[1:nepocas], type = "l", lwd = 2)
