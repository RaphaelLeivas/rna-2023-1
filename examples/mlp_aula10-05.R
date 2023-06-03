rm(list = ls())
dev.off()
# load("C:\\dev\\rna-2023-1\\data2classXOR.txt")

sech2 <- function(u) {
  return(((2 / (exp(u) + exp(-u)) * 2 / (exp(u) + exp(-u)))))
}

xrange <- seq(-5, 5, 0.1)

plot(xrange, sech2(xrange), type = "l", xlim = c(-5, 5), ylim = c(-1, 1), col = "red", lwd = 2)
par(new = TRUE)
plot(xrange, tanh(xrange), type = "l", xlim = c(-5, 5), ylim = c(-1, 1), col = "blue", lwd = 2)

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

# dados de treinamento: usa o ou exclusivo

xtrain <- matrix(c(0, 0, 0, 1, 1, 0, 1, 1), nrow = 4, ncol = 2, byrow = T)

# os dados de treinamento tem que ser com -1 e 1, pois a funcao de ativacao tanh
# trabalha com -1 a 1
ytrain <- matrix(c(-1, 1, 1, -1, 1, -1, -1, 1), nrow = 4, ncol = 2, byrow = T)

maxepocas <- 10000
nepocas <- 0
tol <- 0.1
eepoca <- tol + 1
N <- nrow(xtrain) # numero de amostras
eta <- 0.1
evec <- matrix(nrow = maxepocas, ncol = 1)

i1 <- 1
i4 <- 1
i5 <- 1
i8 <- 1

while ((nepocas < maxepocas) && (eepoca > tol)) {
  ei2 <- 0 # erro quadrado
  xseq <- sample(N) # define sequencia
  for (i in 1:N) {
    irand <- xseq[i] # pega uma amostra aleatoria

    xin <- xtrain[irand, ] # da linha irand com todas as colunas
    yd <- xtrain[irand, ] # y desejado
    i2 <- xin[2]
    i3 <- xin[1]

    yd9 <- yd[2]
    yd10 <- yd[1]

    u6 <- i1 * w61 + i2 * w62 + i3 * w63
    u7 <- i2 * w72 + i3 * 732 + i4 * w74

    i6 <- tanh(u6)
    i7 <- tanh(u7)

    u9 <- i5 * w95 + i6 * w96 + i7 * w97
    u10 <- i6 * w106 + i7 * w107 + i8 * w108

    i9 <- tanh(u9)
    i10 <- tanh(u10)

    e9 <- yd9 - i9
    e10 <- yd10 - i10

    d9 <- e9 * sech2(u9) # delta9, simbolo do impulso unitario
    d10 <- e10 * sech2(u10)

    e6 <- d9 * w96 + d10 * w106
    e7 <- d9 * w97 + d10 * w107

    d6 <- e6 * sech2(u6)
    d7 <- e7 * sech2(u7)

    # agora vem o ajuste
    dw95 <- eta * d9 * i5
    dw96 <- eta * d9 * i6
    dw97 <- eta * d9 * i7

    dw106 <- eta * d10 * i6
    dw107 <- eta * d10 * i7
    dw108 <- eta * d10 * i8

    dw61 <- eta * d6 * i1
    dw62 <- eta * d6 * i2
    dw63 <- eta * d6 * i3

    dw72 <- eta * d7 * i2
    dw73 <- eta * d7 * i3
    dw74 <- eta * d7 * i4

    w95 <- w95 + dw95 # ajuste
    w96 <- w96 + dw96
    w97 <- w97 + dw97

    w106 <- w106 + dw106
    w107 <- w107 + dw107
    w108 <- w108 + dw108

    w61 <- w61 + dw61
    w62 <- w62 + dw62
    w63 <- w63 + dw63

    w72 <- w72 + dw72
    w73 <- w73 + dw73
    w74 <- w74 + dw74

    # todos os pesos ajustados, agora calcula o erro

    ei2 <- ei2 + (e9 * e9) + (e10 * e10)
  }

  # incrementa numero de epocas
  nepocas <- nepocas + 1
  evec[nepocas] <- ei2 / N

  # armazena erro por epocas
  eepoca <- evec[nepocas]
}

plot(evec[1:nepocas], type = "l", lwd = 2)
