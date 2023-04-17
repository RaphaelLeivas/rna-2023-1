rm(list = ls())
dev.off()

library("plot3D")
source("C:\\dev\\rna-2023-1\\examples\\trainperceptron.R")

n <- 2
N <- 30

xc1 <- cbind(rnorm(N / 2) + 6, rnorm(N / 2) + 6)
xc2 <- cbind(rnorm(N / 2), rnorm(N / 2))

plot(
    NULL,
    main = "Treinamento Perceptron",
    xlab = "x1",
    ylab = "x2",
    ylim = c(-10, 10),
    xlim = c(-5, 15)
)

points(xc1[, 1], xc1[, 2], col = "red")
points(xc2[, 1], xc2[, 2], col = "blue")

xall <- rbind(xc1, xc2)
yall <- rbind(matrix(0, nrow = N), matrix(1, nrow = N))

retlist <- trainperceptron(xall, yall, 0.1, 0.01, 100, 1)
wfinal <- as.matrix(retlist[[1]])
erro <- retlist[[2]]

w0 <- wfinal[1]
w1 <- wfinal[2]
w2 <- wfinal[3]

a <- -(w1 / w2)
b <- w0 / w2

print(a)
print(b)

t <- seq(-5, 15, 0.1)

lines(t, a * t + b, col = "orange")

# plotar a linha do perceptron é uma opção de visualizar a separação. outra forma de fazer isso é ver a separação
# como a fronteira entre os pontos (x, y) no espaço que a rede fala que é 0, e os pontos que ela fala que é 1
# essa fronteira (que não necessariamente é dada por um polinômio) é a curva de separação da rede.

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
        M[ci, cj] <- 1.0 * ((xin %*% wfinal) >= 0) # yperceptron (xin,c(1.5,1,1),1)
    }
}

ribbon3D(seqx1x2, seqx1x2, xlim = c(0, 6), ylim = c(0, 6), M, colkey = FALSE)
scatter3D(xc1[, 1], xc1[, 2], matrix(0, nrow = dim(xc1)[1]), add = TRUE, col = "blue", colkey = FALSE)
scatter3D(xc2[, 1], xc2[, 2], matrix(0, nrow = dim(xc1)[1]), add = TRUE, col = "red", colkey = FALSE)

# note que a fronteira de separação no gráfico 3D não é polinômio, e sim apenas uma curva na fronteira entre
# as regiões que valem 0 e 1. a rede foi aplicada a cada ponto da seqx1x2 no espaço, retornando 0 ou 1 a cada ponto.
