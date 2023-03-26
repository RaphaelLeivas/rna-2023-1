rm(list = ls())
dev.off()

source("C:\\dev\\rna-2023-1\\examples\\trainadaline.R")

# queremos modelar um sistema com 4 entrada e saida esperada de
# y(x4, x3, x2, x1) = a4x4 + a3x3 + a2x2 + a1x1 + a0
# temos 4 entradas, e o modelo aprende 5 parametros (a4 até a0)

# vetor em que cada elemento é um tempo do eixo t
# gera valores entre 0 e 2pi, com intervalos de 0.1pi
t <- seq(0, 2 * pi, 0.1 * pi)

# gera valores aleatorios para as entradas
x1 <- matrix(sin(t) + cos(t), ncol = 1)
x2 <- matrix(tanh(t), ncol = 1)
x3 <- matrix(sin(4 * t), ncol = 1)
x4 <- matrix(abs(sin(t)), ncol = 1)

# para teste, assumimos que o sistema possui os parâmetros a seguir
# a = [a4, a3, a2, a1, a0]
# a = [3.2, 0.8, 2, 1, pi/2]

# logo, a saida y(x) esperada do sistema é
y <- pi / 2 + 1 * x1 + 2 * x2 + 0.8 * x3 + 3.2 * x4

# cria matriz de dados de entrada 
X <- cbind(x1, x2, x3, x4)

# temos um conjunto de dados (X, yi) para aprendizagem
plot(
    NULL,
    main = "Treinamento Adaline Multivariado",
    xlab = "Eixo X",
    ylab = "Eixo Y",
    ylim = c(0, 8),
    xlim = c(0, 6.28)
)
# points(x1, y, col = "black")
# points(x2, y, col = "black")
# points(x3, y, col = "black")
# points(x4, y, col = "black")


retlist <- trainadaline(X, y, 0.01, 0.01, 50, 1)
w <- retlist[[1]]
erro <- retlist[[2]]

# aprendizado do modelo acabou aqui, aprendendo o parametro w. agora vamos testar
# o aprendizado feito

# gera novos dados de entrada
ttest <- seq(0, 2 * pi, 0.1 * pi)

# gera valores aleatorios para as entradas
x1test <- matrix(sin(ttest) + cos(ttest), ncol = 1)
x2test <- matrix(tanh(ttest), ncol = 1)
x3test <- matrix(sin(4 * ttest), ncol = 1)
x4test <- matrix(abs(sin(ttest)), ncol = 1)
Xtest <- cbind(1, x1test, x2test, x3test, x4test)

# a saida aprendida, com os dados de teste, é
ytest <- Xtest %*% w

# a saida real do sistema é y

lines(ttest, ytest, col = "red", lw = 1)
lines(t, y, col = "blue", lw = 1)
