source("C:\\dev\\rna-2023-1\\examples\\trainadaline.R")

# aproximação linear do sen(x)
# saida esperada: y = 4x + 2

# vetor em que cada elemento é um tempo do eixo t
# gera valores entre 0 e 2pi, com intervalos de 0.1pi
t <- matrix(seq(0, 2 * pi, 0.1 * pi), ncol = 1)

# suponha que inseriram o sinal sin(t) nesse sistema descrito por 4x + 2
# assim, x = sin(t)
x <- sin(t)

y <- matrix(4 * x + 2, ncol = 1)

# temos um conjunto de dados (xi, yi) para aprendizagem
plot(
    NULL,
    main = "Treinamento Adaline Univariado",
    xlab = "Eixo X",
    ylab = "Eixo Y",
    ylim = c(-2, 20),
    xlim = c(0, 7)
)
points(x, y, col = "black")

# chama a função do treinamento Adaline:
# tenho esses dados x e y, parametro n (eta) = 0.01
# tolerancia de 0.01
# maximo 50 loops
# com a flag 1 para add uma coluna
# aí a função me retorna os parametros aprendidos para a função yhat(x)
retlist <- trainadaline(x, y, 0.01, 0.01, 50, 1)
w <- retlist[[1]]
erro <- retlist[[2]]

# a função aprendida é y^(x) = w[1]x^1 + w[0]x^0, pois w é um vetor de duas posições
yhat <- w[2] * t + 2

lines(t, yhat, col = "blue")

