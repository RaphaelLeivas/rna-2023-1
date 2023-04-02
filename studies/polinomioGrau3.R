rm(list = ls())

library("corpcor")

# gera dados aleatorios, com uma função geradora para deixar mais visual
x1 <- seq(from = -10, to = 10, by = 1)
x2 <- x1^3 + 1500 * rnorm(21, 0.1, 0.1) # geradora com ruido

plot(
    NULL,
    main = "Polinômio Grau 3",
    xlab = "x1",
    ylab = "x2",
    ylim = range(x2),
    xlim = range(x1)
)

points(x1, x2, col = "black", lwd = 1)

# a partir do modelo teórico, sabemos que precisamos de
# w = H^+ y

# primeiro montamos a matriz H com as entradas todas
H <- cbind(x1^3, x1^2, x1^1, x1^0)

# agora o vetor y
y <- x2

# faz a operação teórica
w <- pseudoinverse(H) %*% y

print(w)

# a função aprendida é
yhat <- w[1] * x1^3 + w[2] * x1^2 + w[3] * x1^1 + w[4] * x1^0

lines(x1, yhat, col = "red", lwd = 1)

# nesse caso nao temos aprendizado em loop com a função custo: o modelo
# teórico permitiu fazer a igualdade w = H^+ y diretamente (assim J = 0)
# sem precisar de loop e realimentação a partir do custo J