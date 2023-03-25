# função geradora fg(x) = 0.5x^2 + 3x
# saida esperada fg(x) + ruido gaussiano

rm(list = ls())
dev.off()

library("corpcor") # usado para função da pseudoinversa

fgx <- function(xin) {
    (0.5 * xin^2) + (3 * xin) + 10
}

X <- runif(n = 20, min = -15, max = 10) # vetor aleatorio
Y <- fgx(X) + 10 * rnorm(length(X)) # dados de aprendizado
# os pares (xi, yi) são os dados de aprendizado da maquina
# se nao tiver o ruido gaussiano, a maquina aprenderá exatamente a função
# geradora, isto é, yhat(x) = fg(x)

# cria uma matriz em que cada coluna tem os dados dos argumentos
# assim, a coluna 1 da matriz tem o vetor X^2, etc
# H é a matriz com os dados de entrada x
H <- cbind(X^2, X^1, X^0)

# %*% é multiplicação matricial
# ou seja, temos a equação matricial w = H^-1 * Y, que veio de
# Hw = y. Note que Hw = y é o sistema linear
# de todas equações montadas a partir das entradas
w <- pseudoinverse(H) %*% Y

# aqui, w sao os parametros aprendidos. (vetor de parametros w)

## --- aprendizado da RNA se encerra aqui. --- ##
# agora, é somente teste com os parametros aprendidos w

xgrid <- seq(-15, 10, 0.1) # vetor de com a sequencia
# xgrid = [-15, -14.9, -14,8, ... , 9.8, 9.9, 10]

ygrid <- ((0.5 * xgrid^2) + (3 * xgrid) + 10)
# ygrid é a saida da função geradora para cada entrada de xgrid
# assim, ygrid é a saida do sistema (sem o ruido gaussiano)

Hgrid <- cbind(xgrid^2, xgrid^1, xgrid^0)
# aqui estamos fazendo o sistema linear com as entradas xgrid:
# ygrid = Hw

yhatgrid <- Hgrid %*% w

# yhatgrid é y chapeu: é a saida aprendida da maquina

plot(
    NULL,
    main = "Aprendizado Polinômios RNA",
    xlab = "Eixo X",
    ylab = "Eixo Y",
    ylim = c(-12, 120),
    xlim = c(-15, 10)
)
lines(xgrid, yhatgrid, col = "red")
lines(xgrid, ygrid, col = "blue")
points(X, Y, col = "black")

legend(
    x = -10,
    y = 120,
    legend = c(
        "Saida função geradora",
        "Saída aprendida da maquina",
        "Dados de aprendizado"
    ),
    fill = c("blue", "red", "black")
)
