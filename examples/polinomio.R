# função geradora fg(x) = 0.5x^2 + 3x
# saida esperada fg(x) + ruido gaussiano

rm(list = ls())
dev.off()

library("corpcor")

fgx <- function(xin) {
    (0.5 * xin^2) + (3 * xin) + 10
}

X <- runif(n = 20, min = -15, max = 10) # entrada
Y <- fgx(X) + 10 * rnorm(length(X)) # saida esperada do sistema

H <- cbind(X^2, X^1, X^0)
w <- pseudoinverse(H) %*% Y
print(w)

xgrid <- seq(-15, 10, 0.1)
ygrid <- ((0.5 * xgrid^2) + (3 * xgrid) + 10)

Hgrid <- cbind(xgrid^2, xgrid, 1)
yhatgrid <- Hgrid %*% w

plot(
    xgrid,
    ygrid,
    type = "l",
    col = "black",
    main = "Título do gráfico 02",
    xlab = "Eixo X",
    ylab = "Eixo Y",
    ylim = c(-12, 120),
)
lines(xgrid, yhatgrid, col = "red")
lines(xgrid, ygrid, col = "blue")
points(X, Y, col = "black")