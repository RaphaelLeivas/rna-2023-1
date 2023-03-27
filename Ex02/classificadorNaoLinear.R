x <- seq(-1, 1, by = 0.1)
y <- seq(-1, 1, by = 0.1)
create_grid <- expand.grid(x, y)

circle <- function(x, y) {
    return(sqrt(x^2 + y^2))
}

raio <- 0.6
classe <- 1 * (circle(create_grid$Varl, create_grid$var2) > raio)

plot(
    NULL,
    main = "Classificador Não Linear",
    xlab = "Eixo X",
    ylab = "Eixo Y",
    ylim = c(-1, 1),
    xlim = c(-2, 2),
)

xgrid <- create_grid$Var1
ygrid <- create_grid$Var2

xcircle <- c()
ycircle <- c()

for (i in 1 : length(xgrid)) {
    if (circle(xgrid[i], ygrid[i]) <= raio) {
        xcircle <- append(xcircle, xgrid[i])
        ycircle <- append(ycircle, ygrid[i])
    }
}

create_circle_grid <- expand.grid(xcircle, ycircle)

points(create_grid, col = "red")
points(create_circle_grid, col = "black")

ysup <- sqrt(raio^2 - x^2)
yneg <- - sqrt(raio^2 - x^2)

lines(x, ysup, col = "blue")
lines(x, yneg, col = "blue")
