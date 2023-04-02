rm(list = ls())
dev.off()

source("C:\\dev\\rna-2023-1\\examples\\trainadaline.R")

t <- as.matrix(read.table("C:\\dev\\rna-2023-1\\ex03\\dados\\t"))
x <- as.matrix(read.table("C:\\dev\\rna-2023-1\\ex03\\dados\\x"))
y <- as.matrix(read.table("C:\\dev\\rna-2023-1\\ex03\\dados\\y"))

plot(
    NULL,
    main = "Exercício 3 Parte 2",
    xlab = "x",
    ylab = "y",
    ylim = c(-2, 10),
    xlim = range(t)
)

lines(t[, 1], y[, 1], col = "black", lwd = 1)

retlist <- trainadaline(x, y, 0.01, 0.001, 500, 1)
w <- retlist[[1]]
erro <- retlist[[2]]

yhat <- w[4] * x[, 3] + w[3] * x[, 2] + w[2] * x[, 1] + w[1]

# points(t[, 1], yhat, col = "red", lwd = 1)
lines(t[, 1], yhat, col = "red", lwd = 1)
