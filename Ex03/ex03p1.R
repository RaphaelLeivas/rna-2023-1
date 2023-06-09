source("C:\\dev\\rna-2023-1\\examples\\trainadaline.R")

t <- c(
    0.3,
    0.6,
    0.9,
    1.2,
    1.5,
    1.8,
    2.1,
    2.4,
    2.7,
    3,
    3.3,
    3.6,
    3.9,
    4.2,
    4.5,
    4.8,
    5.1,
    5.4,
    5.7,
    6
)

x <- c(
    0.29552020666134,
    0.564642473395035,
    0.783326909627483,
    0.932039085967226,
    0.997494986604054,
    0.973847630878195,
    0.863209366648874,
    0.675463180551151,
    0.42737988023383,
    0.141120008059868,
    -0.157745694143248,
    -0.442520443294852,
    -0.687766159183973,
    -0.871575772413588,
    -0.977530117665097,
    -0.996164608835841,
    -0.925814682327732,
    -0.772764487555988,
    -0.550685542597638,
    -0.279415498198926
)

y <- c(
    0.588656061998402,
    0.669392742018511,
    0.734998072888245,
    0.779611725790168,
    0.799248495981216,
    0.792154289263459,
    0.758962809994662,
    0.702638954165345,
    0.628213964070149,
    0.54233600241796,
    0.452676291757026,
    0.367243867011544,
    0.293670152244808,
    0.238527268275924,
    0.206740964700471,
    0.201150617349248,
    0.22225559530168,
    0.268170653733204,
    0.334794337220709,
    0.416175350540322
)

x_formatted <- matrix(x, ncol = 1)
y_formatted <- matrix(y, ncol = 1)

retlist <- trainadaline(x_formatted, y_formatted, 0.01, 0.001, 500, 1)
w <- retlist[[1]]
erro <- retlist[[2]]

plot(
    NULL,
    main = "Exercício 3 Parte 1",
    xlab = "x",
    ylab = "y",
    ylim = c(-1, 1),
    xlim = c(-1, 1)
)

points(x, y, col = "blue", lwd = 1)
lines(x, w[2] * x + w[1], col = "red", lwd = 1)