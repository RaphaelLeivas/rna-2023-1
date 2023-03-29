yperceptron <- function(xvec, w) {
    u <- xvec %% w
    y <- 1.0 * (u >= 0)
    return(as.matrix(y))
}
