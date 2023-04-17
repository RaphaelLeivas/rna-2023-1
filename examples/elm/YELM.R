YELM <- function(xin, Z, W, par) {
    if (par == 1) {
        xin <- cbind(1, xin)
    }

    H <- tanh(xin %*% Z)
    Yhat <- sign(H %*% W)

    return(Yhat)
}