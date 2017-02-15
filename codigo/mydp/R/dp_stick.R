#' dp_stick
#'
#' This function generates an approximation of the Dirichlet Process via the stick-breaking construction. Notice that the probabilities won't sum 1.
#' @param n Number of partitions to make.
#' @param M Precision of the baseline distribution.
#' @param fun The distribution to sample from.
#' @param ... Parameters for the distribution.
#'
#' @return Data frame with mass points and the corresponding weights, an approximation to the Dirichlet process.
#' @export
#'
#' @examples dp_stick(n = 10, M = 1, rnorm, mean = 0, sd = 1)
#' @examples dp_stick(n = 100, M = 7, rgamma, shape = 2, rate = 2)
dp_stick <- function(n = 1, M = 1, fun, ...){
    V <- rbeta(n = n, shape1 = 1, shape2 = M)
    SL <- c(1, cumprod(1 - V))[1:n]
    p <- V * SL
    x <- fun(n, ...)
    data.frame(x = x, p = p)
}
