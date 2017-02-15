#' dp_dir
#'
#' This function generates an approximation of the Dirichlet Process via the Dirichlet distribution to sample the weights used in the Sethuraman representation.
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
dp_dir <- function(n = 1, M = 1, fun, ...){
    Y <- rgamma(n = n, shape = M / n, rate = 1)
    p <- Y / sum(Y)
    x <- fun(n, ...)
    data.frame(x = x, p = p)
}
