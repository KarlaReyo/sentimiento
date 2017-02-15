#' dp_urn
#'
#' This function approximates a Dirichlet Process by a generalization of the
#' Polya urn scheme.
#' @param n Number of times the scheme is set to run.
#' @param M Precision of the baseline density.
#' @param fun Density that models the data.
#' @param ... Parameters for the density.
#'
#' @return A data frame containing the mass points with their weights, an approximation to a Dirichlet process.
#' @export
#'
#' @examples dp_urn(n = 10, M = 3, rnorm, mean = 0, sd = 1)
#' @examples dp_urn(n = 10, M = 3, rgamma, shape = 2, rate = 2)
dp_urn <- function(n = 1, M = 1, fun, ...){
    x <- fun(1, ...)
    i <- 1
    while(i < n){
        aalpha <- M / (M + i)
        p <- c(aalpha, rep(1 / (M + i), i))
        y <- sample(x = c(fun(1, ...), x), size = 1, prob = p)
        x <- c(x, y)
        i <- i + 1
    }
    y <- unique(x)
    p <- sapply(seq_along(y), function(z) sum(x == y[z]) / length(x))
    data.frame(x = y, p = p)
}
