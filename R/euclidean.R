#' Euclidean Algorithm
#'
#' Computes the greatest common divisor (GCD) of two numbers using the Euclidean algorithm.
#'
#' @param a A numeric scalar or integer.
#' @param b A numeric scalar or integer.
#' @return An integer giving the greatest common divisor of `a` and `b`.
#' @export
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
euclidean <- function(a, b) {
  stopifnot(is.numeric(a), is.numeric(b), length(a) == 1, length(b) == 1)

  a <- abs(a)
  b <- abs(b)

  while (b != 0) {
    temp <- b
    b <- a %% b
    a <- temp
  }

  return(a)
}
euclidean(123612, 13892347912)
euclidean(100, 1000)
