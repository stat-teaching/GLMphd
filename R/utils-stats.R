#' Coefficient of Variation
#'
#' Computes the coefficient of variation (CV) for a numeric vector, defined as
#' the ratio of the standard deviation to the mean.
#'
#' @param x A numeric vector.
#' @param na.rm Logical. Should missing values be removed? Default is \code{FALSE}.
#'
#' @details
#' The coefficient of variation is defined as
#' \deqn{CV = \frac{\mathrm{sd}(x)}{\mathrm{mean}(x)}}{}
#' and provides a normalized measure of dispersion relative to the mean.
#'
#' If the mean of \code{x} is zero, the result will be \code{Inf}, \code{-Inf}, or \code{NaN}.
#'
#' @return A single numeric value, the coefficient of variation.
#'
#' @examples
#' cv(c(1, 2, 3, 4, 5))
#' cv(c(10, 12, NA, 14), na.rm = TRUE)
#'
#' @seealso \code{\link[stats]{sd}}, \code{\link[base]{mean}}
#'
#' @export

cv <- function(x, na.rm = FALSE){
  sd(x, na.rm = na.rm)/mean(x, na.rm = na.rm)
}
