#' Sample Variance
#'
#' @param x An R object.
#' @param ... Further parameters.
#'
#' @return The (biased) sample variance as a numeric value.
#'
#' @seealso \code{\link{variance.rkt_ecdf}}, \code{\link[stats]{var}}
#'
#' @export
variance <- function(x, ...) {
  UseMethod("variance")
}

#' @rdname variance
#' @export
variance.default <- function(x, ...) {
  n <- length(x)
  var(x, ...) * (n - 1) / n
}


#' Calculate the AUC
#'
#' @param x An R object.
#' @param exact Logical. If the exact formula should be used for calculating the AUC instead of numerical approximation.
#' @param lower,upper The limits of integration.
#' @param n The number of integration points.
#' @param ... Further parameters.
#'
#' @return The area under the curve as a numeric value.
#'
#' @export
auc <- function(x, ...) {
  UseMethod("auc")
}
