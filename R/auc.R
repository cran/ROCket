#' @include generics.R

#' @rdname auc
#' @export
auc.function <- function(x, ...) {
  integrate(x, ...)$value
}

#' @rdname auc
#' @export
auc.curve <- function(x, lower, upper, n = 10000, ...) {
  f <- x
  s <- seq(lower, upper, length.out = n)
  x <- f$x(s)
  y <- f$y(s)
  return(sum(diff(x) * 0.5 * (y[-length(y)] + y[-1])))
}

#' @rdname auc
#' @export
auc.rkt_roc <- function(x, exact = TRUE, ...) {
  roc <- x
  method <- get_method(roc)
  cutoffs <- get_cutoffs(attr(roc, "prep", exact = TRUE))

  if (!exact) {
    if (inherits(roc, "function")) {
      return(auc.function(roc, 0, 1, ...))
    } else if (inherits(roc, "curve")) {
      slim <- range(cutoffs, finite = TRUE) + c(-1, 0)
      return(-auc.curve(roc, slim[1], slim[2]))
    } else {
      stop("Unhandled object class.")
    }
  }

  if (method == 1) {
    x <- roc$x(cutoffs)
    y <- roc$y(cutoffs)
    return(sum(diff(x) * 0.5 * (y[-length(y)] + y[-1])))
  } else if (method %in% c(2, 3)) {
    x <- c(0, environment(roc)$x, 1)
    y <- c(0, environment(roc)$y)
    return(sum(diff(x) * y))
  } else if (method == 4) {
    a <- environment(roc)$a
    b <- environment(roc)$b
    return(pnorm(a/sqrt(1 + b^2)))
  } else {
    stop("Unhandled method.")
  }
}
