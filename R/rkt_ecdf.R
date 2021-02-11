#' Empirical estimate of the CDF
#'
#' Calculate an empirical cumulative distribution function based on a sample \code{x} and optionally a vector \code{w} of weights.
#' 
#' The weights vector \code{w} can contain the counts of each distinct value in \code{x}, this is the most natural use case. 
#' In general the weights are describing the jumps of the final ecdf. Normalization is handled internally.
#' 
#' If \code{x} contains duplicates, corresponding values in \code{w} will be summed up.
#' Only positive weights are allowed. Elements in \code{x} with non-positive weights will be ignored.
#'
#' @param x Numeric vector containing the sample. Alternatively, if \code{w} is supplied, distinct values within the sample. For S3 methods, a function of class \code{rkt_ecdf}.
#' @param w Optional. Numeric vector containing the weights of each value in \code{x}.
#' @param ... Further parameters.
#' 
#' @return A function of class \code{rkt_ecdf}.
#' @export
#'
#' @examples
#' require(ROCket)
#' 
#' plot(rkt_ecdf(rnorm(100)))
#' plot(rkt_ecdf(c(0, 1)))
#' plot(rkt_ecdf(c(0, 1), c(1, 10)))
rkt_ecdf <- function(x, w) {
  if (missing(w)) {
    df <- data.table(x)
    df <- df[, .(w = .N), keyby = x]
  } else if (is.numeric(w) && any(duplicated(x))) {
    df <- data.table(x, w)
    df <- df[, .(w = sum(w)), keyby = x]
  } else if (is.numeric(w)) {
    df <- data.table(x, w)
    df <- df[order(x)]
  } else {
    stop("\"w\" needs to be numeric")
  }
  
  df <- df[!is.na(x) & w > 0]
  total <- sum(df$w)
  
  stopifnot(total > 0)
  df[, y := cumsum(w) / total]
  
  out <- approxfun(df$x, df$y,
                   method = "constant",
                   yleft = 0,
                   yright = 1,
                   f = 0,
                   ties = "ordered")
  class(out) <- c("rkt_ecdf", class(out))
  attr(out, "singularities") <- df$x
  
  out
}

#' @export
#' @rdname rkt_ecdf
print.rkt_ecdf <- function(x, ...) {
  cat(".:: ROCket ECDF Object \n")
  cat("Class:", class(x), "\n")
}

#' @rdname rkt_ecdf
#' @export
mean.rkt_ecdf <- function(x, ...) {
  weighted.mean(environment(x)$x, get_jumps(x))
}

#' @rdname rkt_ecdf
#' @export
variance.rkt_ecdf <- function(x, ...) {
  weighted.mean((environment(x)$x - mean(x))^2, get_jumps(x))
}

#' @export
#' @rdname rkt_ecdf
plot.rkt_ecdf <- function(x, ...) {
  inargs <- list(...)
  
  outargs <- list(f = x,
                  ylim = c(0, 1),
                  xlab = expression(x),
                  ylab = expression(F[n](x)),
                  main = 'ECDF',
                  draw_area = FALSE,
                  h = c(0, 1))
  outargs[names(inargs)] <- inargs
  
  do.call(plot_function, outargs)
  
  invisible()
}
