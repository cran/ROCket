#' ROC points
#'
#' Calculate the ROC points for all meaningful cutoff values based on predicted scores.
#' 
#' In a situation where many of the predicted scores have the same value it might be easier and faster to use aggregated data.
#'
#' @param scores Numeric vector containing the predicted scores.
#' @param positives Numeric vector of the same length as \code{scores}. 
#' The number of positive entities associated with each score. 
#' If data is not aggregated, a vector of 0's and 1's.
#' @param negatives Similar to \code{positives}. Defaults to \code{totals - positives}.
#' @param totals How many times each score was predicted. Defaults to 1 (assuming data is not aggregated). 
#' If any value in \code{positives} is greater than 1 (aggregated data), \code{totals} must be a vector.
#' Not needed if \code{negatives} is supplied.
#' @param ... Further parameters.
#' @param x An environment of class \code{rkt_prep} for S3 methods.
#'
#' @return An environment of class \code{rkt_prep}.
#' @export
#'
#' @examples
#' require(ROCket)
#' 
#' plot(rkt_prep(1:4, c(0, 1, 0, 1)))
#' plot(rkt_prep(1:4, c(0, 1000, 0, 1000), totals = 1000))
#' plot(rkt_prep(1:4, c(100, 200, 300, 400), totals = c(1000, 800, 600, 400)))
rkt_prep <- function(scores, positives, negatives = totals - positives, totals = 1) {
  if(missing(scores)) stop("\"scores\" is required")
  if(missing(positives)) stop("\"positives\" is required")
  
  if (!missing(negatives) && !missing(totals)) {
    stopifnot(all(positives + negatives == totals))
  }
  
  stopifnot(all(totals >= 0))
  stopifnot(all(positives >= 0))
  stopifnot(all(negatives >= 0))

  out <- new.env(parent = emptyenv())

  out$pos_ecdf <- rkt_ecdf(scores, positives)
  out$neg_ecdf <- rkt_ecdf(scores, negatives)

  out$pos_n <- sum(positives)
  out$neg_n <- sum(negatives)

  class(out) <- c("rkt_prep", class(out))

  out
}

#' @export
#' @rdname rkt_prep
print.rkt_prep <- function(x, ...) {
  cat(".:: ROCket Prep Object \n")
  cat("Positives (pos_n):", x$pos_n, "\n")
  cat("Negatives (neg_n):", x$neg_n, "\n")
  cat("Pos ECDF (pos_ecdf):",class(x$pos_ecdf), "\n")
  cat("Neg ECDF (neg_ecdf):",class(x$neg_ecdf), "\n")
}

#' @export
#' @rdname rkt_prep
plot.rkt_prep <- function(x, ...) {
  inargs <- list(...)

  s <- get_cutoffs(x)

  outargs <- list(x = 1 - x$neg_ecdf(s),
                  y = 1 - x$pos_ecdf(s),
                  xlim = c(0, 1),
                  ylim = c(0, 1),
                  xlab = expression(FPR),
                  ylab = expression(TPR),
                  main = 'ROC',
                  v = c(0, 1),
                  h = c(0, 1))
  outargs[names(inargs)] <- inargs

  do.call(plot_points, outargs)

  invisible()
}

