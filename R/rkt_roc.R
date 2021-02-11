#' Empirical estimate of the  ROC
#'
#' Calculate the empirical estimate of the ROC from raw sample or aggregated data.
#'
#' @param prep A \code{rkt_prep} object.
#' @param method A number specifying the type of ROC estimate. Possible values can be viewed with \code{show_methods()}.
#' @param x An object of class \code{rkt_roc}.
#' @param ... Further parameters passed to \code{\link{plot}} and \code{\link{lines}}
#'
#' @return An object of class \code{rkt_roc}, i.e. a function or a list of two functions (for method = 1).
#' @export
#'
#' @examples
#' require(ROCket)
#'
#' scores <- c(1, 2, 3, 4)
#' positives <- c(0, 1, 0, 1)
#' prep <- rkt_prep(scores, positives)
#'
#' roc1 <- rkt_roc(prep, method = 1)
#' roc2 <- rkt_roc(prep, method = 2)
#' roc3 <- rkt_roc(prep, method = 3)
#'
#' plot(roc1)
#' plot(roc2)
#' plot(roc3)
rkt_roc <- function(prep, method = 1) {

  method_fun <- tryCatch(
    get_method_fun(method),
    error = function(e) stop("Couldn't find this method")
  )

  out <- method_fun(prep)

  class(out) <- c("rkt_roc", class(out))
  attr(out, "prep") <- prep
  attr(out, "method") <- method

  out
}

#' @export
#' @rdname rkt_roc
print.rkt_roc <- function(x, ...) {
  method <- get_method(x)
  cat(".:: ROCket ROC Object \n")
  cat("Method:", method, "-", get_method_desc(method), "\n")
}

#' @export
#' @rdname rkt_roc
plot.rkt_roc <- function(x, ...) {
  inargs <- list(...)

  outargs <- list(f = x,
                  xlim = c(0, 1),
                  ylim = c(0, 1),
                  main = 'ROC',
                  draw_area = TRUE,
                  v = c(0, 1),
                  h = c(0, 1))

  if (inherits(x, 'curve')) {
    outargs[["xlab"]] <- expression(FPR)
    outargs[["ylab"]] <- expression(TPR)
    outargs[["slim"]] <- range(get_cutoffs(attr(x, "prep", exact = TRUE)), finite = TRUE) + c(-1, 0)
  } else if (inherits(x, 'function')) {
    outargs[["xlab"]] <- expression(t)
    outargs[["ylab"]] <- expression(ROC(t))
  } else {
    stop("Unknown rkt_roc subclass.")
  }

  outargs[names(inargs)] <- inargs

  if (inherits(x, 'curve')) {
    do.call(plot_curve, outargs)
  } else if (inherits(x, 'function')) {
    do.call(plot_function, outargs)
  }

  invisible()
}

