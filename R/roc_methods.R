
#' Available ROC estimation methods
#'
#' Show the implemented ROC estimation methods.
#'
#' @return A \code{\link{data.table}} containing the number and a short description of each implemented method.
#' @export
show_methods <- function() {
  roc_methods()[, .(nr, desc)]
}

roc_methods <- function() {
  method_list <-
    list(
      list(
        nr = 1,
        desc = "ROC Curve (empirical)",
        fun_name = "rkt_roc_c_emp",
        out_type = "curve"
      ),
      list(
        nr = 2,
        desc = "ROC Function (empirical)",
        fun_name = "rkt_roc_f_emp",
        out_type = "function"
      ),
      list(
        nr = 3,
        desc = "ROC Function (placement values)",
        fun_name = "rkt_roc_f_pv",
        out_type = "function"
      ),
      list(
        nr = 4,
        desc = "ROC Function (binormal)",
        fun_name = "rkt_roc_f_binormal",
        out_type = "function"
      )
    )

  out <- rbindlist(method_list)
  out[]
}

get_method_fun <- function(method_nr) {
  get(roc_methods()[nr == method_nr, fun_name])
}

get_method_desc <- function(method_nr) {
  roc_methods()[nr == method_nr, desc]
}


rkt_roc_c_emp <- function(prep) {
  pos_ecdf <- prep$pos_ecdf
  neg_ecdf <- prep$neg_ecdf

  s <- sort(unique(c(environment(pos_ecdf)$x, environment(neg_ecdf)$x)))
  s <- c(min(s) - 1, s)

  # FPR
  x <- approxfun(s, 1 - neg_ecdf(s),
                 method = "linear",
                 yleft = 1,
                 yright = 0,
                 f = 0,
                 ties = "ordered")
  attr(x, "singularities") <- s

  # TPR
  y <- approxfun(s, 1 - pos_ecdf(s),
                 method = "linear",
                 yleft = 1,
                 yright = 0,
                 f = 0,
                 ties = "ordered")
  attr(y, "singularities") <- s

  out <- structure(
    list(x = x, y = y),
    class = c("curve")
  )

  out
}

rkt_roc_f_emp <- function(prep) {
  pos_ecdf <- prep$pos_ecdf
  neg_ecdf <- prep$neg_ecdf

  x <- rev(1 - c(0, environment(neg_ecdf)$y))
  cutoffs <- rev(c(-Inf, environment(neg_ecdf)$x))
  y <- 1 - pos_ecdf(cutoffs)

  out <- approxfun(x, y,
                   method = "constant",
                   f = 0,
                   ties = "ordered")
  attr(out, "singularities") <- x

  out
}

rkt_roc_f_pv <- function(prep) {
  x <- 1 - prep$neg_ecdf(environment(prep$pos_ecdf)$x)
  counts <- get_jumps(prep$pos_ecdf)

  rkt_ecdf(x, counts)
}

new_rkt_roc_f_binormal <- function(a, b) {
  function(t) pnorm(a + b * qnorm(t))
}

rkt_roc_f_binormal <- function(prep) {
  pos_mean <- mean(prep$pos_ecdf)
  neg_mean <- mean(prep$neg_ecdf)

  pos_sd <- sqrt(variance(prep$pos_ecdf) * prep$pos_n / (prep$pos_n - 1))
  neg_sd <- sqrt(variance(prep$neg_ecdf) * prep$neg_n / (prep$neg_n - 1))

  a <- (pos_mean - neg_mean) / pos_sd
  b <- neg_sd / pos_sd
  out <- new_rkt_roc_f_binormal(a, b)

  out
}

