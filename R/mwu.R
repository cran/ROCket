
#' Mann-Whitney U test
#'
#' Performs the Mann-Whitney U test with a normal approximation.
#'
#' @param prep A \code{rkt_prep} object.
#' @param alternative The alternative hypothesis type. One of: "two.sided", "less", "greater".
#' @param correct Logical. Whether to apply continuity correction.
#'
#' @return A list of the class "htest".
#' @export
mwu.test <- function(prep, alternative = c("two.sided", "less", "greater"), correct = TRUE) {
  alternative <- match.arg(alternative)

  neg_n <- as.numeric(prep$neg_n)
  pos_n <- as.numeric(prep$pos_n)
  n <- neg_n + pos_n

  # fix global binding issue in package check
  count <- NULL
  value <- NULL

  ties <- data.table(
    value = c(
      get_singularities(prep$pos_ecdf),
      get_singularities(prep$neg_ecdf)
    ),
    count = c(
      round(get_jumps(prep$pos_ecdf) * prep$pos_n),
      round(get_jumps(prep$neg_ecdf) * prep$neg_n)
    )
  )[, .(count = sum(count)), keyby = value]$count

  roc <- rkt_roc(prep)
  AUC <- auc(roc)

  U <- round(AUC * neg_n * pos_n, digits = 1)

  tie_correction <- sum(ties^3 - ties) / (n * (n-1))

  mu <- neg_n * pos_n / 2
  sig <- sqrt(neg_n * pos_n * (n + 1 - tie_correction) / 12)

  correction <- ifelse(correct, 0.5, 0)

  p_value <- switch(
    alternative,
    "less" = pnorm(U + correction, mu, sig),
    "greater" = pnorm(U - correction, mu, sig, lower.tail=FALSE),
    "two.sided" = 2 * min(pnorm(U + correction, mu, sig),
                          pnorm(U - correction, mu, sig, lower.tail=FALSE))
  )

  out <- list(
    method = "Mann-Whitney U test",
    data.name = deparse1(substitute(prep)),
    alternative = alternative,
    statistic = c("U" = U),
    p.value = as.numeric(p_value)
  )

  class(out) <- "htest"

  out
}
