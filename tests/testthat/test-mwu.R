context("Mann–Whitney U test")
library(ROCket)
library(data.table)

set.seed(111)

# ----------------------------------------------------
# Compare the p-values of the mwu.test with the wilcox.test

N <- 1000
data <- data.table(score = c(rpois(N, 1), rpois(N, 1.2)), pos = rep(0:1, each = N))
data_agg <- data[, .(totals = .N, A = .N - sum(pos), B = sum(pos)), keyby = score]

prep <- rkt_prep(scores = data_agg$score, negatives = data_agg$A, positives = data_agg$B)

tmp <- expand.grid(
  alternative = c("two.sided", "less", "greater"),
  correct = c(TRUE, FALSE),
  stringsAsFactors = FALSE
)

scenario_list <- split(tmp, 1:nrow(tmp))

for (i in seq_along(scenario_list)) {
  scenario <- as.list(scenario_list[[i]])

  args_A <- c(scenario, prep = prep)
  args_B <- c(scenario, list(x = data[pos == 1, score], y = data[pos == 0, score]))

  test_that(sprintf("mwu.test and wilcox.test give the same p-value (S: %i)", i), {

    expect_equal(
      do.call(mwu.test, args_A)$p.value,
      do.call(wilcox.test, args_B)$p.value,
      tolerance = 0.00000001
    )
  })

}

