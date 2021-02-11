context("AUC")
library(ROCket)

methods_tab <- roc_methods()

# ----------------------------------------------------
# Compare the calculated AUC using the exact formula
# with the approximated version

scenario_list <- list(
  list(
    scores = c(1, 2, 3, 4),
    positives = c(0, 1, 0, 1),
    totals = 1
  ),
  list(
    scores = c(1, 2, 3, 4),
    positives = 10 * c(0, 1, 0, 1),
    totals = 10
  ),
  list(
    scores = c(1, 2, 3, 4),
    positives = c(4, 1, 3, 8),
    totals = 10
  )
  ,
  list(
    scores = rep(rnorm(20), each = 5),
    positives = rep(rbinom(20, 1, 0.5), each = 5),
    totals = 1
  )
)

for (i in seq_along(scenario_list)) {
  scenario <- scenario_list[[i]]
  for (j in 1:nrow(methods_tab)) {
    method_nr <- methods_tab[j, nr]
    test_that(sprintf("exact AUC is equal to integrated (S: %i, M: %i)", i, method_nr), {
      prep <- do.call(rkt_prep, scenario)
      roc <- rkt_roc(prep, method = method_nr)
      expect_equal(auc(roc), auc(roc, exact = FALSE), tolerance = 0.0001)
    })
  }
}

