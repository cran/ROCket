context("ECDF")
library(ROCket)

# ----------------------------------------------------
# Examine the properties of the rkt_ecdf

scenario_list <- list(
  list(
    x = rnorm(4)
  ),
  list(
    x = rnorm(4),
    w = 1
  ),
  list(
    x = rnorm(4),
    w = c(1, 1, 1, 1)
  ),
  list(
    x = rnorm(4),
    w = 10
  ),
  list(
    x = rnorm(4),
    w = 10 * c(1, 1, 1, 1)
  ),
  list(
    x = rnorm(4),
    w = c(1, 2)
  )
)

for (i in seq_along(scenario_list)) {
  scenario <- scenario_list[[i]]
  test_that(sprintf("ecdf is correct (S: %i)", i), {
    f <- do.call(rkt_ecdf, scenario)

    expect_equal(f(-Inf), 0)
    expect_equal(f(Inf), 1)
    expect_identical(order(environment(f)$x), seq_along(environment(f)$x))
    expect_identical(order(environment(f)$y), seq_along(environment(f)$y))
    expect_identical(environment(f)$x, attr(f, "singularities"))
  })
}

# ----------------------------------------------------
# S3 methods calculate the correct values

scenario_list <- list(
  list(
    x = rnorm(4),
    w = c(1, 1, 1, 1)
  ),
  list(
    x = rnorm(4),
    w = 10 * c(1, 1, 1, 1)
  ),
  list(
    x = rnorm(4),
    w = c(1, 2, 3, 4)
  ),
  list(
    x = rnorm(100),
    w = sample(1:5, 100, T)
  ),
  list(
    x = rnorm(1000),
    w = sample(1:5, 1000, T)
  )
)

for (i in seq_along(scenario_list)) {
  scenario <- scenario_list[[i]]
  test_that(sprintf("S3 methods are correct (S: %i)", i), {
    f <- do.call(rkt_ecdf, scenario)
    x <- rep(scenario$x, scenario$w)

    expect_equal(sum(get_jumps(f)), 1)
    expect_true(all(get_jumps(f) > 0))
    expect_equal(mean(f), mean(x))
    expect_equal(variance(f), variance(x))
  })
}
