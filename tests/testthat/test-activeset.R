## tests/testthat/test-activeset.R
library(isotone)
library(broom)
library(ggplot2)

test_that("activeset helper methods", {

  set.seed(456)
  n <- 20
  y <- rnorm(n, mean = rev(seq_len(n)) / 4)

  btota <- cbind(1:(n - 1), 2:n)     # ordre total
  w     <- rep(1, n)                 # poids unitaires

  ac <- isotone::activeSet(
    btota,
    "LS",              # <- solver pré‑défini
    y       = y,
    weights = w        # <- argument exact attendu
  )

  td <- tidy(ac)
  expect_s3_class(td, "tbl_df")
  expect_equal(nrow(td), n)

  aug <- augment(ac)
  expect_equal(aug$.fitted, ac$x)
  expect_equal(aug$.resid,  y - ac$x)

  gl <- glance(ac)
  expect_equal(gl$n, n)
  expect_equal(gl$rss, sum((y - ac$x)^2))

  p <- autoplot(ac)
  expect_s3_class(p, "ggplot")
})
