## Tests unité   gpava
library(isotone)
library(broom)
library(ggplot2)   # <-  pour le générique autoplot()

test_that("gpava helper methods", {

  set.seed(123)
  n <- 25
  y <- rnorm(n, mean = seq_len(n) / 3)

  gp <- isotone::gpava(y = y)   # prédicteur implicite 1:n

  # tidy
  td <- tidy(gp)
  expect_s3_class(td, "tbl_df")
  expect_equal(nrow(td), n)

  # augment
  aug <- augment(gp)
  expect_equal(aug$.fitted, gp$x)
  expect_equal(aug$.resid,  y - gp$x)

  # glance
  gl <- glance(gp)
  expect_equal(gl$n, n)
  expect_equal(gl$rss, sum((y - gp$x)^2))

  # autoplot
  p <- autoplot(gp)
  expect_s3_class(p, "ggplot")
})
