library(testthat)
require(testthat)

source("functions/regression-functions.R")

load("../data/regression.RData")


#
test_that( "RSS Test", {
  x <- residual_sum_squares(rel_tv_sales)
  answer = (summary(rel_tv_sales)$sigma)^2 * 198
  expect_that(x, equals(answer, tolerance = .1))
  })

# Should be 5417
test_that("TSS Test", {
  x <- total_sum_squares(rel_tv_sales)
  expect_that(x, equals(5417, tolerance = .1))
  })

#Should be .6119 for rel_tv_sales, non-adjusted
test_that("R Squared Test", {
  x <- r_squared(rel_tv_sales)
  answer = summary(rel_tv_sales)$r.squared
  expect_that(x, equals(answer, tolerance = .1))
  })

#Should be 312.1 for rel_tv_sales
test_that("F Statistic Test", {
  x = f_statistic(rel_tv_sales)
  expect_that(x, equals(312.1, tolerance = .1))
  })

#Expect 3.259 for rel_tv_sales
test_that("Residual Std Error Test", {
  x <- residual_std_error(rel_tv_sales)
  expect_that(x, equals(3.259, tolerance = .1))
  })
