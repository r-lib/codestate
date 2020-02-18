test_that("track getting and setting bindings", {
  skip_if(covr::in_covr())

  td <- track(x <- 1)$tracking
  expect_equal(td, acc_tibble("set", "binding", "x"))

  td <- track(x)$tracking
  expect_equal(td, acc_tibble("get", "binding", "x"))
})

test_that("don't track bindings from packages", {
  td <- track(data.frame(x = 1L))$tracking
  expect_equal(nrow(td), 0L)
})

test_that("don't track get if set first", {
  testthat::skip_if(covr::in_covr())
  td <- track({x <- 1; x})$tracking
  expect_equal(td, acc_tibble("set", "binding", "x"))
})

test_that("track getting and setting options", {
  testthat::skip_if(covr::in_covr())
  td <- track(data.frame(x = "a"))$tracking
  expect_equal(td, acc_tibble("get", "option", "stringsAsFactors"))

  f <- function() {
    options(hadley = TRUE)
  }
  td <- track((!!f)())$tracking
  expect_equal(td, acc_tibble("set", "option", "hadley"))
})
