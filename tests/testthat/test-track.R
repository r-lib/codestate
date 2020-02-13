test_that("track getting and setting bindings", {
  td <- track(x <- 1)
  expect_equal(td, acc_tibble("set", "binding", "x"))

  td <- track(x)
  expect_equal(td, acc_tibble("get", "binding", "x"))
})

test_that("don't track bindings from packages", {
  td <- track(data.frame(x = 1L))
  expect_equal(nrow(td), 0L)
})

test_that("track getting and setting options", {
  td <- track(data.frame(x = "a"))
  expect_equal(td, acc_tibble("get", "option", "stringsAsFactors"))

  f <- function() {
    options(hadley = TRUE)
  }
  td <- track((!!f)())
  expect_equal(td, acc_tibble("set", "option", "hadley"))
})
