# tracking ----------------------------------------------------------------

test_that("track getting and setting bindings", {
  skip_if(covr::in_covr())

  td <- cache(x <- 1)$tracking
  expect_equal(td, acc_tibble("set", "binding", "x"))

  td <- cache(x)$tracking
  expect_equal(td, acc_tibble("get", "binding", "x"))
})

test_that("don't track bindings from packages", {
  td <- cache(data.frame(x = 1L))$tracking
  expect_equal(nrow(td), 0L)
})

test_that("don't track get if set first", {
  testthat::skip_if(covr::in_covr())
  td <- cache({x <- 1; x})$tracking
  expect_equal(td, acc_tibble("set", "binding", "x"))
})

test_that("track getting and setting options", {
  testthat::skip_if(covr::in_covr())
  td <- cache(data.frame(x = "a"))$tracking
  expect_equal(td, acc_tibble("get", "option", "stringsAsFactors"))

  f <- function() {
    options(hadley = TRUE)
  }
  td <- cache((!!f)())$tracking
  expect_equal(td, acc_tibble("set", "option", "hadley"))
})


# side-effects ------------------------------------------------------------

test_that("capture key side effects", {
  dir <- normalizePath(tempdir())
  chunk <- cache({
    old_opt <- options("chonky" = 1)
    old_wd <- setwd(dir)
  })
  options(old_opt)
  setwd(old_wd)

  expect_equal(chunk$effects$bindings, c("old_opt", "old_wd"))
  expect_equal(chunk$effects$options, list(chonky = 1))
  expect_equal(chunk$effects$wd, dir)
})

test_that("records random seed if changed", {
  chunk <- cache(runif(10))
  expect_equal(chunk$effects$seed, .Random.seed)

  chunk <- cache(1 + 1)
  expect_equal(chunk$effects$seed, NULL)
})

test_that("records package attachment", {
  chunk <- cache(library("R6"))
  detach("package:R6")
  expect_equal(chunk$effects$packages, "R6")
})
