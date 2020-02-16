test_that("capture key side effects", {
  dir <- normalizePath(tempdir())
  chunk <- track({
    x <- 1
    old_opt <- options("chonky" = 1)
    old_wd <- setwd(dir)
  })
  options(old_opt)
  setwd(old_wd)

  out <- side_effects(chunk)
  expect_equal(out$bindings$x, 1)
  expect_equal(out$effects$options, list(chonky = 1))
  expect_equal(out$effects$wd, dir)
})

test_that("records random seed if changed", {
  chunk <- track(runif(10))
  out <- side_effects(chunk)
  expect_equal(out$bindings$.Random.seed, .Random.seed)
})

test_that("records package attachment", {
  chunk <- track(library("R6"))
  detach("package:R6")
  out <- side_effects(chunk)
  expect_equal(out$effects$packages, "R6")
})
