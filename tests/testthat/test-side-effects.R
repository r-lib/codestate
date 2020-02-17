test_that("capture key side effects", {
  dir <- normalizePath(tempdir())
  chunk <- track({
    old_opt <- options("chonky" = 1)
    old_wd <- setwd(dir)
  })
  options(old_opt)
  setwd(old_wd)

  effects <- side_effects(chunk)
  expect_equal(effects$bindings, c("old_opt", "old_wd"))
  expect_equal(effects$options, list(chonky = 1))
  expect_equal(effects$wd, dir)
})

test_that("records random seed if changed", {
  chunk <- track(runif(10))
  effects <- side_effects(chunk)
  expect_equal(effects$seed, .Random.seed)

  chunk <- track(1 + 1)
  effects <- side_effects(chunk)
  expect_equal(effects$seed, NULL)
})

test_that("records package attachment", {
  chunk <- track(library("R6"))
  detach("package:R6")
  effects <- side_effects(chunk)
  expect_equal(effects$packages, "R6")
})
