test_that("on_exit runs in caller env", {
  a <- 1
  local <- function(x) {
    on_exit(a <<- !!x)
  }
  f <- function() {
    local(2)
    3
  }

  f()
  expect_equal(a, 2)
})
