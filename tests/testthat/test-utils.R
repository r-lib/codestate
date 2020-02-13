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

test_that("can temporarily trace a function", {
  f <- function() {
    local_trace(mean, signal("", class = "traced"))
    mean(1:10)
  }
  expect_condition(f(), class = "traced")
  expect_condition(mean(1:10), NA)
})
