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

test_that("find_binding() detects if in package", {
  bnd <- find_binding("data.frame", current_env())
  expect_equal(bnd$in_package, TRUE)
  expect_equal(bnd$val, data.frame)
})

test_that("find_binding() errors if not found", {
  expect_error(find_binding("XXXXXXXXXXXXX", current_env()), "not found")
})
