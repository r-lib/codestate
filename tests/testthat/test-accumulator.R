test_that("can record in active accumulator", {
  local_acc()

  acc_add("a", "b", "c")
  expect_equal(acc_data(), acc_tibble("a", "b", "c"))
})

test_that("otherwise fails", {
  expect_error(acc_add("a", "b", "c"), "active")
})
