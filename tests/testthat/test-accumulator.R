test_that("can record in active accumulator", {
  local_acc()

  acc_add("a", "b", "c")
  expect_equal(acc_data(), acc_tibble("a", "b", "c"))
})

test_that("otherwise fails", {
  expect_error(acc_add("a", "b", "c"), "active")
})

test_that("action and name are recycled to same length as value", {
  local_acc()

  acc_add("a", "b", letters[1:3])
  expect_equal(acc_data(), acc_tibble("a", "b", letters[1:3]))
})
