context("call_with")

test_that("it correctly injects a simple example", {
  fn <- local({ x <- 1; function(y) { x + y } })
  expect_identical(call_with(fn, list(1), list(x = 2)), 3)
})

test_that("it correctly injects a simple example with an environment", {
  fn <- local({ x <- 1; function(y) { x + y } })
  expect_identical(call_with(fn, list(1), list2env(list(x = 2))), 3)
})

test_that("it correctly injects a simple example with an environment override", {
  y <- 2
  fn <- local({ x <- 1; y <- 1; function(z) { c(x, y, z) } })
  expect_identical(call_with(fn, list(1), list2env(list(x = 2))), c(2, 2, 1))
})

test_that("it correctly injects a simple example with a list", {
  y <- 2
  fn <- local({ x <- 1; y <- 1; function(z) { c(x, y, z) } })
  expect_identical(call_with(fn, list(1), list(x = 2)), c(2, 1, 1))
})

test_that("it does not modify the original function's environment", {
  fn <- local({ x <- 1; function(y) { x + y } })
  call_with(fn, list(1), list(x = 2))
  expect_identical(fn(1), 2)
})

