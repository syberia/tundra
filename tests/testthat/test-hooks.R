context("hooks")

let(container, tundraContainer$new("foo"))

describe("Invalid inputs", {
  test_that("it errors if an invalid hook name is provided", {
    expect_error(container$add_hook("foo"))
  })
})

test_that("it can add a simple hook", {
  env  <- list2env(list(x = 0))
  hook <- function() { env$x <- 1 }
  container$add_hook("train_pre_munge", hook)
  container$train(iris)
  expect_equal(env$x, 1)
})


