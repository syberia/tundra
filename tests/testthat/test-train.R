context("tundraContainer$train")

describe("R6", {
  test_that("it cannot be trained twice", {
    container <- tundraContainer$new("foo")
    container$train(iris)
    expect_error(container$train(iris), "has already been trained")
  })

  test_that("it can train a simple example", {
    container <- tundraContainer$new("foo", function(data) {
      output$data <- data
    })

    container$train(iris)
    expect_identical(container$.output$data, iris)
  })
})

describe("refClass", {
  test_that("it cannot be trained twice", {
    container <- tundra_container('foo')
    container$train(iris)
    expect_error(container$train(iris), "has already been trained")
  })

  test_that("it can train a simple example", {
    container <- tundra_container("foo", function(data) {
      output$data <- data
    })

    container$train(iris)
    expect_identical(container$output$data, iris)
  })
})
