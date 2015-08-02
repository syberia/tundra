context("tundraContainer$train")

test_that("it can train a simple example", {
  container <- tundraContainer$new("foo", function(data) {
    output$data <- data
  })

  container$train(iris)
  expect_identical(container$.output$data, iris)
})

