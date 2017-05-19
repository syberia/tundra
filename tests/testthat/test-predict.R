context("tundraContainer$predict")

test_that("it can predict on a simple example", {
  container <- tundraContainer$new("foo", function(data) {
    output$model <- lm(Sepal.Width ~ ., data = data)
  }, function(data) {
    stats::predict(output$model, newdata = data)
  })

  model <- lm(Sepal.Width ~ ., data = iris)
  container$train(iris)
  expect_identical(container$predict(iris), stats::predict(model, newdata = iris))
})

test_that("can't predict with a non-trained container", {
  container <- tundraContainer$new("foo", function(data) {
    output$model <- lm(Sepal.Width ~ ., data = data)
  }, function(data) {
    stats::predict(output$model, newdata = data)
  })

  expect_error(container$predict(iris), "has not been trained yet.")
})
