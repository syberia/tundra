context("tundraContainer$initialize")
library(testthatsomemore)

test_that("it errors when an invalid munge_procedure is provided", {
  expect_error(tundraContainer$new("foo", munge_procedure = NULL), "must be a list")
  expect_error(tundraContainer$new("foo", munge_procedure = 5), "must be a list")
  expect_error(tundraContainer$new("foo", munge_procedure = force), "must be a list")
})

test_that("it successfully creates a tundraContainer when a valid munge_procedure is provided", {
  testthatsomemore::assert(tundraContainer$new("foo", munge_procedure = list()))
  testthatsomemore::assert(
    container <- tundraContainer$new("foo", munge_procedure = stagerunner::stageRunner$new(list(force)))
  )
})

test_that("it can produce human-readable summaries", {
  container <- tundraContainer$new("foo", function(df) { output$model <- 1:10 })
  expect_equal(summary(container), summary(NULL))
  container$train(iris)
  expect_equal(summary(container), summary(1:10))
})

test_that("it can print a nice name", {
  container <- tundraContainer$new("foo")
  printed <- capture.output(print(container))
  expected <- paste("A tundraContainer of type", sQuote("foo"))
  expect_equal(printed, expected)
})
