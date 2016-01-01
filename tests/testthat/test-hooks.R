context("hooks")

let(container, tundraContainer$new("foo"))
let(container_with_munge_side_effect, function(env) {
  force(env)
  mb <- mungebits2::mungebit$new(function(data) { 
    env$effect <- c(env$effect, "mungebit")
    data
  })
  mp <- mungebits2::mungepiece$new(mb)
  tundraContainer$new("foo", munge_procedure = list(mp))
})

let(container_with_train_side_effect, function(env) {
  tundraContainer$new("foo", function(data) { env$effect <- c(env$effect, "train") })
})

describe("Invalid inputs", {
  test_that("it errors if an invalid hook name is provided", {
    expect_error(container$add_hook("foo"))
  })

  test_that("it errors if an invalidly typed hook name is provided", {
    expect_error(container$add_hook("train_pre_munge", NULL))
    expect_error(container$add_hook("train_pre_munge", 1))
    expect_error(container$add_hook("train_pre_munge", iris))
  })
})

test_that("it can add a simple hook", {
  env  <- list2env(list(x = 0))
  hook <- function() { env$x <- 1 }
  container$add_hook("train_pre_munge", hook)
  container$train(iris)
  expect_equal(env$x, 1)
})

test_that("it runs the train pre-munge hook in the correct order", {
  env  <- list2env(list(effect = character(0)))
  hook <- function() { env$effect <- c(env$effect, "hook") }
  container <- container_with_munge_side_effect(env)
  container$add_hook("train_pre_munge", hook)
  container$train(iris)
  expect_equal(env$effect, c("hook", "mungebit"))
})

test_that("it runs the train post-munge hook in the correct order", {
  env  <- list2env(list(effect = character(0)))
  hook <- function() { env$effect <- c(env$effect, "hook") }
  container <- container_with_munge_side_effect(env)
  container$add_hook("train_post_munge", hook)
  container$train(iris)
  expect_equal(env$effect, c("mungebit", "hook"))
})

test_that("it runs the train_finalize hook in the correct order", {
  container <- container_with_train_side_effect(env)
  env  <- list2env(list(effect = character(0)))
  hook <- function() { env$effect <- c(env$effect, "hook") }
  container$add_hook("train_finalize", hook)
  container$train(iris)
  expect_equal(env$effect, c("train", "hook"))
})

test_that("it runs the predict pre-munge hook in the correct order", {
  env  <- list2env(list(effect = character(0)))
  hook <- function() { env$effect <- c(env$effect, "hook") }
  container <- container_with_munge_side_effect(env)
  container$add_hook("predict_pre_munge", hook)
  container$train(iris, munge = FALSE)
  expect_equal(env$effect, character(0))
  container$predict(iris)
  expect_equal(env$effect, c("hook", "mungebit"))
})

test_that("it runs the predict post-munge hook in the correct order", {
  env  <- list2env(list(effect = character(0)))
  hook <- function() { env$effect <- c(env$effect, "hook") }
  container <- container_with_munge_side_effect(env)
  container$add_hook("predict_post_munge", hook)
  container$train(iris, munge = FALSE)
  expect_equal(env$effect, character(0))
  container$predict(iris)
  expect_equal(env$effect, c("mungebit", "hook"))
})

