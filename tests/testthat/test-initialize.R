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
    tundraContainer$new("foo", munge_procedure = stagerunner::stageRunner$new(list(force)))
  )
})

