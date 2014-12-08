context('hooks')

test_that('a train pre-munge hook works', {
  simple <- tundra_container$new('simple',
    munge_procedure = list("Print" = list(function(.) cat('munged'))))
  simple$add_hook('train_pre_munge', function() {
    expect_identical(dataframe, iris)
    cat('trained')
  })
  expect_output(local({ simple$train(iris, verbose = TRUE); NULL }), 'trainedmunged',
    info = 'the train pre-munge hook should have executed')
})

test_that('a train post-munge hook works', {
  simple <- tundra_container$new('simple',
    munge_procedure = list("Print" = list(function(.) cat('munged'))))
  simple$add_hook('train_post_munge', function() {
    expect_identical(dataframe, iris)
    cat('trained')
  })
  expect_output(local({ simple$train(iris, verbose = TRUE); NULL }), 'munged.*trained',
    info = 'the train post-munge hook should have executed')
})

test_that('a predict pre-munge hook works', {
  simple <- tundra_container$new('simple',
    munge_procedure = list("Print" = list(function(.) cat('munged'))))
  simple$add_hook('predict_pre_munge', function() {
    attr(dataframe, 'mungepieces') <- NULL
    expect_identical(dataframe, iris)
    cat('predicted')
  })
  simple$train(iris)
  expect_output(local({ simple$predict(iris, verbose = TRUE); NULL }), 'predictedmunged',
    info = 'the predict pre-munge hook should have executed')
})

test_that('a predict post-munge hook works', {
  simple <- tundra_container$new('simple',
    munge_procedure = list("Print" = list(function(.) cat('munged'))))
  simple$add_hook('predict_post_munge', function() {
    attr(dataframe, 'mungepieces') <- NULL
    expect_identical(dataframe, iris)
    cat('predicted')
  })
  simple$train(iris)
  expect_output(local({ simple$predict(iris, verbose = TRUE); NULL }), 'munged.*predicted',
    info = 'the predict post-munge hook should have executed')
})

test_that('hooks can modify internals', {
  simple <- tundra_container$new('simple')
  simple$add_hook('train_pre_munge', function() {
    internal <<- list(hello = 'world')
  })
  simple$train(iris)
  expect_identical(simple$internal, list(hello = 'world'))
})

test_that('hooks can modify locals', {
  simple <- tundra_container$new('simple')
  simple$add_hook('predict_post_munge', function() {
    attr(dataframe, 'mungepieces') <<- NULL
    dataframe$Species <<- NULL
  })
  simple$train(iris)
  expect_identical(simple$predict(iris), iris[-5])
})

