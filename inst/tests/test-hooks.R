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

