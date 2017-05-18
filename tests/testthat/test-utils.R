context("utils")

describe("list_merge", {
  test_that("can merge two empty lists", {
    expect_equal(list_merge(list(), list()), list())
  })

  test_that("merging with an empty list is a no-op", {
    original <- list(meaning_of_life = 42)
    expect_equal(list_merge(original, list()), original)
  })

  test_that("merging nontrivial lists without names", {
    lstone <- list(42, 228)
    lsttwo <- list(322)
    expect_equal(list_merge(lstone, lsttwo), list(42, 228, 322))
  })

  test_that("merging flas lists", {
    lstone <- list(meaning_of_life = 42)
    lsttwo <- list(foo = "bar")
    expect_equal(list_merge(lstone, lsttwo),
      list(meaning_of_life = 42, foo = "bar"))
  })

  test_that("key overrides", {
    lstone <- list(meaning_of_life = 42, foo = "baz")
    lsttwo <- list(foo = "bar")
    expect_equal(list_merge(lstone, lsttwo),
      list(meaning_of_life = 42, foo = "bar"))
    expect_equal(list_merge(lsttwo, lstone),
      list(meaning_of_life = 42, foo = "baz"))
  })

  test_that("nested lists", {
    lstone <- list(meaning_of_life = 42, foo = list(bar = "baz"))
    lsttwo <- list(foo = list(bar = "quux"))
    expect_equal(list_merge(lstone, lsttwo),
      list(meaning_of_life = 42, foo = list(bar = "quux")))
    expect_equal(list_merge(lsttwo, lstone),
      list(meaning_of_life = 42, foo = list(bar = "baz")))
  })
})
