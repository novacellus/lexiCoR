context("Corpus functions")

test_that("find_word_id returns correct id", {
  expect_equal(find_word_id("PATROLOGIA", "lemma", "mater"), 232)
})

test_that("find_word_pos returns a vector", {
  expect_equal(is.vector(find_word_pos("PATROLOGIA", "lemma", 232)), TRUE)
})
