context("Numericalising")

animals <- data.frame(
  age = c(1, 2, 3.4, 129),
  animal = c("cat", "dog", "mountain goat", "dog"),
  is_nice = c(TRUE, TRUE, FALSE, TRUE),
  size = as.factor(c("small", "medium", "large", "xl")),
  is_hungry = c(FALSE, FALSE, TRUE, TRUE),
  weight = c(1.1, 2.2, NaN, 4.4),
  stringsAsFactors = FALSE
  )

bool_test <- numericalise(animals, "is_nice")
factor_test <- numericalise(animals, "size")

test_that("numericalise returns a data.frame", {
  expect_is(bool_test, "data.frame")
  expect_is(factor_test, "data.frame")
})

test_that("boolean columns are correctly converted", {
  expect_type(bool_test$is_nice, "integer")
  expect_equal(max(bool_test$is_nice), 1)
  expect_equal(min(bool_test$is_nice), 0)
})

test_that("factor columns are correctly converted", {
  expect_type(factor_test$size, "integer")
})
