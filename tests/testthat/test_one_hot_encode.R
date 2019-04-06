context("One-hot encoding")

animals <- data.frame(
  age = c(1, 2, 3.4, 129),
  animal = c("cat", "dog", "mountain goat", "dog"),
  is_nice = c(TRUE, TRUE, FALSE, TRUE),
  size = as.factor(c("small", "medium", "large", "xl")),
  is_hungry = c(FALSE, FALSE, TRUE, TRUE),
  weight = c(1.1, 2.2, NaN, 4.4),
  stringsAsFactors = FALSE
)

animals_with_ties <- data.frame(
  age = c(1, 2, 3.4, 129),
  animal = c("cat", "dog", "cat", "dog"),
  is_nice = c(TRUE, TRUE, FALSE, TRUE),
  size = as.factor(c("small", "medium", "large", "xl")),
  is_hungry = c(FALSE, FALSE, TRUE, TRUE),
  weight = c(1.1, 2.2, NaN, 4.4),
  stringsAsFactors = FALSE
)

encoded_animals <- one_hot_encode(animals, "animal")
encoded_animals_dropped_col <- one_hot_encode(animals, "animal",
                                              drop_one_col = TRUE)
encoded_animals_ties_dropped_col <- one_hot_encode(animals_with_ties, "animal",
                                                   drop_one_col = TRUE)

test_that("one_hot_encode generates correct columns", {
  expect_equal(
    colnames(encoded_animals),
    c("age", "is_nice", "size", "is_hungry", "weight", "animal_cat",
      "animal_dog", "animal_mountain_goat")
  )
  expect_equal(
    colnames(encoded_animals_dropped_col),
    c("age", "is_nice", "size", "is_hungry", "weight", "animal_cat",
      "animal_mountain_goat")
  )
})


test_that("one_hot_encode generates correct columns when most common value is tied", {
  expect_equal(colnames(encoded_animals_ties_dropped_col),
               c("age", "is_nice", "size", "is_hungry", "weight", "animal_dog")
  )
})

test_that("one_hot_encode maintains correct row order", {
  expect_equal(encoded_animals$animal_cat, c(1, 0, 0, 0))
  expect_equal(encoded_animals$animal_dog, c(0, 1, 0, 1))
  expect_equal(encoded_animals$animal_mountain_goat, c(0, 0, 1, 0))
})
