context("One-hot encoding")

animals <- data.frame(
  age = c(1, 2, 3.4, 129),
  animal = c("cat face", "dog face", "monkey face", "dog face"),
  is_nice = c(TRUE, TRUE, FALSE, TRUE),
  size = as.factor(c("small", "medium", "large", "xl")),
  is_hungry = c(FALSE, FALSE, TRUE, TRUE),
  weight = c(1.1, 2.2, NaN, 4.4),
  stringsAsFactors = FALSE
)

test_that("one-hot encoding is working correctly", {
  expect_equal(colnames(one_hot_encode(animals, "animal")),
               c("age", "is_nice", "size", "is_hungry", "weight",
                 "animal_cat_face", "animal_dog_face", "animal_monkey_face"))
  expect_equal(colnames(one_hot_encode(animals, "animal", drop_one_col = TRUE)),
               c("age", "is_nice", "size", "is_hungry", "weight",
                 "animal_dog_face", "animal_monkey_face"))
})
