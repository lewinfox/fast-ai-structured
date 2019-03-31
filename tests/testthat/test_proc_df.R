context("Testing proc_df")

animals <- data.frame(
  age = c(1, 2, 3.4, 129),
  animal = c("cat", "dog", "monkey", "dog"),
  is_nice = c(TRUE, TRUE, FALSE, TRUE),
  size = as.factor(c("small", "medium", "large", "xl")),
  is_hungry = c(FALSE, FALSE, TRUE, TRUE),
  weight = c(1.1, 2.2, NaN, 4.4),
  stringsAsFactors = FALSE
)

proc_df_result <- proc_df(animals, do_scale = TRUE)

test_that("proc_df returns correct data types", {
  expect_is(proc_df_result, "data.frame")
  expect_true(all(sapply(proc_df_result, class) == "numeric"))
})

test_that("proc_df's one-hot encoding works", {
  expect_equal(colnames(proc_df_result),
               c("age", "is_nice", "is_hungry", "weight",
                 "weight_na", "animal_cat", "animal_dog", "animal_monkey",
                 "size_small", "size_medium", "size_large", "size_xl"))
})
