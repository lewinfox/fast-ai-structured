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

proc_df_result <- proc_df(animals)
proc_df_result_scaled <- proc_df(animals, do_scale = TRUE)
proc_df_result_dropped_y <- proc_df(animals, y = "weight")
proc_df_result_flagged_outliers <- proc_df(animals, detect_outliers = TRUE)

test_that("proc_df returns correct data types", {
  expect_is(proc_df_result, "data.frame")
  expect_true(all(sapply(proc_df_result, class) %in% c("integer", "numeric")))
})

test_that("proc_df drops the dependent variable", {
  expect_false("weight" %in% colnames(proc_df_result_dropped_y))
})

test_that("proc_df's one-hot encoding works", {
  expect_equal(colnames(proc_df_result),
               c("age", "is_nice", "is_hungry", "weight",
                 "weight_na", "animal_cat", "animal_dog", "animal_monkey",
                 "size_large", "size_medium", "size_small", "size_xl")
  )
})

test_that("outlier columns are added to data", {
  expect_equal(colnames(proc_df_result_flagged_outliers),
               c("age", "is_nice", "is_hungry", "weight", "weight_na",
                 "age_outlier", "weight_outlier", "animal_cat", "animal_dog",
                 "animal_monkey", "size_large", "size_medium", "size_small",
                 "size_xl"))
})
