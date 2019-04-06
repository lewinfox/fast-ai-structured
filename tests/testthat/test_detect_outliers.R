context("Outlier detection")

df <- iris

test_that("internal params are calculated correctly", {
  # These are just the commands that are executed inside the function
  iqr <- IQR(df$Sepal.Width)
  q_25 <- quantile(df$Sepal.Width, 0.25)
  q_75 <- quantile(df$Sepal.Width, 0.75)
  threshold_lower <- q_25 - (1.5 * iqr)
  threshold_upper <- q_75 + (1.5 * iqr)
  # Test them against the actual outcome
  clipped_df <- detect_outliers(df, "Sepal.Width", outlier_threshold = 1.5,
                                clip_outliers = TRUE)
  expect_false(max(clipped_df$Sepal.Width) > threshold_upper)
  expect_false(min(clipped_df$Sepal.Width) < threshold_lower)
  expect_equal(min(clipped_df$Sepal.Width), unname(threshold_lower))
  expect_equal(max(clipped_df$Sepal.Width), unname(threshold_upper))
})

test_that("clipping works correctly", {
  clipped_df <- detect_outliers(df, "Sepal.Width", clip_outliers = TRUE)
  expect_false(max(df[["Sepal.Width"]]) == max(clipped_df[["Sepal.Width"]]))
  expect_false(min(df[["Sepal.Width"]]) == min(clipped_df[["Sepal.Width"]]))
  expect_equal(sum(clipped_df$Sepal.Width_outlier), 4)
})

test_that("bidirectional outlier flag works", {
  clipped_df <- detect_outliers(df, "Sepal.Length", clip_outliers = TRUE,
                                flag_outliers_both_directions = T)
  expect_equal(colnames(clipped_df),
               c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
                 "Species", "Sepal.Length_outlier_upper", "Sepal.Length_outlier_lower"))
})
