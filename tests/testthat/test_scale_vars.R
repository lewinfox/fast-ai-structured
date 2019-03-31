context("Variable scaling")

df <- data.frame(a = c(1, 2, 3, 4),
                 b = c(1e7, 2e7, 3e7, 4e7),
                 c = c("cat", "dog", "rat", "log"),
                 d = c(-100, -200, -300, -400),
                 e = c(TRUE, TRUE, FALSE, TRUE),
                 f = factor(c("blue", "red", "green", "green")),
                 stringsAsFactors = FALSE)

scaled_df <- scale_vars(df)

test_that("scale_vars returns correct types", {
  expect_is(scaled_df, "data.frame")
  # Testing the data.frame
  expect_is(scaled_df, "data.frame")
  expect_is(scaled_df$a, "numeric")
  expect_is(scaled_df$b, "numeric")
  expect_is(scaled_df$c, "character")
  expect_is(scaled_df$d, "numeric")
  expect_is(scaled_df$e, "logical")
  expect_is(scaled_df$f, "factor")
})

test_that("scale_vars calculates mean and median correctly", {
  expect_equal(scaled_df$a, scaled_df$b)
  expect_equal(scaled_df$a, scaled_df$d * -1)
})

rm(df, scaled_df)
