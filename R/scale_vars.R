#' Transform numeric columns
#'
#' If the `mapper` argument is supplied, equivalent to calling `mapper(df)`. If
#' mapper is not supplied then any numeric columns are scaled and centred to
#' have a mean of 0 and a standard deviation of 1.
#'
#' @param df A `data.frame` to be transformed.
#'
#' @return A transformed `data.frame`.
#'
#' @export
scale_vars <- function(df) {
  for (col in colnames(df)) {
    if (is.numeric(df[[col]])) {
      mean <- mean(df[[col]], na.rm = TRUE)
      sd <- sd(df[[col]], na.rm = TRUE)
      df[[col]] <- (df[[col]] - mean) / sd
    }
  }
return(df)
}
