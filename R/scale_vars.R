#' Transform numeric columns
#'
#' If the `mapper` argument is supplied, equivalent to calling `mapper(df)`. If
#' mapper is not supplied then any numeric columns are scaled and centred to
#' have a mean of 0 and a standard deviation of 1. Optionally, also normalise
#' the values to the range 0, 1.
#'
#' @param df A `data.frame` to be transformed.
#' @param normalise If \code{TRUE}, after scaling and centering the values are
#'   normalised to the range 0, 1.
#'
#' @return A transformed `data.frame`.
#'
#' @export
scale_vars <- function(df, normalise = FALSE) {
  for (col in colnames(df)) {
    if (is.numeric(df[[col]])) {
      mean <- mean(df[[col]])
      sd <- sd(df[[col]])
      df[[col]] <- (df[[col]] - mean) / sd
      if (normalise) {
        min <- min(df[[col]])
        df[[col]] <- df[[col]] - min
        df[[col]] <- df[[col]] / max(df[[col]])
      }
    }
  }
return(df)
}
