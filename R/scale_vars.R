#' Transform numeric columns
#'
#' Any numeric columns are scaled and centred to have a mean of 0 and a standard
#' deviation of 1. Optionally, also normalise the values to the range 0, 1.
#'
#' @param df A `data.frame` to be transformed.
#' @param normalise If \code{TRUE}, after scaling and centering the values are
#'   normalised to the range 0, 1.
#'
#' @return A transformed \code{data.frame}.
#'
#' @export
scale_vars <- function(df, normalise = FALSE) {
  stopifnot(is.data.frame(df))
  for (col in colnames(df)) {
    if (is.numeric(df[[col]])) {
      df[[col]] <- as.vector(scale(df[[col]], center = TRUE, scale = TRUE))
      if (normalise) {
        min <- min(df[[col]], na.rm = TRUE)
        max <- max(df[[col]], na.rm = TRUE)
        df[[col]] <- (df[[col]] - min) / (max - min)
      }
    }
  }
  return(df)
}
