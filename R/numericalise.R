#' Convert a non-numeric column to a numeric representation
#'
#' Given an input \code{data.frame}, the specified column will be converted to
#' numeric values. This is a wrapper for \code{as.numeric(as.factor(df$col))},
#' or as.numeric(cdf$col) for boolean columns.
#'
#' @param df A \code{data.frame}
#' @param col The name (as a string) of the column to be converted
#' @return The transformed \code{data.frame}.
#'
#' @export
numericalise <- function(df, col) {
  stopifnot(is.data.frame(df))
  if (class(df[[col]]) == "logical") {
    df[[col]] <- as.integer(df[[col]])
  } else {
    df[[col]] <- as.integer(as.factor(df[[col]]))
  }
  return(df)
}
