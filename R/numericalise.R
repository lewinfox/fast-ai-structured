#' Convert a non-numeric column to a numeric representation
#'
#' Given an input \code{data.frame}, the specified column will be converted to
#' numeric values unless the number of unique values is greater than
#' \code{max_n_cats}. This is a wrapper for \code{as.numeric(as.factor(col))}.
#'
#' @param df A \code{data.frame}
#' @param col The name (as a string) of the column to be converted
#' @param new_col_name The name of the new column to be added to the df. If not
#'   supplied, defaults to "[col]_numeric".
#' @param max_n_cats Columns containing greater than this number of unique
#'   values will not be numericalised. If this is not supplied, the column will
#'   be converted regardless.
#' @return The input \code{data.frame} with an addtional column containing the
#'   numeric representation of \code{col}.
#'
#' @export
numericalise <- function(df, col, new_col_name = NULL, max_n_cats = Inf) {
  stopifnot(is.data.frame(df))
  if (is.null(new_col_name)) {
    new_col_name <- paste(col, "numeric", sep = "_")
  }
  if (length(unique(df[[col]])) <= max_n_cats) {
    df[[new_col_name]] <- as.numeric(as.factor(df[[col]]))
  }
  return(df)
}
