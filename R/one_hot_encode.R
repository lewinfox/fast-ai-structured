#' One-hot encode a variable
#'
#' Given an input \code{data.frame} and a target column, one-hot encode the
#' column if the number of unique values is greater than a specified threshold.
#' The original column is dropped. To prevent whitespace appearing in column
#' names, any \code{[:space:]} characters are replaced with underscores using
#' \code{gsub}.
#'
#' @param df A data.frame.
#' @param col The column to be encoded.
#' @param max_n_cats Columns will only be one-hot encoded if they contain this
#'   many categories or fewer.
#'
#' @return A \code{data.frame}.
#'
#' @export
one_hot_encode <- function(df, col, max_n_cats = Inf, drop_one_col = FALSE) {
  stopifnot(is.data.frame(df))
  values <- unique(df[[col]])
  # If the number of unique values is more than `max_n_cats` then we will leave
  # the column unaltered.
  if (length(values) <= max_n_cats) {
    if (drop_one_col) {
      # TODO: Drop the most common column rather than just the first
      values <- values[2:length(values)]
    }
    for (value in values) {
      new_col_data <- as.numeric(df[[col]] == value)
      value <- gsub("[[:space:]]", "_", value)
      new_col_name <- paste(col, value, sep = "_")
      df[new_col_name] <- new_col_data
    }
    df[col] <- NULL
  }
  return(df)
}
