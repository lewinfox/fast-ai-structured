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
  count <- table(df[[col]])
  values <- names(count)
  # If the number of unique values is more than `max_n_cats` then we will leave
  # the column unaltered.
  if (length(values) <= max_n_cats) {
    one_hot_vars <- as.data.frame(model.matrix(~ df[[col]] - 1))
    # Names need fixing - we first have to trim off the "df[[col]]" that
    # `model.matrix` has prepended, then replace any spaces with underscores.
    # For readability this is split into two steps.
    trimmed_names <- gsub("^df\\[\\[col\\]\\]", "", colnames(one_hot_vars))
    colnames(one_hot_vars) <- paste(col, gsub("[[:space:]]", "_", trimmed_names), sep = "_")
    df[col] <- NULL

    # If `drop_one_col` is TRUE, the column corresponding to the most common
    # case will be dropped.
    if (drop_one_col) {
      # The names need to be converted to the right format first
      names(count) <- paste(col, gsub("[[:space:]]", "_", names(count)), sep = "_")
      # Find the value/s that are most common
      to_drop <- names(count[count == max(count)])
      # In case there's a tie, pick the first one
      to_drop <- to_drop[1]
      one_hot_vars[[to_drop]] <- NULL
    }
  }
  cbind(df, one_hot_vars)
}
