#' Fix missing values
#'
#' Replace NAs in a data frame with the median for the column, and add a new
#' booolean column indicating which rows have had NAs replaced.
#'
#' @param df A \code{data.frame}.
#'
#' @return The processed \code{data.frame}.
#'
#' @export
fix_missing <- function(df) {
  stopifnot(is.data.frame(df))
  for (col in colnames(df)) {
    if (is.numeric(df[[col]]) & any(is.na(df[[col]]))) {
      na_vec <- is.na(df[[col]])
      med <- median(df[[col]], na.rm = T)
      df[[col]][na_vec] <- med
      new_col_name <- paste(col, "na", sep = "_")
      df[new_col_name] <- na_vec
    }
  }
  return(df)
}
