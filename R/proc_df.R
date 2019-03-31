#' Process a data.frame
#'
#' Port of the Python \code{fast.ai}'s \code{prod_df} function.
#'
#' Given an input data.frame, split off the response variable and convert it
#' to an entirely numeric data.frame. NaNs are replaced using median imputation,
#' and a new boolean `[col]_na` column added to indicate where the missing
#' values were.
#'
#' @param df The data frame of interest.
#' @param y The dependent variable. If not specified it is assumed that
#'   \code{df} contains only independent variables.
#' @param skip_fields A character vector of column names to be dropped from
#'   \code{df}.
#' @param ignore_fields A character vector of column names to be ignored during
#'   processing.
#' @param do_scale Boolean - should columns be scaled and centred?
#' @param na_list List of NA column to add. NA columns are also added if there
#'   are any mising values.
#' @param preproc_fun A function to be applied to \code{df}. This will be simply
#'   called as \code{df <- preproc_fn(df)}, so make sure it is fully
#'   self-contained and returns a \code{data.frame}.
#' @param max_n_cats Column with less than or equal to this number of unique
#'   entries will be converted to one-hot encoded columns.
#' @param subset If set, a random sample of \code{n = subset} rows will be taken.
#' @param mapper If \code{do_scale} is true, the mapper function calculates the
#'   scalng params (mean and standard deviation) to be applied.
#'
#' @return A \code{list} containing three items: \code{x}, \code{y}, \code{NAs},
#'   and (optionally) \code{mapper}, a data.frame containing the mean and
#'   standard deviation of each column.
#'
#' @export
#'
proc_df <- function(df, y = NULL, do_scale = FALSE, nas = NULL, preproc_fun = NULL,
                    max_n_cats = Inf, subset = NULL, mapper = NULL) {

  if (!is.null(y)) {
    result$y <- df[[y]]
    df <- df[setdiff(colnames(df), "a")]
  }

  # ---- Setup and preprocessing ----
  if (!is.null(subset)) {
    rows_to_keep <- sample(x = nrow(df), size = sample, replace = FALSE,
                           prob = NULL)
    df <- df[rows_to_keep, ]
  }
  if (!is.null(preproc_fun)) {
    df <- preproc_fn(df)
  }
  if (any(is.na(df))) {
    df <- fix_missing(df)
  }

  # Scale numeric variables
  if (do_scale) {
    df <- scale_vars(df)
  }

  # ---- One-hot encoding ----
  for (col in colnames(df)) {
    if (!is.numeric(df[[col]]) & !is.logical(df[[col]])) {
      df <- one_hot_encode(df, col, max_n_cats)
    }
  }

  # ---- Convert any booleans to 1 / 0 ----
  df <- as.data.frame(
    lapply(df, function(x) if (is.logical(x)) as.numeric(x) else x),
    stringsAsFactors = FALSE
  )

  # ---- Convert remaining non-numeric variables ----
  for (col in colnames(df)) {
    if (!is.numeric(df[[col]])) {
      df <- numericalise(df, col, max_n_cats = max_n_cats)
    }
  }
  return(df)
}
