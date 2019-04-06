#' Process a data.frame
#'
#' Port of \href{https://github.com/fastai/fastai}{fast.ai}'s \code{proc_df} function.
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
#'   \code{df}. Not yet implemented.
#' @param ignore_fields A character vector of column names to be ignored during
#'   processing. Not yet implemented.
#' @param do_scale Should columns be scaled and centred?
#' @param normalise See \code{\link{scale_vars}}.
#' @param detect_outliers Should outliers be flagged? If \code{TRUE}, also set
#'   \code{outlier_threshold}, \code{clip_outliers} and
#'   \code{flag_outliers_both_directions}. See \link{\code{detect_outliers}}.
#' @param outlier_threshold See \code{\link{detect_outliers}}.
#' @param na_list List of NA column to add. NA columns are also added if there
#'   are any mising values. Not yet implemented.
#' @param preproc_fun A function to be applied to \code{df}. This will be simply
#'   called as \code{df <- preproc_fn(df)} before any other operations, so make
#'   sure it is fully self-contained and returns a \code{data.frame}.
#' @param max_n_cats Column with less than or equal to this number of unique
#'   entries will be converted to one-hot encoded columns.
#' @param subset If set, a random sample of \code{subset} rows will be taken.
#' @param mapper If \code{do_scale} is true, the mapper function calculates the
#'   scaling params (mean and standard deviation) to be applied.
#'
#' @return A transformed \code{data.frame}.
#'
#' @export
#'
proc_df <- function(df,
                    y = NULL,
                    do_scale = FALSE,
                    normalise = FALSE,
                    detect_outliers = FALSE,
                    nas = NULL,
                    preproc_fun = NULL,
                    max_n_cats = Inf,
                    subset = NULL,
                    mapper = NULL) {

  if (!is.null(preproc_fun)) {
    df <- preproc_fn(df)
  }

  stopifnot(is.data.frame(df))

  # If `y` has been specified and is present, drop it.
  if (!is.null(y)) {
    if (y %in% colnames(df)) {
      df <- df[setdiff(colnames(df), y)]
    } else {
      warning("Column ", y, " not found")
    }
  }

  # ---- Subset ----
  if (!is.null(subset)) {
    rows_to_keep <- sample(
      x = nrow(df),
      size = sample,
      replace = FALSE,
      prob = NULL)
    df <- df[rows_to_keep, ]
  }

  # ---- Fix missing ----
  if (any(is.na(df))) {
    # TODO: Implement NA list
    df <- fix_missing(df = df)
  }

  # ---- Detect outliers ----

  # ---- Scale numeric variables ----
  if (do_scale) {
    df <- scale_vars(df = df, normalise = normalise)
  }

  # ---- One-hot encoding ----
  for (col in colnames(df)) {
    if (!is.numeric(df[[col]]) & !is.logical(df[[col]])) {
      df <- one_hot_encode(df = df, col = col, max_n_cats = max_n_cats)
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
      df <- numericalise(df = df, col = col, max_n_cats = max_n_cats)
    }
  }
  return(df)
}
