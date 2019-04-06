#' Flag outliers
#'
#' Flag any values that fall outside a specified range. By default, any
#' observation more than 1.5x the interquartile range will be tagged. The
#' `clip_outliers` option controls whether values outside this range will be
#' trimmed to the appropriate max or min.
#'
#' @param df The data.frame to be transformed
#' @param col The column of interest
#' @param outlier_threshold Values more than \code{outlier_threshold} times the interquartile
#'   range outside the interquartile range are considered outliers.
#' @param clip_outliers If \code{TRUE}, values above or below the specified threshold
#'   will be clipped to the relevant threshold.
#' @param flag_outliers_both_directions If \code{TRUE}, two boolean columns will be added
#'   to indicate whether the values was above or below the detection threshold.
#' @export
detect_outliers <- function(df, col, outlier_threshold = 1.5, clip_outliers = FALSE,
                            flag_outliers_both_directions = FALSE) {
  # Input checking
  stopifnot(is.data.frame(df))
  if (!is.numeric(df[[col]])) {
    warning(col, " is not a numeric column - ignoring")
    return(df)
  }
  # Calculate limits
  q_25 <- quantile(df[[col]], 0.25)
  q_75 <- quantile(df[[col]], 0.75)
  iqr <- IQR(df[[col]])
  threshold_lower <- q_25 - (outlier_threshold * iqr)
  threshold_upper <- q_75 + (outlier_threshold * iqr)
  # Identify outliers
  is_outlier_lower <- df[[col]] < threshold_lower
  is_outlier_upper <- df[[col]] > threshold_upper
  is_outlier <- is_outlier_lower | is_outlier_upper
  # Clip values if requested
  if (clip_outliers) {
    df[[col]][is_outlier_lower] <- threshold_lower
    df[[col]][is_outlier_upper] <- threshold_upper
  }
  # Add boolean indicators to data.frame
  if (flag_outliers_both_directions) {
    flag_col_upper <- paste(col, "outlier_upper", sep = "_")
    flag_col_lower <- paste(col, "outlier_lower", sep = "_")
    df[[flag_col_upper]] <- is_outlier_upper
    df[[flag_col_lower]] <- is_outlier_lower
  } else {
    new_col_name <- paste(col, "outlier", sep = "_")
    df[[new_col_name]] <- is_outlier
  }
  return(df)
}
