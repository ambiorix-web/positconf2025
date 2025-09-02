# Shared data analysis functions for the demo
# These functions will be used by both HTML and API routes

#' Get available datasets
get_available_datasets <- function() {
  list(
    list(
      name = "mtcars",
      title = "Motor Trend Car Road Tests",
      description = "Data from the 1974 Motor Trend US magazine, fuel consumption and 10 aspects of automobile design and performance",
      rows = nrow(mtcars),
      columns = ncol(mtcars)
    ),
    list(
      name = "iris",
      title = "Edgar Anderson's Iris Data",
      description = "Measurements of sepal and petal dimensions for 150 iris flowers from 3 species",
      rows = nrow(iris),
      columns = ncol(iris)
    ),
    list(
      name = "airquality",
      title = "New York Air Quality Measurements",
      description = "Daily air quality measurements in New York, May to September 1973",
      rows = nrow(airquality),
      columns = ncol(airquality)
    )
  )
}

#' Get dataset by name
get_dataset <- function(dataset_name) {
  switch(
    dataset_name,
    "mtcars" = mtcars,
    "iris" = iris,
    "airquality" = airquality,
    NULL
  )
}

#' Get dataset summary statistics
#'
#' @param dataset_name String. The dataset name. Either
#' 'mtcars' (default), 'iris', or 'airquality'.
#' @return Named list with these items:
#'         - dataset_info
#'         - numeric_summary
#'         - factor_summary
#'         - sample_data
#' @export
get_dataset_summary <- function(
  dataset_name = c("mtcars", "iris", "airquality")
) {
  dataset_name <- match.arg(arg = dataset_name)
  data <- get_dataset(dataset_name)

  # Get basic info
  dataset_info <- get_available_datasets()
  info <- dataset_info[[which(sapply(dataset_info, function(x) {
    x$name == dataset_name
  }))]]

  # Calculate summary statistics
  numeric_cols <- sapply(data, is.numeric)
  numeric_data <- data[, numeric_cols, drop = FALSE]

  summary_stats <- list()
  if (ncol(numeric_data) > 0) {
    summary_stats <- lapply(names(numeric_data), function(col) {
      values <- numeric_data[[col]]
      list(
        column = col,
        mean = round(mean(values, na.rm = TRUE), 2),
        median = round(median(values, na.rm = TRUE), 2),
        sd = round(sd(values, na.rm = TRUE), 2),
        min = round(min(values, na.rm = TRUE), 2),
        max = round(max(values, na.rm = TRUE), 2),
        na_count = sum(is.na(values))
      )
    })
    names(summary_stats) <- names(numeric_data)
  }

  # Factor columns
  factor_cols <- sapply(data, function(x) is.factor(x) || is.character(x))
  factor_data <- data[, factor_cols, drop = FALSE]

  factor_stats <- list()
  if (ncol(factor_data) > 0) {
    factor_stats <- lapply(names(factor_data), function(col) {
      values <- factor_data[[col]]
      freq_table <- table(values, useNA = "ifany")
      list(
        column = col,
        unique_count = length(unique(values)),
        most_common = names(freq_table)[which.max(freq_table)],
        most_common_count = max(freq_table),
        na_count = sum(is.na(values))
      )
    })
    names(factor_stats) <- names(factor_data)
  }

  list(
    dataset_info = info,
    numeric_summary = summary_stats,
    factor_summary = factor_stats,
    sample_data = utils::head(data, 5)
  )
}

#' Get raw dataset data
#'
#' @param dataset_name String. Dataset name.
#' @param limit Integer. How many rows should be returned?
#'        Defaults to an empty integer vector.
#' @return data.frame
#' @export
get_dataset_data <- function(dataset_name, limit = integer()) {
  data <- get_dataset(dataset_name)
  if (is.null(data)) {
    return(NULL)
  }

  limit_is_valid <- length(limit) &&
    is.integer(limit) &&
    limit > 0L &&
    limit < nrow(data)

  if (limit_is_valid) {
    data <- data[seq_len(limit), ]
  }

  data
}

