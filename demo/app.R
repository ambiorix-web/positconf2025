# Ambiorix Demo App - Data Dashboard

library(ambiorix)
library(htmltools)

# Source helper files
source("data_analysis.R")
source("html_helpers.R")

# Initialize Ambiorix app
app <- Ambiorix$new()

# ========================================
# HTML ROUTES (Browser Interface)
# ========================================

app$get("/", function(req, res) {
  html_content <- create_homepage()
  res$send(html_content)
})

app$get("/datasets/:name", function(req, res) {
  dataset_name <- req$params$name
  html_content <- create_dataset_page(dataset_name)
  res$send(html_content)
})

app$get("/api", function(req, res) {
  html_content <- create_api_page()
  res$send(html_content)
})

# ========================================
# JSON API ROUTES
# ========================================

app$get("/api/datasets", function(req, res) {
  datasets <- get_available_datasets()
  res$json(list(
    message = "Available datasets",
    count = length(datasets),
    datasets = datasets
  ))
})

app$get("/api/datasets/:name", function(req, res) {
  dataset_name <- req$params$name
  summary_data <- get_dataset_summary(dataset_name)

  if (is.null(summary_data)) {
    res$status(404L)
    res$json(list(
      error = "Dataset not found",
      message = sprintf(
        "Dataset '%s' not found. Available datasets: mtcars, iris, airquality",
        dataset_name
      )
    ))
    return()
  }

  res$json(summary_data)
})

app$get("/api/datasets/:name/data", function(req, res) {
  dataset_name <- req$params$name

  limit <- req$query$limit
  if (!is.null(limit)) {
    limit <- as.numeric(limit)
    if (is.na(limit) || limit <= 0) {
      res$status(400L)
      res$json(list(
        error = "Invalid limit parameter",
        message = "Limit must be a positive number"
      ))
      return()
    }
  }

  data <- get_dataset_data(dataset_name, limit)

  if (is.null(data)) {
    res$status(404L)
    res$json(list(
      error = "Dataset not found",
      message = sprintf(
        "Dataset '%s' not found. Available datasets: mtcars, iris, airquality",
        dataset_name
      )
    ))
    return()
  }

  res$json(list(
    dataset = dataset_name,
    rows_returned = nrow(data),
    limit_applied = !is.null(limit),
    data = data
  ))
})

app$not_found <- function(req, res) {
  if (startsWith(req$PATH_INFO, "/api/")) {
    response <- list(
      error = "Endpoint not found",
      message = sprintf("API endpoint '%s' not found", req$PATH_INFO),
      available_endpoints = list(
        "/api/datasets",
        "/api/datasets/{name}",
        "/api/datasets/{name}/data"
      )
    )
    res$status <- 404L
    return(res$json(response))
  }

  html_content <- create_error_page(
    "Page Not Found",
    sprintf("The page '%s' you're looking for doesn't exist.", req$PATH_INFO)
  )
  res$status <- 404L
  res$send(html_content)
}

app$start(port = 3000L)
