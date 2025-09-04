# HTML helper functions using htmltools and Bootstrap

library(htmltools)

#' Create a Bootstrap page layout
create_bootstrap_page <- function(title, content, show_api_link = TRUE) {
  tags$html(
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1, shrink-to-fit=no"
      ),
      tags$title(title),
      tags$link(
        rel = "stylesheet",
        href = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"
      ),
      tags$link(
        rel = "stylesheet",
        href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.1/font/bootstrap-icons.css"
      )
    ),
    tags$body(
      tags$nav(
        class = "navbar navbar-expand-lg navbar-dark bg-primary mb-4",
        tags$div(
          class = "container",
          tags$a(
            class = "navbar-brand",
            href = "/",
            tags$span(
              tags$i(class = "bi bi-bar-chart"),
              "Ambiorix Data Dashboard"
            )
          ),
          if (show_api_link) {
            tags$div(
              class = "navbar-nav ms-auto",
              tags$a(
                class = "nav-link",
                href = "/api",
                style = "color: #ffd700;",
                tags$span(
                  tags$i(class = "bi bi-link-45deg"),
                  "Explore API"
                )
              )
            )
          }
        )
      ),
      tags$div(
        class = "container",
        content
      ),
      tags$script(
        src = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js"
      )
    )
  )
}

#' Create the homepage
create_homepage <- function() {
  datasets <- get_available_datasets()

  content <- tagList(
    tags$div(
      class = "row",
      tags$div(
        class = "col-lg-8 mx-auto",
        tags$h1(
          class = "display-4 mb-4",
          tags$span(
            tags$i(class = "bi bi-bar-chart"),
            "Interactive Data Dashboard"
          )
        ),
        tags$p(
          class = "lead mb-5",
          "Explore R datasets through both web interface and JSON API - all powered by Ambiorix!"
        ),

        tags$h2("Available Datasets"),

        # ----dataset cards----
        tags$div(
          class = "row",
          lapply(datasets, function(dataset) {
            tags$div(
              class = "col-md-6 col-lg-4",
              tags$div(
                class = "card dataset-card h-100",
                tags$div(
                  class = "card-body",
                  tags$h5(
                    class = "card-title",
                    dataset$title
                  ),
                  tags$p(
                    class = "card-text",
                    dataset$description
                  ),
                  tags$small(
                    class = "text-muted",
                    sprintf(
                      "%d rows × %d columns",
                      dataset$rows,
                      dataset$columns
                    )
                  )
                ),
                tags$div(
                  class = "card-footer",
                  tags$a(
                    href = sprintf("/datasets/%s", dataset$name),
                    class = "btn btn-primary btn-sm",
                    "View Dataset"
                  )
                )
              )
            )
          })
        ),

        # ----API info----
        tags$div(
          class = "card my-5 bg-light",
          tags$div(
            class = "card-body",
            tags$h3(
              class = "card-title",
              "API Endpoints"
            ),
            tags$p(
              "This same data is also available via JSON API:"
            ),
            tags$ul(
              class = "list-unstyled",
              tags$li(
                tags$code("/api/datasets"),
                " - List all datasets"
              ),
              tags$li(
                tags$code("/api/datasets/{name}/summary"),
                " - Get dataset summary"
              ),
              tags$li(
                tags$code("/api/datasets/{name}/data?limit=10"),
                " - Get raw data"
              )
            ),
            tags$a(
              href = "/api/datasets",
              class = "btn btn-outline-primary",
              "Try the API"
            )
          )
        )
      )
    )
  )

  create_bootstrap_page("Data Dashboard - Home", content)
}

#' Create dataset detail page
#'
#' @param dataset_name String. Dataset to build page for.
#'        Either 'mtcars', 'iris', or 'airquality'
#' @return [htmltools::tags]
create_dataset_page <- function(dataset_name) {
  summary_data <- get_dataset_summary(dataset_name)

  info <- summary_data$dataset_info

  numerics_div <- NULL
  numerics_div_class <- "col-lg-12"
  if (length(summary_data$factor_summary)) {
    numerics_div_class <- "col-lg-8"
  }

  if (length(summary_data$numeric_summary)) {
    numerics_div <- tags$div(
      class = numerics_div_class,
      tags$h3(
        tags$i(class = "bi bi-graph-up"),
        "Numeric Columns"
      ),
      tags$div(
        class = "table-responsive",
        tags$table(
          class = "table table-striped stats-table",
          tags$thead(
            class = "table-dark",
            tags$tr(
              tags$th("Column"),
              tags$th("Mean"),
              tags$th("Median"),
              tags$th("Std Dev"),
              tags$th("Min"),
              tags$th("Max"),
              tags$th("Missing")
            )
          ),
          tags$tbody(
            lapply(summary_data$numeric_summary, function(stat) {
              tags$tr(
                tags$td(tags$code(stat$column)),
                tags$td(stat$mean),
                tags$td(stat$median),
                tags$td(stat$sd),
                tags$td(stat$min),
                tags$td(stat$max),
                tags$td(
                  if (stat$na_count > 0) {
                    tags$span(
                      class = "text-warning",
                      stat$na_count
                    )
                  } else {
                    tags$span(class = "text-success", "0")
                  }
                )
              )
            })
          )
        )
      )
    )
  }

  factors_div <- NULL
  factors_div_class <- "col-lg-12"
  if (length(summary_data$numeric_summary)) {
    factors_div_class <- "col-lg-4"
  }

  if (length(summary_data$factor_summary)) {
    factors_div <- tags$div(
      class = factors_div_class,
      tags$h3(
        tags$i(class = "bi bi-tags"),
        "Categorical Columns"
      ),
      lapply(summary_data$factor_summary, function(stat) {
        tags$div(
          class = "card mb-3",
          tags$div(
            class = "card-body",
            tags$h6(
              class = "card-title",
              tags$code(stat$column)
            ),
            tags$p(
              class = "card-text",
              tags$small(
                sprintf("%d unique values", stat$unique_count)
              ),
              tags$br(),
              tags$strong("Most common: "),
              sprintf(
                "'%s' (%d times)",
                stat$most_common,
                stat$most_common_count
              )
            )
          )
        )
      })
    )
  }

  content <- tagList(
    tags$nav(
      "aria-label" = "breadcrumb",
      tags$ol(
        class = "breadcrumb",
        tags$li(
          class = "breadcrumb-item",
          tags$a(href = "/", "Home")
        ),
        tags$li(
          class = "breadcrumb-item active",
          "aria-current" = "page",
          info$title
        )
      )
    ),

    tags$div(
      class = "row mb-4",
      tags$div(
        class = "col",
        tags$h1(info$title),
        tags$p(class = "lead", info$description),
        tags$div(
          class = "d-flex gap-3 mb-3",
          tags$span(
            class = "badge bg-primary",
            sprintf("%d rows", info$rows)
          ),
          tags$span(
            class = "badge bg-success",
            sprintf("%d columns", info$columns)
          )
        ),
        tags$div(
          class = "btn-group",
          tags$a(
            href = sprintf("/api/datasets/%s/summary", dataset_name),
            class = "btn btn-outline-primary btn-sm",
            tags$span(
              tags$i(class = "bi bi-file-earmark-text"),
              "JSON Summary"
            )
          ),
          tags$a(
            href = sprintf("/api/datasets/%s/data?limit=10", dataset_name),
            class = "btn btn-outline-success btn-sm",
            tags$span(
              tags$i(class = "bi bi-bar-chart"),
              "Raw Data (JSON)"
            )
          )
        )
      )
    ),

    tags$div(
      class = "row",
      numerics_div,
      factors_div
    ),

    tags$div(
      class = "mt-4",
      tags$h3(
        tags$i(class = "bi bi-card-list"),
        "Sample Data (First 5 rows)"
      ),
      tags$div(
        class = "table-responsive",
        create_data_table(summary_data$sample_data)
      )
    )
  )

  create_bootstrap_page(sprintf("%s - Dataset Details", info$title), content)
}

#' Create a data table
create_data_table <- function(data) {
  if (!nrow(data)) {
    return(tags$p("No data available."))
  }

  tags$table(
    class = "table table-striped table-sm",
    tags$thead(
      class = "table-dark",
      tags$tr(
        lapply(names(data), function(col) {
          tags$th(col)
        })
      )
    ),
    tags$tbody(
      lapply(seq_len(nrow(data)), function(i) {
        tags$tr(
          lapply(names(data), function(col) {
            value <- data[[col]][i]
            tags$td(
              if (is.na(value)) {
                tags$em(class = "text-muted", "NA")
              } else if (is.numeric(value)) {
                if (value == round(value)) value else round(value, 3)
              } else {
                as.character(value)
              }
            )
          })
        )
      })
    )
  )
}

#' Create API info page
create_api_page <- function() {
  content <- tagList(
    tags$h1(
      tags$i(class = "bi bi-link-45deg"),
      "API Documentation"
    ),
    tags$p(
      class = "lead",
      "The same data available through the web interface is also accessible via JSON API."
    ),

    tags$h2("Endpoints"),

    tags$div(
      class = "card mb-4",
      tags$div(
        class = "card-header bg-success bg-gradient text-white",
        tags$h5(
          class = "mb-0",
          tags$span(
            class = "badge bg-light text-success me-2",
            "GET"
          ),
          "/api/datasets"
        )
      ),
      tags$div(
        class = "card-body",
        tags$p(
          "List all available datasets with basic information."
        ),
        tags$a(
          href = "/api/datasets",
          class = "btn btn-outline-dark btn-sm",
          tags$span(
            "Try it ",
            tags$i(class = "bi bi-arrow-right")
          )
        )
      )
    ),

    # GET /api/datasets/:name
    tags$div(
      class = "card mb-4",
      tags$div(
        class = "card-header bg-success bg-gradient text-white",
        tags$h5(
          class = "mb-0",
          tags$span(
            class = "badge bg-light text-success me-2",
            "GET"
          ),
          "/api/datasets/{name}/summary"
        )
      ),
      tags$div(
        class = "card-body",
        tags$p(
          "Get detailed summary statistics for a specific dataset."
        ),
        tags$div(
          class = "btn-group",
          tags$a(
            href = "/api/datasets/mtcars/summary",
            class = "btn btn-outline-dark btn-sm",
            tags$span(
              "mtcars ",
              tags$i(class = "bi bi-arrow-right")
            )
          ),
          tags$a(
            href = "/api/datasets/iris/summary",
            class = "btn btn-outline-dark btn-sm",
            tags$span(
              "iris ",
              tags$i(class = "bi bi-arrow-right")
            )
          ),
          tags$a(
            href = "/api/datasets/airquality/summary",
            class = "btn btn-outline-dark btn-sm",
            tags$span(
              "airquality ",
              tags$i(class = "bi bi-arrow-right")
            )
          )
        )
      )
    ),

    # GET /api/datasets/:name/data
    tags$div(
      class = "card mb-4",
      tags$div(
        class = "card-header bg-success bg-gradient text-white",
        tags$h5(
          class = "mb-0",
          tags$span(
            class = "badge bg-light text-success me-2",
            "GET"
          ),
          "/api/datasets/{name}/data"
        )
      ),
      tags$div(
        class = "card-body",
        tags$p("Get raw dataset data in JSON format."),
        tags$p(
          tags$strong("Query Parameters:"),
          tags$br(),
          tags$code("limit"),
          " - Limit the number of rows returned (optional)"
        ),
        tags$div(
          class = "btn-group",
          tags$a(
            href = "/api/datasets/mtcars/data?limit=5",
            class = "btn btn-outline-dark btn-sm",
            tags$span(
              "mtcars (5 rows) ",
              tags$i(class = "bi bi-arrow-right")
            )
          ),
          tags$a(
            href = "/api/datasets/iris/data?limit=10",
            class = "btn btn-outline-dark btn-sm",
            tags$span(
              "iris (10 rows) ",
              tags$i(class = "bi bi-arrow-right")
            )
          )
        )
      )
    ),

    tags$div(
      class = "mt-4 p-4 bg-light shadow-sm rounded",
      tags$h4(
        class = "text-success",
        tags$i(class = "bi bi-lightbulb"),
        "Note"
      ),
      tags$p(
        "All API endpoints return JSON data. The browser will display raw JSON, ",
        "but you can use tools like ",
        tags$code("curl"),
        " or browser extensions to format it nicely."
      ),
      tags$code(
        "curl http://localhost:3000/api/datasets/mtcars | jq ."
      )
    )
  )

  create_bootstrap_page("API Documentation", content, show_api_link = FALSE)
}

#' Create error page
create_error_page <- function(title, message) {
  content <- tagList(
    tags$div(
      class = "text-center",
      tags$h1(class = "display-1 text-muted", "404"),
      tags$h2(title),
      tags$p(class = "lead", message),
      tags$a(
        href = "/",
        class = "btn btn-primary",
        tags$span(
          tags$i(class = "bi bi-arrow-left"),
          "Back to Home"
        )
      )
    )
  )

  create_bootstrap_page(title, content)
}
