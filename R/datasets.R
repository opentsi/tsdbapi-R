
#' List datasets
#' 
#' Read information on existing time series datasets.
#' 
#' @family dataset functions
#' @return Table with a row for every existing dataset.
#' @export
list_datasets <- function() {
  
  url <- paste0(base_url(), "datasets")
  res <- req_base(url) |> httr2::req_perform()
  jsonlite::fromJSON(httr2::resp_body_string(res)) |> as.data.frame()
}

#' Create a dataset
#' 
#' Create a time series dataset. Every time series can belong to only one dataset.
#'
#' @inheritParams param_defs
#' @family dataset functions
#' @param description Description of the dataset.
#' @export
create_dataset <- function(
    dataset,
    description) {
  url <- paste0(base_url(), "datasets/", dataset)
  
  data <- list(description=description)
  
  res <- req_base(url) |>
    httr2::req_method("PUT") |> 
    httr2::req_body_json(data) |>
    httr2::req_perform()
  
  cat_message(res)
}

#' Delete dataset
#' 
#' Permanently delete all time series in a dataset, including all vintages and metadata, and the dataset itself.
#'
#' @inheritParams param_defs
#' @family dataset functions
#' @export
delete_dataset <- function(
    dataset,
    skip_check = F) {
  
  if(!skip_check) {
    message("This operation will PERMANENTLY delete the specified time series, including all vintages and metadata. If this is what you intend to do, please type yes below.")
    ans <- readline("answer: ")
    
    if (ans != "yes") {
      stop(sprintf("You typed %s, aborting.", ans))
    }
  }
  
  url <- paste0(base_url(), "datasets/", dataset)

  res <- req_base(url) |>
    httr2::req_method("DELETE") |>
    httr2::req_perform()

  cat_message(res)
}

#' Read dataset time series
#' 
#' Read the time series in a dataset. The time series vintage is specified by the valid_on parameter.
#'
#' @inheritParams param_defs
#' @family dataset functions
#' @inherit read_ts return
#' @export
read_dataset_ts <- function(
    dataset,
    valid_on = Sys.Date(),
    ignore_missing = F) {
  
  url <- paste0(base_url(), "datasets/", dataset, "/ts")
  
  res <- req_base(url) |>
    httr2::req_url_query(
      df = "Y-m-d",
      mime = "json",
      valid_on = as.character(valid_on),
      ignore_missing = to_bool_query_param(ignore_missing)) |>
    httr2::req_perform()
  
  data <- jsonlite::fromJSON(httr2::resp_body_string(res), simplifyDataFrame = F)
  names(data) <- purrr::map_chr(data, "ts_key")
  lapply(data, json_to_ts)
}

#' Read dataset time series metadata
#' 
#' Read the time series metadata of a particular locale of the time series in a dataset.
#'
#' @inheritParams param_defs
#' @family dataset functions
#' @inherit read_ts_metadata return
#' @export
read_dataset_ts_metadata <- function(
    dataset,
    locale = NULL,
    ignore_missing = F) {
  
  url <- paste0(base_url(), "datasets/", dataset ,"/ts/metadata")
  
  res <- req_base(url) |>
    httr2::req_url_query(locale = locale, ignore_missing = to_bool_query_param(ignore_missing)) |>
    httr2::req_perform()
  
  jsonlite::fromJSON(httr2::resp_body_string(res))
}

#' Read dataset time series history
#' 
#' Read multiple vintages of the time series in a dataset The vintage range is given by the valid_from and valid_to parameter. By default, the entire history is read.
#'
#' @inheritParams param_defs
#' @family dataset functions
#' @inherit read_ts_history return
#' @export
read_dataset_ts_history <- function(
    dataset,
    valid_from = as.Date("1900-01-01"),
    valid_to = Sys.Date(),
    ignore_missing = F) {
  
  url <- paste0(base_url(), "datasets/", dataset ,"/ts/history")
  
  res <- req_base(url) |>
    httr2::req_url_query(
      df = "Y-m-d",
      mime = "json",
      valid_from = as.character(valid_from),
      valid_to = as.character(valid_to),
      ignore_missing = to_bool_query_param(ignore_missing)) |>
    httr2::req_perform()
  
  data <- jsonlite::fromJSON(httr2::resp_body_string(res), simplifyDataFrame = F)
  names(data) <- purrr::map_chr(data, ~paste0(.x$ts_key, "_", .x$vintage_date))
  lapply(data, json_to_ts)
}

#' Read dataset time series keys
#' 
#' Read the keys of the time series in a dataset.
#'
#' @inheritParams param_defs
#' @family dataset functions
#' @return Character vector of time series keys.
#' @export
read_dataset_keys <- function(dataset) {
  
  url <- paste0(base_url(), "datasets/", dataset, "/keys")
  
  res <- req_base(url) |> 
    httr2::req_url_query(mime="csv") |>
    httr2::req_perform()
  
  jsonlite::fromJSON(httr2::resp_body_string(res)) |> as.character()
}

#' Read dataset time series vintage write time
#'
#' Read the time at which a time series vintage was written. The vintage is specified by the valid_on parameter.
#'
#' @inheritParams param_defs
#' @family dataset functions
#' @inherit read_ts_write_time return
#' @export
read_dataset_ts_write_time <- function(
    dataset,
    valid_on = Sys.Date(),
    ignore_missing = F) {
  
  url <- paste0(base_url(), "datasets/", dataset, "/ts/write-time")
  
  res <- req_base(url) |>
    httr2::req_url_query(
      valid_on=as.character(valid_on),
      ignore_missing=to_bool_query_param(ignore_missing)) |>
    httr2::req_perform()
  
  jsonlite::fromJSON(httr2::resp_body_string(res)) |> as.data.frame()
}

#' Read dataset time series vintage release
#'
#' Read the vintage release information of the time series in a dataset. The vintage is specified by the valid_on parameter.
#'
#' @inheritParams param_defs
#' @family dataset functions
#' @inherit read_ts_release return
#' @export
read_dataset_ts_release <- function(
    dataset,
    valid_on = Sys.Date(),
    ignore_missing = F) {
  
  url <- paste0(base_url(), "datasets/", dataset, "/ts/release")
  
  res <- req_base(url) |>
    httr2::req_url_query(
      valid_on=as.character(valid_on),
      ignore_missing=to_bool_query_param(ignore_missing)) |>
    httr2::req_perform()
  
  jsonlite::fromJSON(httr2::resp_body_string(res)) |> as.data.frame()
}

#' Read dataset time series release history
#' 
#' Read the release information of multiple time series vintages. The vintage range is given by the valid_from and valid_to parameter. By default, the entire release history is read.
#'
#' @inheritParams param_defs
#' @family dataset functions
#' @inherit read_ts_release_history return
#' @export
read_dataset_ts_release_history <- function(
    dataset,
    valid_from = as.Date("1900-01-01"),
    valid_to = Sys.Date(),
    ignore_missing = F) {
  
  url <- paste0(base_url(), "datasets/", dataset, "/ts/release/history")
  
  res <- req_base(url) |>
    httr2::req_url_query(
      valid_from=as.character(valid_from),
      valid_to=as.character(valid_to),
      ignore_missing=to_bool_query_param(ignore_missing)) |>
    httr2::req_perform()
  
  jsonlite::fromJSON(httr2::resp_body_string(res)) |> as.data.frame()
}

#' Read dataset time series release future
#'
#' Read the release information of future time series vintages.
#'
#' @inheritParams param_defs
#' @family dataset functions
#' @inherit read_ts_release_future return
#' @export
read_dataset_ts_release_future <- function(
    dataset,
    ignore_missing = F) {
  
  url <- paste0(base_url(), "datasets/", dataset, "/ts/release/future")
  
  res <- req_base(url) |>
    httr2::req_url_query(
      ignore_missing=to_bool_query_param(ignore_missing)) |>
    httr2::req_perform()
  
  jsonlite::fromJSON(httr2::resp_body_string(res)) |> as.data.frame()
}

#' Write dataset time series release
#'
#' Write the release information for a vintage of the time series in a dataset. The vintage is specified by the valid_on parameter.
#'
#' @inheritParams param_defs
#' @family dataset functions
#' @export
write_dataset_ts_release <- function(
    dataset,
    release_topic,
    release_year,
    release_period,
    valid_on = Sys.Date(),
    ignore_missing = F) {
  
  url <- paste0(base_url(), "datasets/", dataset, "/ts/release")
  
  data <- list(
    release_topic=jsonlite::unbox(release_topic),
    release_year=jsonlite::unbox(release_year),
    release_period=jsonlite::unbox(release_period),
    valid_on=jsonlite::unbox(as.character(valid_on)),
    ignore_missing=jsonlite::unbox(to_bool_query_param(ignore_missing)))
  
  res <- req_base(url) |>
    httr2::req_method("PATCH") |> 
    httr2::req_body_json(data, auto_unbox = F) |> 
    httr2::req_perform()
  
  cat_message(res)
}

