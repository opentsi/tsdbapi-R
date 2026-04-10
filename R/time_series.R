
ts_base_url <- function() {
  paste0(base_url(), "ts/")
}

#' Read time series
#' 
#' Read time series given by their unique identifiers (keys). The vintage is specified by the valid_on parameter.
#' By default, the most recent vintage is read.
#'
#' @inheritParams param_defs
#' @family time series functions
#' @return List of time series. Regular time series have the class ts and irregular time series the class xts.
#' @export
read_ts <- function(
    ts_keys,
    valid_on = Sys.Date(),
    ignore_missing = F) {

  url <- ts_base_url()
  
  res <- req_base(url) |>
    httr2::req_url_query(
      df="Y-m-d",
      mime="json",
      keys=paste0(ts_keys, collapse = ","),
      valid_on=as.character(valid_on),
      ignore_missing=to_bool_query_param(ignore_missing)) |>
    httr2::req_perform()
  
  data <- jsonlite::fromJSON(httr2::resp_body_string(res), simplifyDataFrame = F)
  names(data) <- purrr::map_chr(data, "ts_key")
  lapply(data, json_to_ts)
}

#' Read time series history
#' 
#' Read multiple time series vintages. The vintage range is given by the valid_from and valid_to parameters. By default, the entire history is read.
#'
#' @inheritParams param_defs
#' @family time series functions
#' @return  List of time series.
#' Regular time series have the class ts and irregular time series the class xts.
#' The name of each time series is the time series key concatenated with the vintage date in format YYYYMMDD.
#' @export
read_ts_history <- function(
    ts_keys,
    valid_from = NULL,
    valid_to = Sys.Date(),
    ignore_missing = F) {
  
  url <- paste0(ts_base_url(), "history")
  
  res <- req_base(url) |>
    httr2::req_url_query(
      df="Y-m-d",
      mime="json",
      keys=paste0(ts_keys, collapse = ","),
      valid_from=as.character(valid_from),
      valid_to=as.character(valid_to),
      ignore_missing=to_bool_query_param(ignore_missing)) |>
    httr2::req_perform()

  data <- jsonlite::fromJSON(httr2::resp_body_string(res), simplifyDataFrame = F)
  names(data) <- purrr::map_chr(data, ~paste0(.x$ts_key, "_", stringr::str_replace_all(.x$vintage_date, "-", "")))
  lapply(data, json_to_ts)
}

#' Read time series metadata
#' 
#' Read the time series metadata of a particular locale.
#'
#' @inheritParams param_defs
#' @family time series functions
#' @return List of time series metadata. Each list element is named by the corresponding time series key and contains the metadata as a named list.
#' @export
read_ts_metadata <- function(
    ts_keys,
    locale = NULL,
    ignore_missing = F) {
  
  url <- paste0(ts_base_url(), "metadata")
  
  res <- req_base(url) |>
    httr2::req_url_query(
      locale=locale, 
      keys=paste0(ts_keys, collapse = ","),
      ignore_missing=to_bool_query_param(ignore_missing)) |>
    httr2::req_perform()
  
  jsonlite::fromJSON(httr2::resp_body_string(res))
}

#' Write time series
#' 
#' Write time series as a particular vintage (given by the valid_from parameter). 
#'
#' @inheritParams param_defs
#' @param valid_from Vintage date of the time series. Defaults to the current date.
#' @param access_sets Names of the access sets the time series are added to.
#' If new time series are written, you must provide at least one access set name.
#' @family time series functions
#' @export
write_ts <- function(
  ts_list,
  valid_from = Sys.Date(),
  access_sets = character(),
  release_topic = NULL,
  release_year = NULL,
  release_period = NULL,
  access = NULL,
  pre_release_access = NULL,
  release_date = NULL) {
  
  url <- ts_base_url()

  data <- list(
    valid_from=jsonlite::unbox(valid_from),
    release_date=jsonlite::unbox(lubridate::format_ISO8601(release_date, usetz = "Z")),
    release_topic=jsonlite::unbox(release_topic),
    release_year=jsonlite::unbox(release_year),
    release_period=jsonlite::unbox(release_period),
    access_sets=access_sets,
    access=jsonlite::unbox(access),
    pre_release_access=jsonlite::unbox(pre_release_access))
  
  data$ts_data <- unname(purrr::imap(ts_list, ~{
    freq <- if(is.ts(.x)) frequency(.x) else NULL
    list(ts_key=jsonlite::unbox(.y), frequency=jsonlite::unbox(freq), time=zoo::as.Date(.x), value=as.vector(.x))
  }))
  
  res <- req_base(url) |>
    httr2::req_method("PUT") |> 
    httr2::req_body_json(purrr::compact(data), auto_unbox = F, na = "null") |> 
    httr2::req_perform()
  
  cat_message(res)
}

#' Rename time series
#' 
#' Change the time series' unique identifiers (keys).
#' The length of the parameters ts_keys and ts_keys_new must be the same.
#
#' @family time series functions
#' @inheritParams param_defs
#' @param ts_keys Existing time series keys
#' @param ts_keys_new New time series keys
#' @export
rename_ts <- function(
    ts_keys,
    ts_keys_new,
    ignore_missing = F) {
  
  url <- paste0(ts_base_url(), "key")
  
  data <- list(
    keys=ts_keys,
    keys_new=ts_keys_new,
    ignore_missing=jsonlite::unbox(to_bool_query_param(ignore_missing)))
  
  res <- req_base(url) |>
    httr2::req_method("PATCH") |> 
    httr2::req_body_json(data, auto_unbox = F) |> 
    httr2::req_perform()
  
  cat_message(res)
}

#' Read time series vintage write time
#' 
#' Read the time at which a time series vintage was written.
#' The vintage is specified by the valid_on parameter.
#'
#' @inheritParams param_defs
#' @family time series functions
#' @return Table with the vintage write time for every time series key.
#' @export
read_ts_write_time <- function(
    ts_keys,
    valid_on = Sys.Date(),
    ignore_missing = F) {
  
  url <- paste0(ts_base_url(), "write-time")
  
  res <- req_base(url) |>
    httr2::req_url_query(
      keys=paste0(ts_keys, collapse = ","),
      valid_on=as.character(valid_on),
      ignore_missing=to_bool_query_param(ignore_missing)) |>
    httr2::req_perform()
  
  jsonlite::fromJSON(httr2::resp_body_string(res)) |> as.data.frame()
}

#' Read time series vintage release
#'
#' Read the time series vintage release information.
#' The vintage is specified by the valid_on parameter.
#' 
#' @inheritParams param_defs
#' @family time series functions
#' @return Table with vintage date, release topic, year, period and time for every time series key.
#' @export
read_ts_release <- function(
    ts_keys,
    valid_on = Sys.Date(),
    ignore_missing = F) {
  
  url <- paste0(ts_base_url(), "release")
  
  res <- req_base(url) |>
    httr2::req_url_query(
      keys=paste0(ts_keys, collapse = ","),
      valid_on=as.character(valid_on),
      ignore_missing=to_bool_query_param(ignore_missing)) |>
    httr2::req_perform()
  
  jsonlite::fromJSON(httr2::resp_body_string(res)) |> as.data.frame()
}

#' Read time series vintage release history
#'
#' Read the release information of multiple time series vintages.
#' The vintage range is given by the valid_from and valid_to parameters.
#' By default, the entire release history is read.
#' 
#' @inheritParams param_defs
#' @family time series functions
#' @return Table with vintage date, release topic, year, period and time for every time series key.
#' @export
read_ts_release_history <- function(
    ts_keys,
    valid_from = NULL,
    valid_to = NULL,
    ignore_missing = F) {
  
  url <- paste0(ts_base_url(), "release/history")
  
  res <- req_base(url) |>
    httr2::req_url_query(
      keys=paste0(ts_keys, collapse = ","),
      valid_from=as.character(valid_from),
      valid_to=as.character(valid_to),
      ignore_missing=to_bool_query_param(ignore_missing)) |>
    httr2::req_perform()
  
  jsonlite::fromJSON(httr2::resp_body_string(res)) |> as.data.frame()
}

#' Read time series release future
#'
#' Read the release information of future time series vintages.
#' 
#' @inheritParams param_defs
#' @family time series functions
#' @return Table with release topic, year, period and time for every time series key and future release.
#' @export
read_ts_release_future <- function(
    ts_keys,
    ignore_missing = F) {
  
  url <- paste0(ts_base_url(), "release/future")
  
  res <- req_base(url) |>
    httr2::req_url_query(
      keys=paste0(ts_keys, collapse = ","),
      ignore_missing=to_bool_query_param(ignore_missing)) |>
    httr2::req_perform()
  
  jsonlite::fromJSON(httr2::resp_body_string(res)) |> as.data.frame()
}

#' Write time series release
#'
#' Write release information for a time series vintage. The vintage is specified by the valid_on parameter.
#' 
#' @inheritParams param_defs
#' @family time series functions 
#' @export
write_ts_release <- function(
  ts_keys,
  release_topic,
  release_year,
  release_period,
  valid_on = Sys.Date()) {
  
  url <- paste0(ts_base_url(), "release")
  
  data <- list(
    keys=ts_keys,
    release_topic=jsonlite::unbox(release_topic),
    release_year=jsonlite::unbox(release_year),
    release_period=jsonlite::unbox(release_period),
    valid_on=jsonlite::unbox(as.character(valid_on)))
  
  res <- req_base(url) |>
    httr2::req_method("PATCH") |> 
    httr2::req_body_json(data, auto_unbox = F) |> 
    httr2::req_perform()
  
  cat_message(res)
}

#' Write time series metadata
#' 
#' Write time series metadata of a particular locale.
#'
#' @inheritParams param_defs
#' @family time series functions
#' @param metadata_list List of time series metadata named by the time series keys.
#' Each list element contains the metadata of a time series as a named list (key-value pairs).
#' @export
write_ts_metadata <- function(
    metadata_list,
    locale = NULL,
    overwrite = F) {
  
  url <- paste0(ts_base_url(), "metadata")
  
  data <- list(
    overwrite = to_bool_query_param(overwrite),
    locale = locale,
    metadata = unname(purrr::imap(metadata_list, ~{
      list(ts_key=jsonlite::unbox(.y), metadata=.x)
    }))
  )
  
  res <- req_base(url) |>
    httr2::req_method("PATCH") |> 
    httr2::req_body_json(purrr::compact(data)) |> 
    httr2::req_perform()
  
  cat_message(res)
}

#' Write time series dataset
#' 
#' Assign time series to a dataset. Every time series can belong to only one dataset.
#'
#' @inheritParams param_defs
#' @family time series functions
#' @export
write_ts_dataset <- function(
    ts_keys,
    dataset) {
  
  url <- paste0(ts_base_url(), "dataset")
  
  data <- list(keys=ts_keys, dataset=jsonlite::unbox(dataset))
  
  res <- req_base(url) |>
    httr2::req_method("PATCH") |> 
    httr2::req_body_json(data, auto_unbox = F) |> 
    httr2::req_perform()
  
  cat_message(res)
}

#' Read time series dataset
#'
#' Read the dataset the time series belongs to.
#' Every time series can belong to only one dataset.
#' 
#' @inheritParams param_defs
#' @family time series functions
#' @return Table with dataset name for every time series key.
#' @export
read_ts_dataset <- function(
    ts_keys,
    ignore_missing = F) {
  
  url <- paste0(ts_base_url(), "dataset")
  
  res <- req_base(url) |>
    httr2::req_url_query(
      keys=paste0(ts_keys, collapse = ","),
      ignore_missing=to_bool_query_param(ignore_missing)) |>
    httr2::req_perform()
  
  jsonlite::fromJSON(httr2::resp_body_string(res)) |> as.data.frame()
}

  
#' Delete time series
#'
#' Delete time series given by their unique identifiers (keys). The time series are removed completely, including all vintages and metadata.
#' 
#' @family time series functions
#' @inheritParams param_defs
#' @export
delete_ts <- function(
    ts_keys,
    ignore_missing = F,
    skip_check = F) {
  
  if(!skip_check){
    message("This operation will PERMANENTLY delete the specified time series, including all vintages and metadata. If this is what you intend to do, please type yes below.")
    ans <- readline("answer: ")
    
    if (ans != "yes") {
      stop(sprintf("You typed %s, aborting.", ans))
    }
  }
  
  url <- ts_base_url()
  
  data <- list(
    keys=ts_keys,
    ignore_missing=jsonlite::unbox(to_bool_query_param(ignore_missing)))
  
  res <- req_base(url) |>
    httr2::req_method("DELETE") |> 
    httr2::req_body_json(data, auto_unbox = F) |> 
    httr2::req_perform()
  
  cat_message(res)
}


#' Find time series
#' 
#' Find time series keys by regular expression.
#'
#' @param regexp Regular expression pattern to search for time series keys.
#' @family time series functions
#' @return Vector of time series keys matching the regular expression.
#' @export
find_ts <- function(
    regexp) {
  
  url <- paste0(ts_base_url(), "key")
  
  res <- req_base(url) |>
    httr2::req_url_query(regexp=regexp) |>
    httr2::req_perform()
  
  jsonlite::fromJSON(httr2::resp_body_string(res)) |> as.character()
}
