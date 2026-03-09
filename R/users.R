
user_base_url <- function(username) {
  paste0(base_url(), "users/", username, "/")
}

#' List users
#' 
#' Show information on time series database users.
#'
#' @returns Table with a row for every user.
#' @family user management functions
#' @export
list_users <- function() {
  url <- paste0(base_url(), "users")
  res <- req_base(url) |> httr2::req_perform()
  jsonlite::fromJSON(httr2::resp_body_string(res))  
}

#' Create user
#' 
#' Create a time series database user with a given role.
#'
#' @inheritParams param_defs 
#' @param role Role of the user. Must be one of 'admin', 'intern' or 'extern'.
#' @export
create_user <- function(username, role) {
  
  url <- user_base_url(username)
  data <- list(role=role)
  
  res <- req_base(url) |>
    httr2::req_method("PUT") |>
    httr2::req_body_json(data) |>
    httr2::req_perform()
  
  cat_message(res)
}

#' List user access sets
#' 
#' List the access sets and associated permissions of a particular user.
#'
#' @inheritParams param_defs 
#' @returns Table with a row for every user access set.
#' @family user management functions
#' @export
list_user_access_sets <- function(username = "self") {
  url <- paste0(user_base_url(username), "access-sets")
  res <- req_base(url) |> httr2::req_perform()
  jsonlite::fromJSON(httr2::resp_body_string(res)) |> as.data.frame()
}

#' Add user access sets
#' 
#' Add access sets for a user with a given permission.
#'
#' @inheritParams param_defs
#' @family user management functions
#' @export
add_user_access_sets <- function(
    username = "self",
    access_sets,
    permission) {
  
  url <- paste0(user_base_url(username), "access-sets")
  
  data <- list(access_sets=access_sets, operation=jsonlite::unbox("add"), permission=jsonlite::unbox(permission))
  
  res <- req_base(url) |>
    httr2::req_method("PATCH") |>
    httr2::req_body_json(data, auto_unbox = F) |>
    httr2::req_perform()
  
  cat_message(res)
}

#' Remove user access sets
#' 
#' Remove access sets and associated permissions from a user.
#' 
#' @inheritParams param_defs
#' @family user management functions
#' @export
remove_user_access_sets <- function(
    username = "self",
    access_sets) {
  
  url <- paste0(user_base_url(username), "access-sets")
  
  data <- list(access_sets=access_sets, operation=jsonlite::unbox("remove"))
  
  res <- req_base(url) |>
    httr2::req_method("PATCH") |>
    httr2::req_body_json(data, auto_unbox = F) |>
    httr2::req_perform()
  
  cat_message(res)
}

#' Read user quota
#' 
#' Check the number of time series downloads remaining in the current subscription year.
#'
#' @inheritParams param_defs 
#' @return Table with quota information.
#' @export
read_user_quota <- function(username = "self") {

  url <- paste0(user_base_url(username), "quota")
  res <- req_base(url) |> httr2::req_perform()
  jsonlite::fromJSON(httr2::resp_body_string(res)) |> as.data.frame()
}

#' Create user quota
#' 
#' Create an annual quota for a data service subscriber. Without a quota, the user has an unlimited number of time series downloads.
#'
#' @inheritParams param_defs
#' @family user management functions
#' @export
create_user_quota <- function(username, subscription_annual_quota, subscription_start_date) {
  
  url <- paste0(user_base_url(username), "quota")
  data <- list(subscription_start_date=subscription_start_date, subscription_annual_quota=subscription_annual_quota)
  
  res <- req_base(url) |>
    httr2::req_method("PUT") |>
    httr2::req_body_json(data) |>
    httr2::req_perform()
  
  cat_message(res)
}

#' Write user quota
#' 
#' Set a user's quota for the current subscription year and/or the subscription's default annual quota.
#'
#' @inheritParams param_defs
#' @family user management functions
#' @export
write_user_quota <- function(username, current_year_quota = NULL, subscription_annual_quota = NULL) {
  
  url <- paste0(user_base_url(username), "quota")
  data <- list(current_year_quota=current_year_quota, subscription_annual_quota=subscription_annual_quota)
  
  res <- req_base(url) |>
    httr2::req_method("PATCH") |>
    httr2::req_body_json(purrr::compact(data)) |>
    httr2::req_perform()
  
  cat_message(res)
}

#' Delete user quota
#' 
#' Delete the existing quota of a data service subscriber.
#' Without a quota, the user has an unlimited number of time series downloads.
#'
#' @inheritParams param_defs
#' @family user management functions
#' @export
delete_user_quota <- function(username) {
  
  url <- paste0(user_base_url(username), "quota")

  res <- req_base(url) |>
    httr2::req_method("DELETE") |>
    httr2::req_perform()
  
  cat_message(res)
}

#' Create user API key
#' 
#' Create an API key for a user for programmatic access.
#' Any previously created API key for the user will be overwritten and therefore invalidated.
#' Store the newly created API key securely because it cannot be retrieved later.
#' 
#' @inheritParams param_defs
#' @family user management functions
#' @returns List with API key.
#' @export
create_user_api_key <- function(username = "self") {
  url <- paste0(user_base_url(username), "api-key")
  res <- req_base(url) |> httr2::req_perform()
  jsonlite::fromJSON(httr2::resp_body_string(res))
}
