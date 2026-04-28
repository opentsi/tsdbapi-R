
.onLoad <- function(libname, pkgname) {
  options(
    tsdbapi.oauth_client_id = Sys.getenv("TSDBAPI_OAUTH_CLIENT_ID", unset = "tsdb-api"),
    tsdbapi.oauth_token_url = Sys.getenv("TSDBAPI_OAUTH_TOKEN_URL", unset = "https://keycloak.kof.ethz.ch/realms/main/protocol/openid-connect/token"),
    tsdbapi.oauth_auth_url = Sys.getenv("TSDBAPI_OAUTH_AUTH_URL", unset = "https://keycloak.kof.ethz.ch/realms/main/protocol/openid-connect/auth"),
    tsdbapi.oauth_auth_device_url = Sys.getenv("TSDBAPI_OAUTH_AUTH_DEVICE_URL", unset = "https://keycloak.kof.ethz.ch/realms/main/protocol/openid-connect/auth/device"),
    tsdbapi.oauth_redirect_url = Sys.getenv("TSDBAPI_OAUTH_REDIRECT_URL", unset = "http://127.0.0.1/"),
    tsdbapi.oauth_flow = Sys.getenv("TSDBAPI_OAUTH_FLOW", unset = if(httr2:::is_hosted_session()) "device" else "code"),
    tsdbapi.api_key = Sys.getenv("TSDBAPI_API_KEY", unset = ""),
    tsdbapi.url_staging = Sys.getenv("TSDBAPI_URL_STAGING", unset = "https://tsdb-api.stage.kof.ethz.ch/v2/"),
    tsdbapi.url_production = Sys.getenv("TSDBAPI_URL_PRODUCTION", unset = "https://tsdb-api.kof.ethz.ch/v2/"),
    tsdbapi.url_test = Sys.getenv("TSDBAPI_URL_TEST", unset = "http://localhost:3001/v2/"),
    tsdbapi.environment = Sys.getenv("TSDBAPI_ENVIRONMENT", unset = "production"),
    tsdbapi.access_type = Sys.getenv("TSDBAPI_ACCESS_TYPE", unset = "auth"),
    tsdbapi.read_before_release = Sys.getenv("TSDBAPI_READ_BEFORE_RELEASE", unset = T)
  )
}


#' Set package configuration
#' 
#' Set the package configuration options for the current session. Every option is initialized with the corresponding environment variable on package load.
#'
#' @family package configuration functions
#' @param oauth_client_id OAuth client ID.
#' @param oauth_token_url OAuth token URL
#' @param oauth_auth_url OAuth authorization URL
#' @param oauth_auth_device_url OAuth device authorization URL
#' @param oauth_flow OAuth authorization flow. Must be either 'device' (default for hosted sessions) or 'code' (default otherwise).
#' @param oauth_redirect_url OAuth redirect URL
#' @param api_key API key for programmatic access. Use \code{\link[tsdbapi]{create_user_api_key}} to create an API key.
#' @param url_staging URL of staging API
#' @param url_production URL of production API
#' @param url_test URL of test API
#' @param environment Whether to use the production, staging or test API. Must be one of 'production', 'staging' or 'test.
#' @param access_type How to access time series data. Must be one of 'auth' (the default), 'public' or 'preview'.
#' The access types 'public' and 'preview' bypass authentication.
#' Use the access type 'public' to read public time series and the access type 'preview' to read time series previews (latest 2 years of data missing).
#' Use 'auth' for authenticated access.
#' @param read_before_release Whether to read time series vintages before their official release. Defaults to TRUE. This option will only have
#' an effect if you have pre release access to the requested time series.
#' @export
#' 
set_config <- function(
  oauth_client_id = NULL,
  oauth_token_url = NULL,
  oauth_auth_url = NULL,
  oauth_auth_device_url = NULL,
  oauth_flow = NULL,
  oauth_redirect_url = NULL,
  api_key = NULL,
  url_staging = NULL,
  url_production = NULL,
  url_test = NULL,
  environment = NULL,
  access_type = NULL,
  read_before_release = NULL
) {
  if(!is.null(oauth_client_id)) {
    options(tsdbapi.oauth_client_id = oauth_client_id)
  }
  if(!is.null(oauth_flow)) {
    options(tsdbapi.oauth_flow = oauth_flow)
  }
  if(!is.null(oauth_token_url)) {
    options(tsdbapi.oauth_token_url = oauth_token_url)
  }
  if(!is.null(oauth_auth_url)) {
    options(tsdbapi.oauth_auth_url = oauth_auth_url)
  }
  if(!is.null(oauth_auth_device_url)) {
    options(tsdbapi.oauth_auth_url = oauth_auth_url)
  }
  if(!is.null(oauth_redirect_url)) {
    options(tsdbapi.oauth_redirect_url = oauth_redirect_url)
  }
  if(!is.null(api_key)) {
    options(tsdbapi.api_key = api_key)
  }
  if(!is.null(url_staging)) {
    options(tsdbapi.url_staging = url_staging)
  }
  if(!is.null(url_production)) {
    options(tsdbapi.url_production = url_production)
  }
  if(!is.null(url_test)) {
    options(tsdbapi.url_test = url_test)
  }
  if(!is.null(environment)) {
    options(tsdbapi.environment = environment)
  }
  if(!is.null(access_type)) {
    options(tsdbapi.access_type = access_type)
  }
  if(!is.null(read_before_release)) {
    options(tsdbapi.read_before_release = read_before_release)
  }
}

#' Get package configuration
#' 
#' Get the package configuration options of the current session. Every option is initialized with the corresponding environment variables on package load.
#'
#' @family package configuration functions
#' @returns Named list of options.
#' @export
get_config <- function() {
  opts <- c(
    "oauth_client_id",
    "oauth_flow",
    "oauth_token_url",
    "oauth_auth_url",
    "oauth_auth_device_url",
    "oauth_redirect_url",
    "api_key",
    "url_staging",
    "url_production",
    "url_test",
    "environment",
    "access_type",
    "read_before_release")
  
  names(opts) <- opts
  purrr::map(opts, ~getOption(paste0("tsdbapi.", .x)))
}

get_oauth_client <- function() {
  httr2::oauth_client(
    id = getOption("tsdbapi.oauth_client_id"),
    token_url = getOption("tsdbapi.oauth_token_url"),
    auth = "body"
  )
} 

req_base <- function(url) {
  
  req <- httr2::request(url) |> httr2::req_error(body = function(res) httr2::resp_body_json(res)$message)
  
  api_key <- getOption("tsdbapi.api_key")
  
  if(getOption("tsdbapi.access_type") == "auth") {
    if(api_key == "") {
      if(getOption("tsdbapi.oauth_flow")=="device") {
        code <- httr2::oauth_flow_auth_code_pkce()
        req <- req |> httr2::req_oauth_device(
          client = get_oauth_client(),
          auth_url = getOption("tsdbapi.oauth_auth_device_url"),
          auth_params = list(code_challenge=code$challenge, code_challenge_method=code$method),
          token_params = list(code_verifier=code$verifier))
      } else {
        req <- req |> httr2::req_oauth_auth_code(
          client = get_oauth_client(),
          redirect_uri = httr2:::normalize_redirect_uri(getOption("tsdbapi.oauth_redirect_url"))$uri,
          auth_url = getOption("tsdbapi.oauth_auth_url"))
      }
    } else {
      req <- req |> httr2::req_headers_redacted("x-api-key" = api_key)
    }
  }
  
  req |> httr2::req_url_query(
    access_type = getOption("tsdbapi.access_type"),
    read_before_release = to_bool_query_param(getOption("tsdbapi.read_before_release"))
  )
}

to_bool_query_param <- function(arg) {
  if(arg) "true" else ""
}

json_to_ts <- function(data) {
  freq <- data$frequency
  res <- xts::xts(data$value, order.by = as.Date(data$time), frequency = freq)
  if (!is.null(freq) && isTRUE(freq > 0)) {
    tsbox::ts_ts(res)
  } else {
    res
  }
}

base_url <- function() {
  envir <- getOption("tsdbapi.environment")
  if(envir == "staging") {
    getOption("tsdbapi.url_staging")
  } else if(envir == "production") {
    getOption("tsdbapi.url_production")
  } else if(envir == "test") {
    getOption("tsdbapi.url_test")
  } else {
    stop("tsdbapi environment ", envir, " not supported")
  }
}

cat_message <- function(res) {
  cat(httr2::resp_body_json(res)$message, "\n")
}

