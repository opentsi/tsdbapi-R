#' Common parameters
#'
#' @param ts_keys Character vector of time series identifiers (keys)
#' @param valid_on Selects the time series vintage with the vintage date equal to or before this date
#' @param valid_from Start date of the vintage range
#' @param valid_to End date of the vintage range
#' @param ts_list Named list of time series of class, zoo or xts with time series keys as list names
#' @param access_sets Vector of access set names
#' @param access  The access level of the time series. This parameter is obsolete once the Time Series DB API v1 is discontinued.
#' @param pre_release_access The access level of the time series before the time of release. This parameter is obsolete once the Time Series DB API v1 is discontinued.
#' @param release_topic Topic of the release. For example 'kofbaro'.
#' @param release_year Year of the release
#' @param release_period Period of the release. For monthly releases a number from 1 to 12. For quarterly releases a number from 1 to 4. For half-yearly releases a number from 1 to 2. For yearly releases always 1. 
#' @param release_date Time of the release (POSIXct). This parameter is obsolete once the Time Series DB API v1 is discontinued.
#' @param subscription_annual_quota Annual download quota of data service subscription
#' @param current_year_quota Download quota of the current year. It is automatically set to subscription_annual_quota at the start of a subscription year.
#' @param dataset Name of the dataset. A dataset is a group of time series with a common theme. Every time series can belong to only one dataset.
#' @param ignore_missing Whether to ignore missing or forbidden time series when requesting time series data
#' @param access_type One of 'auth' (the default), 'public' or 'preview'. The access types 'public' and 'preview' bypass authentication. Use the access type 'public' to read public time series and the access type 'preview' to read time series previews. Use 'auth' for authenticated access.
#' @param locale The locale of the metadata. Can be any string, but ISO codes are recommended (such as 'en', 'de', 'fr', 'it'). Set to NULL for unlocalized metadata (default).
#' @param owner Username of the owner of the time series collection
#' @param collection Name of the time series collection
#' @param username Username of the time series database user
#' @param access_set Name of the access set. An access set is a group of time series for which a permission (read_quota, read, read_before_release, write) can be granted to individual users. Every time series can be a member of multiple access sets.
#' @param access_sets Names of the access sets. An access set is a group of time series for which a permission (one of read_quota, read, read_before_release or write) can be granted to users. A time series can be a member of multiple access sets.
#' @param permission The permission granted to the user for the time series in the access set. Must be one of read_quota, read, read_before_release or write.
#' @param overwrite If TRUE, the existing metadata is replaced completely. If FALSE (default), new fields are added and existing fields are updated.
#' @param skip_check If FALSE (default), the user must manually confirm the deletion of time series.
#' @param subscription_start_date Start date of the user subscription
#' @param vintage_date character string containing a YYYY-MM-DD formatted date to indicate the date that this particular vintage refers to.
#' @name param_defs
NULL
