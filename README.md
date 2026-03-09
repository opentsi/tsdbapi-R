# Interact with the KOF Time Series Database API

The **tsdbapi** package is an R wrapper for the [KOF Time Series DB API](https://tsdb-api.kof.ethz.ch/v2/docs).
The functionality of the package includes reading time series data, metadata and release information from the KOF time series database.

## Installation

Install the package directly from the Github repository with
``` r
devtools::install_github("KOF-ch/tsdbapi-R")
```
The package is not yet published on CRAN.

## Basic Usage

Use the function `read_ts` to read time series from the KOF time series database. The code below reads the time series
with the keys *ch.kof.globalbaro.coincident* and *ch.kof.globalbaro.leading* and returns them as a list of objects of class `ts`.
``` r
tsdbapi::read_ts(ts_keys=c("ch.kof.globalbaro.coincident","ch.kof.globalbaro.leading"))
```

### Authorization

When running the code above, you will be redirected to KOF's identity provider (Keycloak), where you have to log in with your KOF credentials (unless you are already logged in). If you do not have a KOF account, you can still access the public time series in the KOF time series database by setting the `access_type` to *public*:
``` r
tsdbapi::set_config(access_type = "public")
tsdbapi::read_ts(ts_keys="ch.kof.barometer")
```
The time series *ch.kof.barometer* is a public time series.

For **programmatic access** without user log in, for example in a non-interactive session, you must use an API key. Create an API key for your user with:
``` r
api_key <- tsdbapi::create_user_api_key()
print(api_key)
```
Any previously created API key will be overwritten and therefore invalidated. Store the newly created API key securely because it cannot be retrieved later.

Use the API key by setting the environment variable `TSDBAPI_API_KEY` before running an R script. Alternatively, you can set the corresponding package configuration option:
``` r
tsdbapi::set_config(api_key="my_api_key")
tsdbapi::read_ts(ts_keys="ch.kof.barometer")
```

### Vintages

Every time series can have multiple vintages (or versions). A time series vintage is based on the data available at its **vintage date**.

By default, `read_ts` returns the most recent vintage (or version) of the time series. 
To specify a different vintage, use the `valid_on` parameter. The code below reads the KOF barometer vintage based on the data available on January 15, 2026.
``` r
tsdbapi::read_ts("ch.kof.barometer", valid_on = "2026-01-15")
```
For users with role *extern* (everyone not employed at KOF), a time series vintage is only visible once it has been officially released, hence its data, including its vintage date, can only be read after release. A time series vintage is usually released several days after its vintage date.

### Release information

The release information, including the release time, of the most recent time series vintage can be read with
``` r
tsdbapi::read_ts_release(ts_keys="ch.kof.barometer")
```
Use the `valid_on` parameter to specify a different vintage. For users with the role *extern*, only vintages that have been released are visible. However, the release information of future, yet to be released time series vintages can be read with
``` r
tsdbapi::read_ts_release_future(ts_keys="ch.kof.barometer")
```
Note that the release time of a future vintage is not guaranteed and is subject to change (although changes are rare).

### Download Quota

If you are a KOF data service subscriber, the number of time series downloads (reads) per year is limited by a quota. You can check your annual download quota and the number of time series downloads remaining in the current subscription year with
``` r
tsdbapi::read_user_quota()
```

### Collections
To read an entire collection of time series use
``` r
tsdbapi::read_collection_ts(collection="bs_indicator", owner="public")
```
Every time series collection has an owner. By default, the owner is assumed to be yourself (`owner="self"`).

You can list all collections you are allowed to see with
``` r
tsdbapi::list_collections()
```
For users with the role *extern*, the includes all collections owned by the user himself and by the user *public*.

### Metadata

The metadata of one or multiple time series can be read with
``` r
tsdbapi::read_ts_metadata(ts_keys="ch.kof.barometer")
```
To read the metadata of an entire collection of time series:
``` r
tsdbapi::read_collection_ts_metadata(collection="bs_indicator", owner="public")
```

## Advanced usage

Consult the package help for a detailed documentation of all available functions.