## Planet Labs API

## Order API
orderurl <- "https://api.planet.com/compute/ops/orders/v2"

## Search API
searchurl <- "https://api.planet.com/data/v1"


#' Get API Authorization
#'
#' Get Planet API Key Authorization
#'
#' @importFrom httr authenticate
#' @export
#'
get_auth <- function() {
        key <-  Sys.getenv("PLANET_API_KEY")
        if(!nzchar(key)) {
                stop("key is length 0; make sure to set your API key as an environment variable")
        }
        authenticate(key, "")
}

#' Check Quota
#'
#' Check Current Download Quota
#'
#' @description Check your current download quota and how much of it you have used. The quota is measured in square kilometers
#'
#' @return A data frame with columns 'quota_used', 'quota_sqkm', and 'pct_used'
#'
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @export
#'
plnt_quota <- function() {
        auth <- get_auth()
        r <- GET("https://api.planet.com/auth/v1/experimental/public/my/subscriptions",
                 auth)
        d <- fromJSON(as.character(r))
        d$pct_used <- with(d, round(100 * quota_used / quota_sqkm, 1))
        d[, c("quota_used", "quota_sqkm", "pct_used")]
}



#' Build Search Query
#'
#' Create JSON for search query
asset_filter <- function(asset_type) {
        list(type = "AssetFilter",
             config = as.list(asset_type))
}

date_range_filter <- function(start, end) {
        list(type = "DateRangeFilter",
             field_name = "acquired",
             config = list(
                     gte = paste0(start, "T00:00:00.000Z"),
                     lte = paste0(end, "T00:00:00.000Z")
             ))
}

geometry_filter <- function(aoi_geojson) {
        json <- read_json(aoi_geojson)
        list(type = "GeometryFilter",
             field_name = "geometry",
             config = json$features[[1]]$geometry)
}

#' Search Planet API
#'
#' Post a search query to the Planet Labs API
#'
#' @param details_json path to JSON file containing the details of the search, such as filters
#'
#' @return A list containing the search results
#'
#' @importFrom httr POST upload_file
#' @importFrom utils URLencode
#' @importFrom jsonlite fromJSON
#' @export
#'
plnt_search <- function(details_json) {
        auth <- get_auth()
        search_string <- file.path(searchurl,
                                   URLencode(paste0("quick-search",
                                                    "?_sort=acquired asc",
                                                    "&_page_size=50")))
        r <- POST(search_string,
                  auth,
                  body = upload_file(details_json))
        fromJSON(as.character(r))
}


#' Order Planet API
#'
#' Post an order query to the Planet Labs API
#'
#' @param details_json path to JSON file containing the details of the order, such as filters, delivery, or notifications
#'
#' @return A list containing the order results
#'
#' @importFrom httr POST upload_file
#' @export
#'
plnt_order <- function(details_json) {
        auth <- get_auth()
        r <- POST(orderurl,
                  auth,
                  body = upload_file(details_json))
        r
}


#' Plot Visual Image
#'
#' Plot a Visual Satellite Image
#'
#' @param x a GDAL object
#' @param pch pch for plot (default 15)
#' @param ... other parameters passed to \code{plot}
#'
#' @description Combine values in red (band3), green (band2), and blue (band1) channels to create a "color" image
#'
#' @export
#'
plnt_plot_visual <- function(x, pch = 15, ...) {
        col <- with(x@data, rgb(band3, band2, band1, maxColorValue = 255))
        dxy <- x@grid@cells.dim
        gr <- expand.grid(x = 1:dxy[1], y = dxy[2]:1)
        gr$col <- col
        with(gr, plot(x, y, col = col, pch = pch,
                      asp = 1, ...))
}


normalize_band <- function(x, max_value = 255) {
        x <- (x - min(x, na.rm = TRUE)) / max(x, na.rm = TRUE)
        x <- x * max_value
        x[is.na(x)] <- 0
        x
}

#' Plot Image
#'
#' Plot Any Satellite Image via RGB
#'
#' @param x a GDAL object
#' @param pch pch for plot (default 15)
#' @param ... other parameters passed to \code{plot}
#'
#' @description Normalize color bands in RGB and then combine values in red (band3), green (band2), and blue (band1) channels to create a "color" image
#'
#' @export
#'
plnt_plot_any <- function(x, pch = 15, ...) {
        x@data$band3 <- normalize_band(x@data$band3)
        x@data$band2 <- normalize_band(x@data$band2)
        x@data$band1 <- normalize_band(x@data$band1)
        col <- with(x@data, rgb(band3, band2, band1, maxColorValue = 255))
        dxy <- x@grid@cells.dim
        gr <- expand.grid(x = 1:dxy[1], y = dxy[2]:1)
        gr$col <- col
        with(gr, plot(x, y, col = col, pch = pch,
                      asp = 1, ...))
}








