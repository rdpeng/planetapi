## Planet Labs API

## Order API
orderurl <- "https://api.planet.com/compute/ops/orders/v2"

## Search API
searchurl <- "https://api.planet.com/data/v1"

#' Pretty Print JSON
#'
#' Print JSON for a query nicely
#'
#' @param x R object to be converted to JSON
#' @importFrom jsonlite toJSON
#' @export
jsonit <- function(x, ...) {
        toJSON(x, pretty = TRUE, auto_unbox = TRUE)
}


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
        d <- suppressMessages(fromJSON(as.character(r)))
        d$pct_used <- with(d, round(100 * quota_used / quota_sqkm, 1))
        d[, c("quota_used", "quota_sqkm", "pct_used")]
}


#' Search Planet API
#'
#' Post a search query to the Planet Labs API
#'
#' @param searchq Search query represented as an R object
#'
#' @return An object containing the search results
#'
#' @importFrom httr POST upload_file
#' @importFrom utils URLencode
#' @importFrom jsonlite fromJSON write_json
#' @export
#'
plnt_search <- function(searchq) {
        auth <- get_auth()

        ## Write it out
        searchJSONfile <- paste(tempfile(), "json", sep = ".")
        write_json(searchq, searchJSONfile,
                   auto_unbox = TRUE, pretty = TRUE)

        ## Search
        search_string <- file.path(searchurl,
                                   URLencode(paste0("quick-search",
                                                    "?_sort=acquired asc")))
        POST(search_string,
             auth,
             body = upload_file(searchJSONfile))
}

#' Get All IDs from Search
#'
#' Follow _next URLs to get all IDs when spread across pages
#'
#' @param Result from search query
#'
#' @returns a character vector of IDs
#'
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @export
#'
get_all_ids <- function(r) {
        auth <- get_auth()
        result <- suppressMessages(fromJSON(as.character(r)))
        idlist <- list()
        i <- 1
        nextlink <- result$`_links`$`_next`
        while(!is.null(nextlink)) {
                idlist[[i]] <- result$features$id
                r <- GET(nextlink, auth)
                result <- suppressMessages(fromJSON(as.character(r)))
                nextlink <- result$`_links`$`_next`
                i <- i + 1
        }
        unlist(idlist, use.names = FALSE)
}




#' Order Planet API
#'
#' Post an order query to the Planet Labs API
#'
#' @param ord R object containing the details of the order, such as filters, delivery, or notifications
#'
#' @return A list containing the order results
#'
#' @importFrom httr POST upload_file
#' @importFrom jsonlite write_json
#' @export
#'
plnt_order <- function(ord) {
        auth <- get_auth()
        orderJSONfile <- paste(tempfile(), "json", sep = ".")
        write_json(ord, orderJSONfile,
                   pretty = TRUE, auto_unbox = TRUE)

        POST(orderurl,
             auth,
             body = upload_file(orderJSONfile)
        )
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
#' @import ggplot2
#' @export
#'
plnt_plot_visual <- function(x, ...) {
        col <- with(x@data, rgb(band1, band2, band3, maxColorValue = 255))
        dxy <- x@grid@cells.dim
        gr <- expand.grid(x = 1:dxy[1], y = dxy[2]:1)
        gr$col <- col
        gr %>%
                ggplot(aes(x, y)) +
                geom_raster(fill = col) +
                coord_fixed() +
                theme_bw()
}



#' Download Zip Bundle
#'
#' Download files associated with an order ID
#'
#' @param order_id the Planet order ID
#' @param ddir directory into which file should be downloaded
#'
#' @export
#' @importFrom utils download.file
#'
download_order <- function(order_id, ddir = "data") {
        out <- plnt_status(order_id, silent = TRUE)
        if(out$state != "success")
                stop("cannot download order; order state: ", out$state)
        nms <- basename(out$`_links`$results$name)
        i <- grep("manifest.json", nms, fixed = TRUE, invert = TRUE)
        if(length(i) > 1) {
                cat(nms[i], sep = "\n")
                stop("more than one file to download")
        }
        destfile <- file.path(ddir, nms[i])
        download.file(out$`_links`$results$location[i], destfile)
        invisible(destfile)
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





#' Compute AOI Size
#'
#' Check the size of an AOi in km^2
#'
#' @param jsonfile path to geojson file
#'
#' @return 'units' object with area in km^2
#'
#' @export
#' @importFrom sf read_sf st_area
#' @import units
#'
aoi_size <- function(jsonfile) {
        aoi <- read_sf(jsonfile, quiet = TRUE)
        area <- st_area(aoi)
        units(area) <- make_units(km^2)
        area
}


#' Check All Orders
#'
#' Get a table of all orders made
#'
#' @return a data frame of orders containing the time created, order ID, state, and name
#' @export
#' @import tibble
#' @import dplyr
#' @importFrom httr GET
#' @importFrom lubridate ymd_hms
#' @importFrom jsonlite fromJSON
#'
check_orders <- function() {
        auth <- get_auth()
        r <- GET(orderurl, auth)
        response <- suppressMessages(fromJSON(as.character(r)))
        with(response$orders, tibble(created = created_on,
                                     id = id,
                                     state = state,
                                     name = name)) %>%
                mutate(created = ymd_hms(created)) %>%
                arrange(desc(created))
}


#' Check Status of an Order
#'
#' Return the status information for an order ID
#'
#' @param order_id Order ID
#' @param silent should status summary be printed?
#'
#' @import dplyr
#' @import tibble
#' @importFrom tidyr pivot_longer
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @export
#'
plnt_status <- function(order_id, silent = FALSE) {
        auth <- get_auth()
        status <- GET(paste(orderurl, order_id, sep = "/"), auth)
        out <- suppressMessages({
                status %>%
                        as.character() %>%
                        fromJSON()
        })
        showvars <- c("name", "created_on", "last_message", "state")
        if(all(showvars %in% names(out))
           && !silent) {
                summary <- out[showvars] %>%
                        as_tibble() %>%
                        rename(order_name = name) %>%
                        pivot_longer(everything())
                print(summary)
        }
        invisible(out)
}







