## Viz routines


#' Plot Visual Image
#'
#' Plot a Visual Satellite Image
#'
#' @param x a GDAL object
#'
#' @description Combine values in red (band3), green (band2), and blue (band1) channels to create a "color" image
#'
#' @import ggplot2
#' @export
#'
plnt_plot_visual <- function(x) {
        col <- with(x@data, rgb(band1, band2, band3, maxColorValue = 255))
        dxy <- x@grid@cells.dim
        gr <- expand.grid(x = 1:dxy[1], y = dxy[2]:1)
        gr$col <- col
        gr %>%
                ggplot(aes(x, y)) +
                geom_raster(fill = col) +
                coord_fixed() +
                theme_void()
}


#' @importFrom stats sd plogis
normalize_band <- function(x, mult = 1, shift = 0, max_value = 255) {
        mx <- mean(x, na.rm = TRUE) - shift
        scale <- sd(x, na.rm = TRUE) * mult
        y <- plogis(x, location = mx, scale = scale)
        y <- y * max_value
        y[is.na(y)] <- 0
        y
}

#' Plot Image
#'
#' Plot Any Satellite Image via RGB
#'
#' @param x a GDAL object
#' @param mult scale factor for normalizing color bands
#' @param shift shift factor for normalizing color bands
#'
#' @description Normalize color bands in RGB and then combine values in red (band3), green (band2), and blue (band1) channels to create a "color" image
#'
#' @export
#'
plnt_plot_any <- function(x, mult = 1, shift = 0) {
        x@data$band3 <- normalize_band(x@data$band3, mult, shift)
        x@data$band2 <- normalize_band(x@data$band2, mult, shift)
        x@data$band1 <- normalize_band(x@data$band1, mult, shift)
        col <- with(x@data, rgb(band3, band2, band1, maxColorValue = 255))
        dxy <- x@grid@cells.dim
        gr <- expand.grid(x = 1:dxy[1], y = dxy[2]:1)
        gr$col <- col
        gr %>%
                ggplot(aes(x, y)) +
                geom_raster(fill = col) +
                coord_fixed() +
                theme_void()
}






