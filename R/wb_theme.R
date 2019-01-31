### THANKS TO https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2 for an excelent guide

#' Function to extract WB colors as hex codes
#'
#' @param ... Character names of drsimonj_colors
#'
wb_col <- function(...) {
  wb_colors <- c(
    `Solid Blue`       = "#002244",
    `Bright Blue`      = "#009FDA",
    `orange`           = "#F05023",
    `yellow`           = "#FDB714",
    `red`              = "#EB1C2D",
    `brown`            = "#F78D28",
    `blue`             = "#009CA7",
    `green`            = "#00AB51",
    `purple`           = "#872B90",
    `light_blue`       = "#00A996")

  cols <- c(...)

  if (is.null(cols))
    return (wb_colors)

  wb_colors[cols]
}

#' Return function to interpolate a drsimonj color palette
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
wb_pal <- function(palette = "main", reverse = FALSE, ...) {

  wb_palettes <- list(
    `main`  = wb_col("red", "yellow", "green", "blue", "purple")
  )

  pal <- wb_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

#' Color scale constructor for drsimonj colors
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_wb <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- wb_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("wb_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for drsimonj colors
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_wb <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- wb_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("wb_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

