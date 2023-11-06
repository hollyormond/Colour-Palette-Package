# Colour palette for SLR Consulting



slr_palettes <- list(
  primary_green = c(
   '#EEF7DB',
   '#D6F591',
   '#A9C272',
   '#667545',
   '#3C533C',
   '#263326'
  ),
  shades = c(
   '#F6F6F2',
   '#EBECE7',
   '#D4D9D2',
   '#BEC5BC',
   '#51544A',
   '#1E1E1E'
  ),
  secondary_palette = c(
   '#DDF3F5',
   '#C4F0F5',
   '#ABEEF5',
   '#92ECF5',
   '#679EA3',
   '#3D5254',
   '#DDE4F5',
   '#C4D4F5',
   '#ABC3F5',
   '#92B2F5',
   '#6779A3',
   '#3D4454',
   '#EFDDF5',
   '#E7C4F5',
   '#E0ABF5',
   '#D993F5',
   '#9167A3',
   '#4E3D54',
   '#F5DDE6',
   '#F5C4D7',
   '#F5ABC7',
   '#F593B7',
   '#A3677E',
   '#523B44',
   '#F5E0DD',
   '#F5CCC4',
   '#F5B7AB',
   '#F5A293',
   '#A36F67',
   '#54403D',
   '#F5E9DD',
   '#F5DDC4',
   '#F5D1AB',
   '#F5C492',
   '#A38667',
   '#54493D',
   '#EEF7DB',
   '#D6F591',
   '#A9C272',
   '#667545',
   '#3C533C',
   '#263326'
  ),
  secondary_light = c(
   '#DDF3F5',
   '#ABC3F5',
   '#E7C4F5',
   '#F5C4D7',
   '#F5E0DD',
   '#F5D1AB',
   '#D6F591'
  ),
  secondary_mid = c(
   '#92ECF5',
   '#92B2F5',
   '#D993F5',
   '#F593B7',
   '#F5A293',
   '#F5C492',
   '#667545'
  ),
  secondary_dark = c(
   '#679EA3',
   '#3D5254',
   '#6779A3',
   '#4E3D54',
   '#A3677E',
   '#A36F67',
   '#A38667',
   '#3C533C'
  ),
  colslr30 = c(
    '#679EA3',
    '#F5A293',
    '#3D5254',
    '#51544A',
    '#6779A3',
    '#92ECF5',
    '#4E3D54',
    '#BEC5BC',
    '#A9C272',
    '#A3677E',
    '#D6F591',
    '#A36F67',
    '#9167A3',
    '#F5C4D7',
    '#ABC3F5',
    '#A38667',
    '#F5DDE6',
    '#F5ABC7',
    '#C4D4F5',
    '#F5C492',
    '#D993F5',
    '#3C533C',
    '#EEF7DB',
    '#E7C4F5',
    '#667545',
    '#ABEEF5',
    '#BEC5BC',
    '#263326',
    '#C4F0F5',
    '#D4D9D2',
    '#F5CCC4'
  ))

##########

#' A collection of palettes for SLR Consulting.
#'
#' Use the following colour palettes when producing data visualisations for SLR Consulting.
#'
#' @param n Number of colors desired.
#'   If omitted, all the colours are used.
#' @param name Name of palette. The choices are:
#'   \code{primary_green}, \code{shades},  \code{secondary_palette},
#'   \code{secondary_light}, \code{secondary_mid},  \code{secondary_dark}, \code{colslr_30}

#' @param type Either "continuous" or "discrete". Use continuous if you want
#'   to automatically interpolate between colours.
#'   @importFrom graphics rgb rect par image text
#' @return A vector of colours.
#' @export
#' @keywords colors
#' @examples
#' slr_palettes("primary_green")
#' slr_palettes("colslr30")
#' slr_palettes("secondary_palette")
#' slr_palettes("secondary_dark", 3)
#'
#' # If you need more colours than normally found in a palette, you
#' # can use a continuous palette to interpolate between existing
#' # colours
#' pal <- slr_palettes(21, name = "primary_green", type = "continuous")
#' image(volcano, col = pal)
#'
#' # Colour SLR
#'
#' This function takes a color palette as input and returns a plot of the palette.
#'
#' @param palette A vector of colors to use for the palette.
#' @export
#' @examples
#' colslr(primary_green)
colslr <- function(name, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)

  pal <- slr_palettes[[name]]
  if (type == "continuous" && name == "primary_green") {
    pal <- slr_palettes[["primary_greenContinuous"]]
  }

  if (is.null(pal))
    stop("Colour palette not found.")

  if (missing(n)) {
    n <- length(pal)
  }

  if (type == "discrete" && n > length(pal)) {
    stop("Sorry, you want more colours than what this palette can offer. Try using colslr30 which has 30 colours.")
  }

  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = name)
}

#' Print a color palette
#'
#' This function takes a color palette as input and returns a plot of the palette.
#'
#' @param x A vector of colors to use for the palette.
#' @export
#' @examples
#' print.palette(primary_green)
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}
#' heatmap
#'
#' A heatmap example
"heatmap"
