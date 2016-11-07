#' Initialize the model Stand
#'
#' @details
#' If soil is a matrix, the number of columns must be equal to npatch. In that way each patch can have its own soil depth.
#' The patches represented as hexagons can either be arranged in a square or in a line. The later one for example to represent a time series (succession).
#' @param area patch size area
#' @param npatch number of patches
#' @param soil a vector or matrix of soil depths.
#' @param z the height of each patch.
#' @param arrangement patch arrangement ('square' or 'linear'), a two element vector with number of rows/colums. A matrix for layout (not yet ready).
#' @param composition 'spatial' or 'temporal'
#' @param dist the fractional distance between the hexagons
#' @return a \code{\link{Stand-class}}
#' @importFrom methods new
#' @importFrom grDevices colorRampPalette
#' @export
#' @include classes.R
#' @author Joerg Steinkamp \email{steinkamp.joerg@@gmail.com}
#' @examples
#' stand <- initStand(npatch=9, z=sort(rnorm(9, sd=2)))
#' stand3D(stand)
#'
#' stand <- initStand(npatch=9, z=sort(rnorm(9, sd=2)), arrangement='linear')
#' stand3D(stand)
initStand <- function(area=1000, npatch=1, soil=c(0, -0.5, -1.5), z=0, arrangement="square", composition="spatial", dist=0.05) {
  if (is.matrix(soil)) {
    if (ncol(soil) != npatch)
      stop("'npatch' and 'ncol(soil)' differ!")
  } else {
    if (!is.vector(soil))
      stop("'soil' must either be a matrix or vector")
  }
  if (length(z)==1) {
    z = rep(z, npatch)
  } else if (length(z) != npatch) {
    stop("'z' must be either of length 'npatch' or 1!")
  }

  hexagon <- getHexagon(area=area, z=c(0, -1))
  if (typeof(arrangement) == "character") {
    if (arrangement=="square") {
      nxy.max = ceiling(sqrt(npatch))
      nxy.min = floor(sqrt(npatch))
    } else {
      nxy.min = npatch
      nxy.max = 1
    }
  } else if (typeof(arrangement) == "numeric" && is.vector(arrangement)) {
    nxy.min = arrangement[2]
    nxy.max = arrangement[1]
  } else if (typeof(arrangement) == "numeric" && is.matrix(arrangement)) {
    nxy.min = nrow(arrangement)
    nxy.max = ncol(arrangement)
    stop("Not yet ready!")
  }

  ## How I choose the colors:
  ## library(RColorBrewer)
  ## bp = brewer.pal(9, 'YlOrBr')
  ## plot(rep(1,9), cex=5, pch=16, col=bp)
  ## bp[c(4,8)] ##  => c("#FEC44F", "#993404")
  soil.colors = colorRampPalette(c("#993404", "#FEC44F"))

  patches=list()
  for (i in 1:nxy.max) {
    for (j in 1:nxy.min) {
      id <- (i-1) * nxy.min + j
      if (id > npatch)
        break
      x = (1.5 + dist) * (i-1) * hexagon@supp[['outer.radius']]
      y = (2.0 + dist) * (j-1) * hexagon@supp[['inner.radius']] + (i %% 2) * (1+ dist/1.95) * hexagon@supp[['inner.radius']]
      if (id==1) {
        patch.pos <- matrix(c(x, y, z[1]), 3, 1)
        row.names(patch.pos) <- c("x", "y", "z")
      } else {
        patch.pos = cbind(patch.pos, c(x, y, z[id]))
      }
      if (is.matrix(soil)) {
        coltab <- list()
        coltab[['soil']] = soil.colors(length(soil[, id])-1)
        patches[[id]] = new("Patch", id=id, soil=soil[, id], color.table=coltab)
      } else {
        coltab <- list()
        coltab[['soil']] = soil.colors(length(soil)-1)
        patches[[id]] = new("Patch", id=id, soil=soil, color.table=coltab)
      }
    }
  }
  return(new("Stand", area=area, hexagon=hexagon, arrangement=arrangement, composition=composition, patch.pos=t(patch.pos), patches=patches))
}

#' 3D view of the stands
#'
#' Uses \code{\link{rgl}} to visualize a single, if patch.id is given, or all patch soil hexagons
#'
#' @param stand the \code{\link{Stand-class}} to visualize
#' @param patch.id the patch IDs to create. Default: all.
#' @return None
#' @export
#' @import rgl
#' @include classes.R
#' @author Joerg Steinkamp \email{steinkamp.joerg@@gmail.com}
#' @seealso \code{\link{initStand}} for examples
stand3D <- function(stand, patch.id=NULL) {
  if (is.null(patch.id))
    patch.id <- 1:length(stand@patches)

  for (i in patch.id) {
    patch.hex = stand@hexagon@vertices
    offset = matrix(stand@patch.pos[i, ], nrow(patch.hex), 3, byrow=TRUE)
    patch.hex[,1:2] = patch.hex[,1:2] + offset[,1:2]
    for (j in 1:(length(stand@patches[[i]]@soil)-1)) {
      patch.hex[1:6,  3] = stand@patches[[i]]@soil[j]
      patch.hex[7:12, 3] = stand@patches[[i]]@soil[j+1]
      patch.hex[,3] = patch.hex[,3] + offset[,3]
      triangles3d(patch.hex[stand@hexagon@id, ], col=stand@patches[[i]]@color.table[['soil']][j])
    }
  }
}
