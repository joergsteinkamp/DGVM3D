#' Initialize the model Stand
#'
#' @details
#' If soil is a matrix, the number of columns must be equal to npatch. In that way each patch can have its own soil depth.
#' The patches represented as hexagons can either be arranged in a square or in a line. The later one for example to represent a time series (succession).
#' @param area patch size area
#' @param npatch number of patches
#' @param soil a vector or matrix of soil depths.
#' @param arrangement patch arrangement ('square' or 'linear').
#' @param composition 'spatial' or 'temporal'
#' @param dist the fractional distance between the hexagons
#' @return a \code{\link[DGVM3D]{Stand-class}}
#' @export
#' @import RColorBrewer
#' @include classes.R
#' @author Joerg Steinkamp \email{steinkamp.joerg@@gmail.com}
#' @examples
#' stand <- initStand(npatch=9, z=rnorm(9))
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
  if (arrangement=="square") {
    nxy.max = ceiling(sqrt(npatch))
    nxy.min = floor(sqrt(npatch))
  } else {
    nxy.max = 1
    nxy.min = npatch
  }

  patches=list()
  for (i in 1:nxy.max) {
    for (j in 1:nxy.min) {
      id <- (i-1) * nxy.min + j
      if (id > npatch)
        break
      x = (1.5 + dist) * (i-1) * hexagon@supp[['outer.radius']]
      y = (2.0 + dist) * (j-1) * hexagon@supp[['inner.radius']] + (i %% 2) * (1+ dist/1.95) * hexagon@supp[['inner.radius']]
      if (id==1) {
        patch.pos <- matrix(c(x,y, z[1]), 3, 1)
      } else {
        patch.pos = cbind(patch.pos, c(x, y, z[id]))
      }
      if (is.matrix(soil)) {
        coltab <- list()
        coltab[['soil']] = brewer.pal(length(soil[, id]), 'YlOrBr')[2:length(soil[, id])]
        patches[[id]] = new("Patch", id=id, soil=soil[, id], color.table=coltab)
      } else {
        coltab <- list()
        coltab[['soil']] = brewer.pal(length(soil), 'YlOrBr')[2:length(soil)]
        patches[[id]] = new("Patch", id=id, soil=soil, color.table=coltab)
      }
    }
  }


  return(new("Stand", area=area, hexagon=hexagon, arrangement=arrangement, composition=composition, patch.pos=t(patch.pos), patches=patches))
}

#' 3D view of the stands
#'
#' @param stand the \code{\link[DGVM3D]{Stand-class}} to visualize
#' @export
#' @import rgl
#' @include classes.R
#' @author Joerg Steinkamp \email{steinkamp.joerg@@gmail.com}
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