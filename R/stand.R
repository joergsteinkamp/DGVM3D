#' Initialize the model Stand
#'
#' @details
#' If soil is a matrix, the number of columns must be equal to npatch. In that way each patch can have its own soil depth.
#' The patches represented as hexagons can either be arranged in a square or in a line. The later one for example to represent a time series (succession).
#' @param npatch number of patches
#' @param year the initialization year
#' @param soil a vector or matrix of soil depths.
#' @param z the height of each patch.
#' @param layout patch layout ('square' or 'linear'), a two element vector with number of rows/columns. A matrix for layout (not yet ready).
#' @param composition 'spatial' or 'temporal'
#' @param dist the fractional distance between the hexagons
#' @return a \code{\link{Stand-class}}
#' @importFrom methods new
#' @importFrom grDevices colorRampPalette
#' @export
#' @include classes.R
#' @examples
#' \dontrun{
#' stand <- initStand(npatch=9, z=sort(rnorm(9, sd=2)))
#' stand3D(stand)
#'
#' stand <- initStand(npatch=9, z=sort(rnorm(9, sd=2)), layout='linear')
#' stand3D(stand)
#' }
initStand <- function(npatch=1, year=2000, soil=c(0, -0.5, -1.5), z=0, layout="square", composition="spatial", dist=0.05) {
  if (is.matrix(soil)) {
    if (ncol(soil) != npatch)
      stop("'npatch' and 'ncol(soil)' differ!")
  } else {
    if (!is.vector(soil))
      stop("'soil' must either be a matrix or vector")
  }
  if (length(z) == 1) {
    z = rep(z, npatch)
  } else if (length(z) != npatch) {
    stop("'z' must be either of length 'npatch' or 1!")
  }

  hexagon <- getHexagon(area=dgvm3d.options("patch.area"), z=c(0, -1))

  if (typeof(layout) == "character") {
    if (layout == "square") {
      nxy.max = ceiling(sqrt(npatch))
      nxy.min = floor(sqrt(npatch))
      ## with 3 patches the above got the values 2 and 1, so 3 was never reached in the loop below
      if (npatch == 3)
        nxy.min=2
      layout = c(nxy.min, nxy.max)
    } else {
      nxy.min = npatch
      nxy.max = 1
      layout = c(nxy.min, nxy.max)
    }
  } else if (typeof(layout) == "integer" && is.vector(layout)) {
    nxy.min = layout[2]
    nxy.max = layout[1]
  } else if (typeof(layout) == "integer" && is.matrix(layout)) {
    nxy.min = nrow(layout)
    nxy.max = ncol(layout)
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
      id <- (i - 1) * nxy.min + j
      if (id > npatch)
        break
      if (composition == "temporal") {
        x = (2 + dist) * (i - 1) * hexagon@supp[['outer.radius']]
        y = (2 + dist) * (j - 1) * hexagon@supp[['inner.radius']]
      } else {
        x = (1.5 + dist) * (i - 1) * hexagon@supp[['outer.radius']]
        y = (2.0 + dist) * (j - 1) * hexagon@supp[['inner.radius']] + (i %% 2) * (1 + dist / 1.95) * hexagon@supp[['inner.radius']]
      }
      if (id == 1) {
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

  return(new("Stand",
             area=dgvm3d.options("patch.area"),
             year=year,
             hexagon=hexagon,
             layout=layout,
             composition=composition,
             patch.pos=t(patch.pos),
             patches=patches))
}

#' Remove/add trees with a new vegetation data.frame
#'
#' Removes those individuals with the shortest distance to any neighbor and adds new individuals randomly.
#'
#' @param stand stand to update
#' @param vegetation new vegetation data.frame
#' @param year the next year
#' @return stand
#' @export
updateStand <- function(stand, vegetation, year=NULL) {
  Year=PID=Crownarea=NULL
  if (is.null(year)) {
    old.year = stand@year
    new.years = sort(unique(vegetation$Year))
    if (all(new.years <= old.year))
      stop("No larger year in vegetation data.frame!")
    if (!is.null(year)) {
      if (all(new.years != year))
        stop(paste0("Given 'year' (", year, ") not in 'vegetation data.frame!"))
    } else {
      year = new.years[which(new.years > old.year)[1]]
    }
  }
  vegetation = subset(vegetation, Year == year)
  stand@year = year

  for ( i in 1:length(stand@patches)) {
    new.patch.veg = subset(vegetation, PID == stand@patches[[i]]@pid)
    if (nrow(new.patch.veg) > 0) {
      new.patch.veg$x = NA
      new.patch.veg$y = NA
      new.patch.veg$dnn = NA
      old.vid = unique(stand@patches[[i]]@vegetation$VID)
      if (length(old.vid) > 0) {
        ## removal of killed individuals
        for (j in 1:length(old.vid)) {
          remain = sum(new.patch.veg$VID == old.vid[j])
          old.trees = stand@patches[[i]]@vegetation[stand@patches[[i]]@vegetation$VID == old.vid[j], ]
          old.trees = old.trees[with(old.trees, order(-dnn)), ]
          new.patch.veg[new.patch.veg$VID == old.vid[j], "x"] = old.trees$x[1:remain]
          new.patch.veg[new.patch.veg$VID == old.vid[j], "y"] = old.trees$y[1:remain]
        }
      }
      ## f_js_20180224 DEBUG one grass has coord values
      stand@patches[[i]]@vegetation = subset(new.patch.veg, Crownarea < 0 | !is.finite(Crownarea))
      stand@patches[[i]]@vegetation = rbind(stand@patches[[i]]@vegetation,
                                            establishTrees(subset(new.patch.veg, Crownarea > 0 & is.finite(Crownarea)), stand@hexagon@supp$inner.radius))
    } else {
      stand@patches[[i]]@vegetation = data.frame()
    }
  }
  return(stand)
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
#' @seealso \code{\link{initStand}} for examples
stand3D <- function(stand, patch.id=NULL) {
  if (is.null(patch.id))
    patch.id <- 1:length(stand@patches)

  for (i in patch.id) {
    patch.hex = stand@hexagon@vertices
    offset = matrix(stand@patch.pos[i, ], nrow(patch.hex), 3, byrow=TRUE)
    patch.hex[,1:2] = patch.hex[,1:2] + offset[,1:2]
    for (j in 1:(length(stand@patches[[i]]@soil) - 1)) {
      patch.hex[1:6,  3] = stand@patches[[i]]@soil[j]
      patch.hex[7:12, 3] = stand@patches[[i]]@soil[j + 1]
      patch.hex[,3] = patch.hex[,3] + offset[,3]
      triangles3d(patch.hex[stand@hexagon@id, ], col=stand@patches[[i]]@color.table[['soil']][j])
    }
  }
}
