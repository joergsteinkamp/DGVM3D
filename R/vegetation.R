#' poplulate a patch with its vegetation
#'
#' Randomly 'plant' the trees in the patch within a given radius.
#'
#' @param vegetation the vegetation data.frame
#' @param radius the radius used to distribute the vegetation to
#' @return the vegetation data.frame with the positions
#' @include classes.R
#' @importFrom stats runif rbeta
#' @export
#' @author Joerg Steinkamp \email{steinkamp.joerg@@gmail.com}
#' @examples
#' \dontrun{
#' dgvm3d.options("default")
#' stand = initStand(npatch=1)
#' veg = data.frame(DBH=rep(0.5, 100))
#' veg$Height    = veg$DBH * 35
#' veg$Crownarea = veg$DBH * 10
#' veg$LeafType  = sample(0:1, nrow(veg), replace=TRUE)
#' veg$ShadeType = sample(0:1, nrow(veg), replace=TRUE)
#' stand@patches[[1]]@vegetation = establishVegetation(veg, stand@hexagon@supp[['inner.radius']])
#' }
establishVegetation <- function(vegetation=NULL, radius=1) {
  if (is.null(vegetation))
    stop("'vegetation' data.frame is missing!")

  samples          <- dgvm3d.options("samples")
  overlap          <- dgvm3d.options("overlap")
  sort.column      <- dgvm3d.options("sort.column")
  establish.method <- dgvm3d.options("establish.method")
  est.beta.param   <- dgvm3d.options("establish.beta.parameters")

  if (dgvm3d.options("verbose")) {
    message("### establishVegetation ###")
    message(sprintf("Using %i samples in max. %i repetitions (max. crown radius overlap: %0.3f).", samples[1], samples[2], overlap))
    message(paste0("Sorting by '", sort.column[1], "' in '", sort.column[2], "' order."))
  }

  if (any(colnames(vegetation) == sort.column[1])) {
    if (sort.column[2] == "ascending") {
      vegetation = eval(parse(text=paste0("vegetation[with(vegetation, order(", sort.column[1],")), ]")))
    } else {
      vegetation = eval(parse(text=paste0("vegetation[with(vegetation, order(-", sort.column[1],")), ]")))
    }
  } else {
    warning(paste0("Column '", sort.column[1], "' does not exist. No sorting perfomed."))
    message(paste0("Column '", sort.column[1], "' does not exist. No sorting perfomed."))
  }

  ## if vegetation table is empty, e.g. after disturbance
  if (nrow(vegetation)==0)
    return(vegetation)

  ## check for present position columns
  if (!all(c("x","y") %in% colnames(vegetation))) {
    if (dgvm3d.options("verbose"))
      message("New establishment.")
    vegetation$x = NA
    vegetation$y = NA
  } else if (dgvm3d.options("verbose")) {
    message("Establishing new trees.")
  }

  ## first tree
  if (all(is.na(vegetation$x)) || all(is.na(vegetation$x))) {
    phi <- runif(1) * 2 * pi
    r   <- runif(1) * radius
    vegetation$x[1] = sin(phi) * r
    vegetation$y[1] = cos(phi) * r
  }

  ## any other tree
  for (i in which(is.na(vegetation$x) | is.na(vegetation$y))) {
    trees.with.xy = which(!is.na(vegetation$x) & !is.na(vegetation$y))
    nwhile = 0
    dist <- matrix(NA, i-1, samples[1])
    while(all(is.na(dist))) {
      if (nwhile > samples[2])
        stop("Could not find suitable position for next tree. Adjust dgvm3d.options 'overlap' and 'samples'!")
      phi <- runif(samples[1]) * 2 * pi
      ## slightly biased towards the center, since max distance below tends to place less points in the center.
      ## Need to find the optimum values (depends also in number of samples)
      r   <- rbeta(samples[1], est.beta.param[1], est.beta.param[2]) * radius
      new.x <- sin(phi) * r
      new.y <- cos(phi) * r

      ## distance to all other trees
      dist <- matrix(NA, length(trees.with.xy), samples[1])
      for (j in 1:samples[1])
        dist[,j] <- sqrt((new.x[j] - vegetation$x[trees.with.xy])^2 + (new.y[j] -vegetation$y[trees.with.xy])^2)

      crown.radius <- sqrt(vegetation$Crownarea[trees.with.xy]/pi)
      min.dist <- (crown.radius + sqrt(vegetation$Crownarea[i]/pi)) * (1 - overlap)
      min.dist = matrix(rep(min.dist, samples[1]), length(min.dist), samples[1])

      dist[dist < min.dist] = NA

      ## choose the nearest tree for each location and apply the desired 'method'
      ## on those
      dist = apply(dist, 2, min)
      if (!all(is.na(dist))) {
        if (establish.method=="max") {
          k = which.max(dist)
        } else if (establish.method=="min") {
          k = which.min(dist)
        } else if (establish.method=="random") {
          k = sample(which(is.finite(dist)), 1)
        } else {
          stop(paste0("'establish.method' '", establish.method, "' not known!"))
        }
        vegetation$x[i] = new.x[k]
        vegetation$y[i] = new.y[k]
      }
      nwhile = nwhile + 1
    } ## while
  }
  ## distance to nearest neighbour
  vegetation$dnn = sapply(1:nrow(vegetation), function(x) {
    min(sqrt((vegetation$x[x]-vegetation$x[-x])^2 + (vegetation$y[x] - vegetation$y[-x])^2))
  })
  return(vegetation)
}


#' Plant the trees of an already created patch/stand
#'
#' @param stand the stand for plantation
#' @param patch.id one or several specific patches only
#' @param crown.opacity alpha value for the green tree crowns. Setting it to something different than 1 slows down the rendering substatially!
#' @return the updated stand
#' @author Joerg Steinkamp \email{steinkamp.joerg@@gmail.com}
#' @export
#' @import rgl
#' @examples
#' \dontrun{
#' stand = initStand(npatch=2)
#' stand3D(stand, 1)
#' veg = data.frame(DBH=rep(0.4, 50))
#' veg$Height    = veg$DBH * 35
#' veg$Crownarea = veg$DBH * 5
#' veg$LeafType  = sample(1:2, nrow(veg), replace=TRUE)
#' veg$ShadeType = sample(1:2, nrow(veg), replace=TRUE)
#' stand@patches[[1]]@vegetation = establishVegetation(veg, stand@hexagon@supp[['inner.radius']])
#' dummy = plant3D(stand, 1)
#'
#' stand3D(stand, 2)
#' veg = data.frame(DBH=rep(0.5, 100) * rgamma(100, 2.5, 9))
#' veg$Height    = veg$DBH * 35  * rbeta(nrow(veg),10,1)
#' veg$Crownarea = veg$DBH * 5 * rnorm(nrow(veg), 1, 0.1)
#' veg$LeafType  = sample(1:2, nrow(veg), replace=TRUE)
#' veg$ShadeType = sample(1:2, nrow(veg), replace=TRUE)
#' stand@patches[[2]]@vegetation = establishVegetation(veg, stand@hexagon@supp[['inner.radius']])
#' dummy = plant3D(stand, 2)
#' }
plant3D <- function(stand=NULL, patch.id=NULL, crown.opacity=1) {
  if (is.null(patch.id))
    patch.id <- 1:length(stand@patches)

  ## How I choose the colors:
  ## library(RColorBrewer)
  ## bp = brewer.pal(9, 'YlGn')
  ## plot(rep(1,9), cex=5, pch=16, col=bp)
  ## bp[c(5,9)] ##  => c("#78C679", "#004529")
  ## from dark (tolerant) to light green (intolerant)
  crown.colors = colorRampPalette(c("#004529", "#78C679"))
  color.column = dgvm3d.options("color.column")

  for (i in patch.id) {
    ## only in those patches with vegetation
    if (nrow(stand@patches[[i]]@vegetation) > 0) {
      offset = stand@patch.pos[i, ]
      ## chose the canopy color
      if (!any(names(stand@patches[[i]]@color.table) == "crown")) {
        n = eval(parse(text=paste0("length(unique(stand@patches[[i]]@vegetation$", color.column, "))")))
        stand@patches[[i]]@color.table[['crown']] = crown.colors(n)
      }
      col = stand@patches[[i]]@color.table[['crown']]
      if (length(col) != eval(parse(text=paste0("length(unique(stand@patches[[i]]@vegetation$", color.column, "))")))) {
        col = eval(parse(text=paste0("crown.colors(length(unique(stand@patches[[i]]@vegetation$", color.column,")))")))
        stand@patches[[i]]@color.table[['crown']] = col
      }
      for (j in 1:nrow(stand@patches[[i]]@vegetation)) {
        tree3D(stand@patches[[i]]@vegetation[j, ], offset, col, opacity=crown.opacity)
      }
    }
  }
  return(stand)
}

## draw a single tree
##
## @param tree one column of the \code{\link{Patch-class}} vegetation data.frame slot
## @param offset x/y center and surface (z) of the respective patch
## @param col crown colors for the shade classes
## @param opacity alpha value for the tree crown (heavy impacting performance)
## @param faces number of faces/triangles used per stem and tree cone 3-times for ellipsoid.
## @return None
## @import rgl
## @export
## @author Joerg Steinkamp \email{steinkamp.joerg@@gmail.com}
tree3D <- function(tree=NULL, offset=c(0, 0, 0), col=c("#22BB22", "33FF33"), opacity=1, faces=19) {
  color.column = dgvm3d.options("color.column")
  crownRadius = sqrt(tree$Crownarea/pi)
  if (tree$LeafType==1) {
    shade3d(cylinder3d(matrix(c(tree$x + offset[1], tree$y + offset[2], offset[3],
                                tree$x + offset[1], tree$y + offset[2], 0.25 * tree$Height + offset[3]),
                              nrow=2, byrow=TRUE),
                       rep(tree$DBH/2, 2), sides=faces), col="#8B4513")
    cone = getCone(radius = crownRadius, height = 0.75 * tree$Height, faces=faces, close=TRUE)
    cone@vertices[,1] = cone@vertices[,1] + tree$x + offset[1]
    cone@vertices[,2] = cone@vertices[,2] + tree$y + offset[2]
    cone@vertices[,3] = cone@vertices[,3] + 0.25 * tree$Height + offset[3]
    triangles3d(cone@vertices[cone@id, ], col=col[eval(parse(text=paste0("tree$",color.column)))], alpha=opacity)
  } else if (tree$LeafType==2) {
    shade3d(cylinder3d(matrix(c(tree$x + offset[1], tree$y + offset[2], offset[3],
                                tree$x + offset[1], tree$y + offset[2], 0.34 * tree$Height + offset[3]),
                              nrow=2, byrow=TRUE),
                       rep(tree$DBH/2, 2), sides=faces), col="#8B4513")
    ellipsoid = getEllipsoid(radius=crownRadius, height=0.66*tree$Height, faces=3*faces)
    ellipsoid@vertices[,1] = ellipsoid@vertices[,1] + tree$x + offset[1]
    ellipsoid@vertices[,2] = ellipsoid@vertices[,2] + tree$y + offset[2]
    ellipsoid@vertices[,3] = ellipsoid@vertices[,3] + 0.25 * tree$Height + offset[3]
    triangles3d(ellipsoid@vertices[ellipsoid@id, ], col=col[eval(parse(text=paste0("tree$",color.column)))], alpha=opacity)
  }
}
