#' poplulate a patch with its vegetation
#'
#' Randomly 'plant' the trees in the patch within a given radius.
#'
#' @param vegetation the vegetation data.frame
#' @param radius the radius used to distribute the vegetation to
#' @param jitter add a small amount of noise to the positions. Applies only for dgvm3d.options("establish.method") = "row" or "sunflower" (default: FALSE).
#' @param ... additiontal parameters passed to jitter.
#' @return the vegetation data.frame with the positions
#' @include classes.R
#' @importFrom stats runif rbeta
#' @export
#' @examples
#' \dontrun{
#' dgvm3d.options("default")
#' stand = initStand(npatch=1)
#' veg = data.frame(DBH=rep(0.5, 100))
#' veg$Height    = veg$DBH * 35
#' veg$Crownarea = veg$DBH * 10
#' veg$LeafType  = sample(1:2, nrow(veg), replace=TRUE)
#' veg$ShadeType = sample(1:2, nrow(veg), replace=TRUE)
#' stand@patches[[1]]@vegetation = establishTrees(veg, stand@hexagon@supp[['inner.radius']])
#' stand3D(stand)
#' dummy = plant3D(stand)
#' rot.z = rotationMatrix(0, 0, 0, 1)
#' rot.y = rotationMatrix(0, 1, 0, 0)
#' rgl.viewpoint(userMatrix = rot.y %*% rot.z, fov=1)
#'
#' rgl.clear()
#' dgvm3d.options(establish.method = "sunflower")
#' stand@patches[[1]]@vegetation = establishTrees(veg, stand@hexagon@supp[['inner.radius']])
#' stand3D(stand)
#' dummy = plant3D(stand)
#'
#' rgl.clear()
#' dgvm3d.options(establish.method = "row")
#' stand@patches[[1]]@vegetation = establishTrees(veg, stand@hexagon@supp[['inner.radius']],
#'                                                jitter=TRUE, amount=0.01)
#' stand3D(stand)
#' dummy = plant3D(stand)
#' }
establishTrees <- function(vegetation=NULL, radius=1, jitter=FALSE, ...) {
  if (is.null(vegetation))
    stop("'vegetation' data.frame is missing!")

  samples          <- dgvm3d.options("samples")
  overlap          <- dgvm3d.options("overlap")
  sort.column      <- dgvm3d.options("sort.column")
  establish.method <- dgvm3d.options("establish.method")

  if (dgvm3d.options("verbose") && dgvm3d.options("establish.method") == "random") {
    message("### establishTrees ###")
    message(sprintf("Using %i samples in max. %i repetitions (max. crown radius overlap: %0.3f).", samples[1], samples[2], overlap))
    message(paste0("Sorting by '", sort.column[1], "' in '", sort.column[2], "' order."))
  } else if (dgvm3d.options("verbose")) {
    message("### establishTrees ###")
    message(paste0 ("Using method: '", establish.method, "'."))
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
  if (nrow(vegetation) == 0)
    return(vegetation)

  ## check for presence of position columns
  ## and establishment method
  established = FALSE
  if (!all(c("x", "y") %in% colnames(vegetation))) {
    if (dgvm3d.options("verbose"))
      message("New establishment.")
    if (establish.method == "sunflower") {
      tree.ids <- which(vegetation$Crownarea > 0 & is.finite(vegetation$Crownarea))
      positions <- sunflower.disc(length(tree.ids))
      if (jitter) {
        positions$x = jitter(positions$x, ...)
        positions$y = jitter(positions$y, ...)
      }
      vegetation$x[vegetation$Crownarea > 0 & is.finite(vegetation$Crownarea)] = positions$x * radius
      vegetation$y[vegetation$Crownarea > 0 & is.finite(vegetation$Crownarea)] = positions$y * radius
      established = TRUE
    } else if (establish.method == "row") {
      tree.ids <- which(vegetation$Crownarea > 0 & is.finite(vegetation$Crownarea))
      positions <- row.disc(length(tree.ids))
      if (jitter) {
        positions$x = jitter(positions$x, ...)
        positions$y = jitter(positions$y, ...)
      }
      vegetation$x[vegetation$Crownarea > 0 & is.finite(vegetation$Crownarea)] = positions$x * radius
      vegetation$y[vegetation$Crownarea > 0 & is.finite(vegetation$Crownarea)] = positions$y * radius
      established = TRUE

    } else {
      vegetation$x = NA
      vegetation$y = NA
    }
  } else if (dgvm3d.options("verbose")) {
    message("Establishing additional trees.")
  }

  if (!established && establish.method != "random") {
    warning("If there are already trees with positions, only 'random' establishment works!")
  }

  ## first tree (excluding grasses)
  if (all(is.na(vegetation$x)) || all(is.na(vegetation$x))) {
    phi <- runif(1) * 2 * pi
    r   <- runif(1) * radius
    vegetation$x[1] = sin(phi) * r
    vegetation$y[1] = cos(phi) * r
  }

  ## any other tree
  ## Filter grasses by negative Crownarea, should better be done by lifeform
  for (i in which((is.na(vegetation$x) | is.na(vegetation$y)) & vegetation$Crownarea > 0 & is.finite(vegetation$Crownarea))) {
    trees.with.xy = which(!is.na(vegetation$x) & !is.na(vegetation$y) & vegetation$Crownarea > 0 & is.finite(vegetation$Crownarea))
    nwhile = 0
    dist <- matrix(NA, i - 1, samples[1])
    while(all(is.na(dist))) {
      if (nwhile > samples[2])
        stop("Could not find suitable position for next tree. Adjust dgvm3d.options 'overlap' and 'samples'!")
      phi <- runif(samples[1]) * 2 * pi
      ## slightly biased towards the center, since max distance below tends to place less points in the center.
      ## Need to find the optimum values (depends also in number of samples)
      new.pos = random.disc(samples[1]) * radius
      #r   <- rbeta(samples[1], est.beta.param[1], est.beta.param[2]) * radius
      #new.x <- sin(phi) * r
      #new.y <- cos(phi) * r

      ## distance to all other trees
      dist <- matrix(NA, length(trees.with.xy), samples[1])
      for (j in 1:samples[1])
        dist[,j] <- sqrt((new.pos$x[j] - vegetation$x[trees.with.xy])^2 + (new.pos$y[j] -vegetation$y[trees.with.xy])^2)

      ## TODO: NaNs are produced for grasses, since Crownarea = -1
      ## 20180209: should be fixed now
      crown.radius <- sqrt(vegetation$Crownarea[trees.with.xy] / pi)
      min.dist <- (crown.radius + sqrt(vegetation$Crownarea[i] / pi)) * (1 - overlap)
      min.dist = matrix(rep(min.dist, samples[1]), length(min.dist), samples[1])

      dist[dist < min.dist] = NA

      ## choose the nearest tree for each location and apply the desired 'method'
      ## on those
      dist = apply(dist, 2, min)
      if (!all(is.na(dist))) {
        k = sample(which(is.finite(dist)), 1)

        vegetation$x[i] = new.pos$x[k]
        vegetation$y[i] = new.pos$y[k]
      }
      nwhile = nwhile + 1
    } ## while
  }

  ## distance to nearest neighbour
  vegetation$dnn = sapply(1:nrow(vegetation), function(x) {
    min(sqrt((vegetation$x[x] - vegetation$x[-x])^2 + (vegetation$y[x] - vegetation$y[-x])^2))
  })
  return(vegetation)
}


#' Plant the trees of an already created patch/stand
#'
#' @param stand the stand for plantation
#' @param patch.id one or several specific patches only
#' @param crown.opacity alpha value for the green tree crowns. Setting it to something different than 1 slows down the rendering substatially!
#' @return the updated stand
#' @export
#' @import rgl
#' @importFrom utils tail
#' @examples
#' \dontrun{
#' stand = initStand(npatch=2)
#' stand3D(stand, 1)
#' veg = data.frame(DBH=rep(0.4, 50))
#' veg$Height    = veg$DBH * 35
#' veg$Crownarea = veg$DBH * 5
#' veg$LeafType  = sample(1:2, nrow(veg), replace=TRUE)
#' veg$ShadeType = sample(1:2, nrow(veg), replace=TRUE)
#' stand@patches[[1]]@vegetation = establishTrees(veg, stand@hexagon@supp[['inner.radius']])
#' dummy = plant3D(stand, 1)
#'
#' stand3D(stand, 2)
#' veg = data.frame(DBH=rep(0.5, 100) * rgamma(100, 2.5, 9))
#' veg$Height    = veg$DBH * 35  * rbeta(nrow(veg),10,1)
#' veg$Crownarea = veg$DBH * 5 * rnorm(nrow(veg), 1, 0.1)
#' veg$LeafType  = sample(1:2, nrow(veg), replace=TRUE)
#' veg$ShadeType = sample(1:2, nrow(veg), replace=TRUE)
#' stand@patches[[2]]@vegetation = establishTrees(veg, stand@hexagon@supp[['inner.radius']])
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

  ## determin the number of colors
  ncol = length(unique(as.vector(unlist(sapply(stand@patches, function(x) {
    if (nrow(x@vegetation) > 0) {
    y = x@vegetation[, dgvm3d.options("color.column")]
    return(unique(y))
    } else {
      return(NULL)
    }
    })))))

  for (i in patch.id) {
    ## only in those patches with vegetation
    if (nrow(stand@patches[[i]]@vegetation) > 0) {
      offset = stand@patch.pos[i, ]
      ## chose the canopy color
      if (!any(names(stand@patches[[i]]@color.table) == "crown")) {
        ##n = eval(parse(text=paste0("length(unique(stand@patches[[i]]@vegetation$", color.column, "))")))
        stand@patches[[i]]@color.table[['crown']] = crown.colors(ncol)
      }
      col = stand@patches[[i]]@color.table[['crown']]
      ##if (length(col) != eval(parse(text=paste0("length(unique(stand@patches[[i]]@vegetation$", color.column, "))")))) {
      ##  col = eval(parse(text=paste0("crown.colors(length(unique(stand@patches[[i]]@vegetation$", color.column,")))")))
      ##  stand@patches[[i]]@color.table[['crown']] = col
      ##}
      for (j in 1:nrow(stand@patches[[i]]@vegetation)) {
        tree3D(stand@patches[[i]]@vegetation[j, ], offset, col, opacity=crown.opacity)
      }
      grass3D(stand@patches[[i]]@vegetation, stand@hexagon, offset=matrix(stand@patch.pos[i, ], nrow(stand@hexagon@vertices), 3, byrow=TRUE), col=tail(stand@patches[[1]]@color.table$crown, 1))
    }
  }
  return(stand)
}

#' Plant the grass on the patch
#'
#' @param grass vegetation data.frame
#' @param kind so far only a hexagon is allowed (TriangBody)
#' @param offset the patch offset
#' @param col the color to use for grass
#' @param opacity.threshold no grass is drawn below the lower values of LAI and full opacity is used above the upper value
#' @param height.scale scale the LAI by this factor as height for the hexagon.
#'
#' @importFrom rgl triangles3d
#' @return NULL
## @export
#'
grass3D <- function(grass=NULL, kind=NULL, offset=c(0, 0, 0), col="green", opacity.threshold=c(0.2, 2), height.scale=0.1) {
  ## TODO: somehow distinguish C3 and C4 grass
  ## should be done before calling this function
  grass = grass[grass$Crownarea <= 0 | !is.finite(grass$Crownarea), ]
  if (nrow(grass) > 0) {
    if (sum(grass$LAI > min(opacity.threshold))) {
      if (class(kind) == "TriangBody") {
        patch.hex = kind@vertices
        patch.hex[,1:2] = patch.hex[,1:2] + offset[,1:2]
        patch.hex[7:12, 3] = -patch.hex[7:12, 3] * sum(grass$LAI * height.scale)
        patch.hex[,3] = patch.hex[,3] + offset[,3]
        triangles3d(patch.hex[kind@id, ], col=col, opacity=min(1, sum(grass$LAI) / max(opacity.threshold)))
      } else {
        warning("This grass 'kind' is not yet implemented!")
      }
    }
  }
  return(invisible(NULL))
}

#' draw a single tree
#'
#' @param tree one column of the \code{\link{Patch-class}} vegetation data.frame slot
#' @param offset x/y center and surface (z) of the respective patch
#' @param col crown colors for the shade classes
#' @param opacity alpha value for the tree crown (heavy impacting performance)
#' @param faces number of faces/triangles used per stem and tree cone 3-times for ellipsoid.
#' @return NULL
#' @importFrom rgl triangles3d cylinder3d
## @export
tree3D <- function(tree=NULL, offset=c(0, 0, 0), col=c("#22BB22", "33FF33"), opacity=1, faces=19) {
  if (tree$Crownarea <= 0 || !is.finite(tree$Crownarea))
    return(NULL)
  color.column = dgvm3d.options("color.column")
  crownRadius = sqrt(tree$Crownarea / pi)
  if (tree$LeafType == 1) {
    if (is.null(tree$BoleHeight)) {
      tree$BoleHeight = 0.25 * tree$Height
    }
    shade3d(cylinder3d(matrix(c(tree$x + offset[1], tree$y + offset[2], offset[3],
                                tree$x + offset[1], tree$y + offset[2], tree$BoleHeight + offset[3]),
                              nrow=2, byrow=TRUE),
                       rep(tree$DBH/2, 2), sides=faces), col="#8B4513")
    cone = getCone(radius = crownRadius, height = tree$Height - tree$BoleHeight, faces=faces, close=TRUE)
    cone@vertices[,1] = cone@vertices[,1] + tree$x + offset[1]
    cone@vertices[,2] = cone@vertices[,2] + tree$y + offset[2]
    cone@vertices[,3] = cone@vertices[,3] + tree$BoleHeight + offset[3]
    triangles3d(cone@vertices[cone@id, ], col=col[eval(parse(text=paste0("tree$",color.column)))], alpha=opacity)
  } else if (tree$LeafType == 2) {
    if (is.null(tree$BoleHeight)) {
      tree$BoleHeight = 0.3333 * tree$Height
    }
    shade3d(cylinder3d(matrix(c(tree$x + offset[1], tree$y + offset[2], offset[3],
                                tree$x + offset[1], tree$y + offset[2], 1.05 * tree$BoleHeight + offset[3]),
                              nrow=2, byrow=TRUE),
                       rep(tree$DBH/2, 2), sides=faces), col="#8B4513")
    ellipsoid = getEllipsoid(radius=crownRadius, height=tree$Height - tree$BoleHeight, faces=3*faces)
    ellipsoid@vertices[,1] = ellipsoid@vertices[,1] + tree$x + offset[1]
    ellipsoid@vertices[,2] = ellipsoid@vertices[,2] + tree$y + offset[2]
    ellipsoid@vertices[,3] = ellipsoid@vertices[,3] + tree$BoleHeight + offset[3]
    triangles3d(ellipsoid@vertices[ellipsoid@id, ], col=col[eval(parse(text=paste0("tree$",color.column)))], alpha=opacity)
  }
  return(invisible(NULL))
}
