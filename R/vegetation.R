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
#' stand = initStand(npatch=2)
#' stand3D(stand, 1)
#' veg = data.frame(DBH=rep(0.4, 50))
#' veg$Height    = veg$DBH * 35
#' veg$Crownarea = veg$DBH * 5
#' veg$LeafType  = sample(1:2, nrow(veg), replace=TRUE)
#' veg$ShadeType = sample(1:2, nrow(veg), replace=TRUE)
#' stand@patches[[1]]@vegetation = establishPatch(veg, stand@hexagon@supp[['inner.radius']])
#' dummy = plant3D(stand, 1)
#'
#' stand3D(stand, 2)
#' veg = data.frame(DBH=rep(0.5, 100) * rgamma(100, 2.5, 9))
#' veg$Height    = veg$DBH * 35  * rbeta(nrow(veg),10,1)
#' veg$Crownarea = veg$DBH * 5 * rnorm(nrow(veg), 1, 0.1)
#' veg$LeafType  = sample(1:2, nrow(veg), replace=TRUE)
#' veg$ShadeType = sample(1:2, nrow(veg), replace=TRUE)
#' stand@patches[[2]]@vegetation = establishPatch(veg, stand@hexagon@supp[['inner.radius']])
#' dummy = plant3D(stand, 2)
plant3D <- function(stand=NULL, patch.id=NULL, crown.opacity=1) {
  if (is.null(patch.id))
    patch.id <- 1:length(stand@patches)

  ## How I choose the colors:
  ## library(RColorBrewer)
  ## bp = brewer.pal(9, 'YlGn')
  ## plot(rep(1,9), cex=5, pch=16, col=bp)
  ## bp[c(5,9)] ##  => c("#78C679", "#004529")
  ## from dark (tolerant) to light green (intolerant)
  shade.colors = colorRampPalette(c("#004529", "#78C679"))

  for (i in patch.id) {
    offset = stand@patch.pos[i, ]
    if (!any(names(stand@patches[[i]]@color.table) == "shade")) {
      n = length(unique(stand@patches[[i]]@vegetation$ShadeType))
      stand@patches[[i]]@color.table[['shade']] = shade.colors(n)
    }
    col=stand@patches[[i]]@color.table[['shade']]
    if (nrow(stand@patches[[i]]@vegetation) > 0) {
      for (j in 1:nrow(stand@patches[[i]]@vegetation)) {
        tree3D(stand@patches[[i]]@vegetation[j, ], offset, col, opacity=crown.opacity)
      }
    }
  }
  return(stand)
}

#' draw a single tree
#'
#' @param tree one column of the \code{\link{Patch-class}} vegetation data.frame slot
#' @param offset x/y center and surface (z) of the respective patch
#' @param col crown colors for the shade classes
#' @param opacity alpha value for the tree crown (heavy impacting performance)
#' @param faces number of faces/triangles used per stem and tree cone
#' @return None
#' @import rgl
#' @export
#' @author Joerg Steinkamp \email{steinkamp.joerg@@gmail.com}
tree3D <- function(tree=NULL, offset=c(0, 0, 0), col=c("#22BB22", "33FF33"), opacity=1, faces=29) {
  ##print(tree)
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
    triangles3d(cone@vertices[cone@id, ], col=col[tree$ShadeType], alpha=opacity)
  } else if (tree$LeafType==2) {
    shade3d(cylinder3d(matrix(c(tree$x + offset[1], tree$y + offset[2], offset[3],
                                tree$x + offset[1], tree$y + offset[2], 0.34 * tree$Height + offset[3]),
                              nrow=2, byrow=TRUE),
                       rep(tree$DBH/2, 2), sides=faces), col="#8B4513")
#    shade3d(ellipse3d(diag(3), centre=c(tree$x + offset[1],
#                                        tree$y + offset[2],
#                                        tree$Height * 0.67 + offset[3]),
    #                      scale=c(0.2*crownRadius, 0.2*crownRadius, 0.33*0.2*tree$Height)), col=col[tree$ShadeType], alpha=opacity)
    drawEllipsoid(rx=crownRadius, ry=crownRadius, rz=0.33*tree$Height,
                 ctr=c(tree$x + offset[1],
                       tree$y + offset[2],
                       tree$Height * 0.67 + offset[3]),
                 n=faces, col=col[tree$ShadeType], opacity=opacity)
  }
}
