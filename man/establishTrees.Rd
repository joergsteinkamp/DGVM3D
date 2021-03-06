% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vegetation.R
\name{establishTrees}
\alias{establishTrees}
\title{populate a patch with its vegetation}
\usage{
establishTrees(vegetation = NULL, radius = 1, jitter = FALSE, ...)
}
\arguments{
\item{vegetation}{the vegetation data.frame}

\item{radius}{the radius used to distribute the vegetation to}

\item{jitter}{add a small amount of noise to the positions. Applies only for dgvm3d.options("establish.method") = "row" or "sunflower" (default: FALSE).}

\item{...}{additional parameters passed to jitter.}
}
\value{
the vegetation data.frame with the positions
}
\description{
Randomly 'plant' the trees in the patch within a given radius.
}
\examples{
\dontrun{
dgvm3d.options("default")
stand = initStand(npatch=1)
veg = data.frame(DBH=rep(0.5, 100))
veg$Height    = veg$DBH * 35
veg$Crownarea = veg$DBH * 10
veg$LeafType  = sample(1:2, nrow(veg), replace=TRUE)
veg$ShadeType = sample(1:2, nrow(veg), replace=TRUE)
stand@patches[[1]]@vegetation = establishTrees(veg, stand@hexagon@supp[['inner.radius']])
stand3D(stand)
dummy = plant3D(stand)
rot.z = rotationMatrix(0, 0, 0, 1)
rot.y = rotationMatrix(0, 1, 0, 0)
rgl.viewpoint(userMatrix = rot.y \%*\% rot.z, fov=1)

rgl.clear()
dgvm3d.options(establish.method = "sunflower")
stand@patches[[1]]@vegetation = establishTrees(veg, stand@hexagon@supp[['inner.radius']])
stand3D(stand)
dummy = plant3D(stand)

rgl.clear()
dgvm3d.options(establish.method = "row")
stand@patches[[1]]@vegetation = establishTrees(veg, stand@hexagon@supp[['inner.radius']],
                                               jitter=TRUE, amount=0.01)
stand3D(stand)
dummy = plant3D(stand)
}
}
