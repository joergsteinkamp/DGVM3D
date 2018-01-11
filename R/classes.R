#' a Information to draw a triangular object
#'
#' @slot vertices the vertices of the bject
#' @slot id the column indices of the vertex matrix to draw the triangular body
#' @slot supp supplementary information
#' @exportClass TriangBody
setClass("TriangBody",
         slots=c(vertices = "matrix",
                 id       = "numeric",
                 supp     = "list")
)

#' One model patch
#'
#' This defines the basic class
#'
#' @slot id unique ID
#' @slot pid the patch id in the vegtation data.frame
#' @slot soil vector of soil layer depth
#' @slot vegetation the vegetation data.frame
#' @slot color.table lookup table for coloring
#' @exportClass Patch
setClass("Patch",
         slots = c(id          = "numeric",
                   pid         = "numeric",
                   soil        = "numeric",
                   vegetation  = "data.frame",
                   color.table = "list")
)

#' One model stand consisting of several patches
#'
#' @slot patches list of patches in one stand
#' @slot area the area of each patch
#' @slot year the year of the current patch vegetation
#' @slot hexagon a \code{\link{TriangBody-class}} Hexagon definition used for all patches
#' @slot layout either 'linear' or 'square'
#' @slot composition either 'spatial' or 'temporal'. Has no effect yet.
#' @slot patch.pos the position of the patche hexagon centers
#' @exportClass Stand
setClass("Stand",
         slots=c(patches     = "list",
                 area        = "numeric",
                 year        = "numeric",
                 hexagon     = "TriangBody",
                 layout      = "numeric",
                 composition = "character",
                 patch.pos   = "matrix")
)

#' set some variables used in cascading functions
#'
#' @param x query character 'x' for its value.
#' @param patch.area the patch area in m^2.
#' @param samples 2 element vector. 1. number of samples to determine the next trees position. 2. max. number to repeat the sampling
#' @param overlap fraction of crownradius allowed to overlap.
#' @param sort.column 2 element vector: 1. vegetation data.frame culumn name to sort by. 2. "descending" (default) or "ascending".
#' @param establish.method where to place the next tree: 'random', 'min' or 'max' of valid sampled new positions.
#' @param establish.beta.parameters shape parameters for beta random value to get the distance from the patch center. For 'random' it should be biased away from the center c(1.03, 0.9), whereas for 'max' this should be biased towards from the center c(0.97, 1.4), otherwise trees tend to accumulate in the center.
#' @param color.column name of the vegetation column to create the canopy colors from.
#' @param verbose print some information.
#' @export
dgvm3d.options <- function(x=NULL,
                           patch.area=NULL,
                           samples=NULL,
                           overlap=NULL,
                           sort.column=NULL,
                           establish.method=NULL,
                           establish.beta.parameters=NULL,
                           color.column=NULL,
                           verbose=NULL) {
  if (!is.null(x)) {
    if (x=="default") {
      options(dgvm3d.patch.area=1000)
      options(dgvm3d.samples=c(10, 10))
      options(dgvm3d.overlap=0.5)
      options(dgvm3d.sort.column=c("Crownarea", "descending"))
      options(dgvm3d.establish.method="random")
      options(dgvm3d.establish.beta.parameters=c(1.1, 0.9))
      options(dgvm3d.color.column="ShadeType")
      options(dgvm3d.verbose=TRUE)
      return(TRUE)
    } else {
      if (grepl("^dgvm3d", x)) {
        return(options(x)[[1]])
      } else {
        return(options(paste0("dgvm3d.",x))[[1]])
      }
    }
  }
  if (!is.null(patch.area))
    options(dgvm3d.patch.area=patch.area)
  if (!is.null(samples))
    options(dgvm3d.samples=samples)
  if (!is.null(overlap))
    options(dgvm3d.overlap=overlap)
  if (!is.null(sort.column))
    options(dgvm3d.sort.column=sort.column)
  if (!is.null(establish.method))
    options(dgvm3d.establish.method=establish.method)
  if (!is.null(establish.beta.parameters))
    options(dgvm3d.establish.beta.parameters=establish.beta.parameters)
  if (!is.null(color.column))
    options(dgvm3d.color.column=color.column)
  if (!is.null(verbose))
    options(dgvm3d.verbose=verbose)
}

## initializing the global options
.onAttach <- function(libname, pkgname) {
  dgvm3d.options("default")
}


#' Vegetation timeseries data from 1860-2005
#'
#' A list of 3 data.frames with simulation results of a LPJ-GUESS model run without random patch disturbance at 3 locations.
#' All patches at all locations were disturbed in 1859.
#'
#'  @name dgvm3d.succession
#'  @docType data
#'  @keywords data
"dgvm3d.succession"
