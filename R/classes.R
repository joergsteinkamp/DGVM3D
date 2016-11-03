#' a Information to draw a triangular object
#'
#' @slot vertices the vertices of the bject
#' @slot id the column indices of the vertex matrix to draw the triangular body
#' @slot supp supplementary information
#' @exportClass TriangBody
#' @author Joerg Steinkamp \email{steinkamp.joerg@@gmail.com}
setClass("TriangBody",
         slots=c(vertices = "matrix",
                 id = "numeric",
                 supp ="list")
)

#' One model patch
#'
#' This defines the basic class
#'
#' @slot id unique ID
#' @slot soil vector of soil layer depth
#' @slot vegetation the vegetation data.frame
#' @slot color.table lookup table for coloring
#' @exportClass Patch
#' @author Joerg Steinkamp \email{steinkamp.joerg@@gmail.com}
setClass("Patch",
         slots = c(id = "numeric",
                   soil = "numeric",
                   vegetation = "data.frame",
                   color.table = "list")
)

#' One model stand consisting of several patches
#'
#' @slot patches list of patches in one stand
#' @slot area the area of each patch
#' @slot hexagon a \code{\link{TriangBody-class}} Hexagon definition used for all patches
#' @slot arrangement either 'linear' or 'square'
#' @slot composition either 'spatial' or 'temporal'. Has no effect yet.
#' @slot patch.pos the position of the patche hexagon centers
#' @exportClass Stand
#' @author Joerg Steinkamp \email{steinkamp.joerg@@gmail.com}
setClass("Stand",
         slots=c(patches = "list",
                area = "numeric",
                hexagon = "TriangBody",
                arrangement ="character",
                composition = "character",
                patch.pos = "matrix")
)
