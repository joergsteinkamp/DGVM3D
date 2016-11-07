succession <- function(vegetation, patch.id=0, init.year=1901, years=seq(1950, 2000, 10)) {
  print("DUMMY")
}

#' Visualize a snapshot of patches.
#'
#' @param vegetation the data.frame of individual trees
#' @param stand.id the stand to take a snapshot off.
#' @param patch.id all patches (default) or just one.
#' @param year which year to take the snapshot off.
#' @return a \code{\link{Stand-class}}.
#' @export
#' @author Joerg Steinkamp \email{steinkamp.joerg@@gmail.com}
snapshot <- function(vegetation, stand.id=0, patch.id=NULL, year=2000) {
  SID=PID=Year=NULL
  patch.ids = unique(vegetation$PID)
  npatch = length(patch.ids)
  vegetation = subset(vegetation, SID==stand.id & Year==year)
  if (!is.null(patch.id)) {
    vegetation = subset(vegetation, PID==patch.id)
    patch.ids = patch.id
    npatch=1
  }

  stand <- initStand(npatch=npatch)
  for (i in 1:npatch) {
    if (!all( c("x", "y") %in% colnames(vegetation))) {
      if (dgvm3d.options("verbose"))
        message("Randomly distribution trees in patch")
      stand@patches[[i]]@vegetation = establishPatch(subset(vegetation, PID==patch.ids[i]), stand@hexagon@supp[['inner.radius']])
    } else {
      if (any(is.na(vegetation$x)) || any(is.na(vegetation$y))) {
        warning("NAs in tree position. Randomly redistribution.")
        message("NAs in tree position. Randomly redistribution.")
        stand@patches[[i]]@vegetation = establishPatch(subset(vegetation, PID==patch.ids[i]), stand@hexagon@supp[['inner.radius']])
      } else {
        if (dgvm3d.options("verbose"))
          message("Using valid x/y values present in 'vegetation data.frame.")
        stand@patches[[i]]@vegetation = vegetation
      }
    }
    if (dgvm3d.options("verbose")) {
      message(sprintf("Total crownarea (patch %3i): %7.2f (needle: %7.2f; broadleaved: %7.2f)", patch.ids[i],
                      sum(stand@patches[[i]]@vegetation$Crownarea),
                      sum(stand@patches[[i]]@vegetation$Crownarea[stand@patches[[i]]@vegetation$LeafType==1]),
                      sum(stand@patches[[i]]@vegetation$Crownarea[stand@patches[[i]]@vegetation$LeafType==2])
      ))
    }
  }
  stand3D(stand)
  stand = plant3D(stand)
  return(stand)
}

