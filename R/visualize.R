#' create a temporal succession
#'
#' @param vegetation data.frame
#' @param stand.id the Stand ID
#' @param patch.id the patch ID, if NULL all available ones are considered
#' @param init.year year, when to initialize the tree positions
#' @param years the years to be included
#' @return a stand object
#' @export
#' @examples
#' \dontrun{
#' stand=succession(dgvm3d.succession[[3]], init.year=1865, years=c(1865, seq(1875, 2000, 25)),
#'                  patch.id=sample(1:12, 3))
#' stand3D(stand)
#' stand=plant3D(stand)
#' }
succession <- function(vegetation, stand.id=1, patch.id=NULL, init.year=1901, years=seq(1950, 2000, 10)) {
  SID=PID=Year=NULL
  if (is.null(patch.id))
    patch.id = sort(unique(vegetation$PID))
  npatch = length(patch.id)
  init.vegetation = subset(vegetation, SID==stand.id & Year==init.year)
  stand <- initStand(npatch=npatch, year=init.year)

  for ( i in 1:npatch) {
    stand@patches[[i]]@vegetation = establishTrees(subset(init.vegetation, PID==patch.id[i]), stand@hexagon@supp[['inner.radius']])
    stand@patches[[i]]@pid = patch.id[i]
  }

  avail.years = sort(unique(vegetation$Year))
  avail.years = avail.years[which(avail.years>init.year)]
  avail.years = avail.years[which(avail.years<=max(years))]

  ts.stand = initStand(npatch=npatch*length(years), layout = c(npatch, length(years)), composition = "temporal")

  if (init.year==years[1]) {
    for (i in 1:npatch) {
      print(1+(i-1)*length(years))
      ts.stand@patches[[1+(i-1)*length(years)]] = stand@patches[[i]]
    }
  }

  for (i in 1:length(avail.years)) {
    stand = updateStand(stand, vegetation, year = avail.years[i])
    match.year = which(years==avail.years[i])
    if (length(match.year)==1) {
      for (j in 1:npatch) {
        ts.pid=match.year + (j-1)*length(years)
        ts.stand@patches[[ts.pid]] = stand@patches[[j]]
      }
    }
  }
  return(ts.stand)
}

#' Visualize a snapshot of patches.
#'
#' @param vegetation the data.frame of individual trees
#' @param stand.id the stand to take a snapshot off.
#' @param patch.id all patches (default) or just one.
#' @param year which year to take the snapshot off.
#' @return a \code{\link{Stand-class}}.
#' @export
#' @examples
#' \dontrun{
#' stand=snapshot(dgvm3d.succession[[1]])
#' }
snapshot <- function(vegetation, stand.id=1, patch.id=NULL, year=2000) {
  SID=PID=Year=NULL
  patch.ids = unique(vegetation$PID)
  npatch = length(patch.ids)
  vegetation = subset(vegetation, SID==stand.id & Year==year)
  if (!is.null(patch.id)) {
    vegetation = vegetation[vegetation$PID %in% patch.id, ]
    patch.ids = patch.id
    npatch=length(patch.id)
  }

  stand <- initStand(npatch=npatch, year=year)
  for (i in 1:npatch) {
    if (!all( c("x", "y") %in% colnames(vegetation))) {
      if (dgvm3d.options("verbose"))
        message("Randomly distributing trees in patch")
      stand@patches[[i]]@vegetation = establishTrees(subset(vegetation, PID==patch.ids[i]), stand@hexagon@supp[['inner.radius']])
      stand@patches[[i]]@pid = patch.ids[i]
    } else {
      if (any(is.na(vegetation$x)) || any(is.na(vegetation$y))) {
        warning("NAs in tree position. Random redistribution of missing positions.")
        message("NAs in tree position. Random redistribution of missing positions.")
        stand@patches[[i]]@vegetation = establishTrees(subset(vegetation, PID==patch.ids[i]), stand@hexagon@supp[['inner.radius']])
      } else {
        if (dgvm3d.options("verbose"))
          message("Using valid x/y values present in 'vegetation data.frame.")
        ## distance to nearest neighbour
        vegetation$dnn = sapply(1:nrow(vegetation), function(x) {
          min(sqrt((vegetation$x[x]-vegetation$x[-x])^2 + (vegetation$y[x] - vegetation$y[-x])^2))
        })
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

