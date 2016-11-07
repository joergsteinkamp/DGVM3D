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
#' dgvm3d.options("default")
#' stand = initStand(npatch=1)
#' veg = data.frame(DBH=rep(0.5, 100))
#' veg$Height    = veg$DBH * 35
#' veg$Crownarea = veg$DBH * 5
#' veg$LeafType  = sample(0:1, nrow(veg), replace=TRUE)
#' veg$ShadeType = sample(0:1, nrow(veg), replace=TRUE)
#' stand@patches[[1]]@vegetation = establishPatch(veg, stand@hexagon@supp[['inner.radius']])
establishPatch <- function(vegetation=NULL, radius=1) {
  if (is.null(vegetation))
    stop("'vegetation' data.frame is missing!")

  samples          <- dgvm3d.options("samples")
  overlap          <- dgvm3d.options("overlap")
  sort.column      <- dgvm3d.options("sort.column")
  establish.method <- dgvm3d.options("establish.method")
  est.beta.param   <- dgvm3d.options("establish.beta.parameters")

  if (dgvm3d.options("verbose")) {
    message("### establishPatch ###")
    message(sprintf("Using %i samples in max. %i repetitions (max. crown radius overlap: %0.3f).", samples[1], samples[2], overlap))
    message(paste0("Sorting by '", sort.column[1], "' in '", sort.column[2], "' order."))
  }

  if (any(colnames(vegetation) == sort.column[1])) {
    if (sort.column[2] == "ascending") {
      vegetation = eval(parse(text=paste0("vegetation[with(vegetation, order(", sort.column[1],")), ]")))
    } else {
      vegetation = eval(parse(text=paste0("vegetation[with(vegetation, order(-", sort.column[1],")), ]")))
    }
  }

  if (nrow(vegetation)==0)
    return(vegetation)

  vegetation$x = NA
  vegetation$y = NA
  phi <- runif(1) * 2 * pi
  r   <- runif(1) * radius
  vegetation$x[1] = sin(phi) * r
  vegetation$y[1] = cos(phi) * r
  for (i in 2:nrow(vegetation)) {

    nwhile=0
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

      dist <- matrix(NA, i-1, samples[1])
      for (j in 1:samples[1])
        dist[,j] <- sqrt((new.x[j] - vegetation$x[1:(i-1)])^2 + (new.y[j] -vegetation$y[1:(i-1)])^2)

      crown.radius <- sqrt(vegetation$Crownarea[1:i]/pi)
      min.dist <- (crown.radius[1:(i-1)] + crown.radius[i]) * (1 - overlap)

      dist[dist < min.dist] = NA

      dist <- colSums(dist)
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
