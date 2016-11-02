#' poplulate a patch with it´s vegetation
#'
#' @param stand the stand
#' @param patch.id the patch to polulate
#' @param vegetation the vegetation data.frame
#' @param sample number of samples to determine the next trees position
#' @return the vegetation data.frame with the positions
#' @include classes.R
#' @import RColorBrewer
#' @export
#' @author Joerg Steinkamp \email{steinkamp.joerg@@gmail.com}
#' @examples
#' stand=initStand(npatch=1)
#' veg = data.frame(DBH=rep(0.5, 100))  ##rgamma(100, 2.5, 9))
#' veg$Height = veg$DBH * 35  ## * rbeta(nrow(veg),10,1)
#' veg$Crownarea = veg$DBH * 5 ## * rnorm(nrow(veg), 1, 0.1)
#' veg$LeafType = sample(0:1, nrow(veg), replace=TRUE)
#' veg$ShadeType = sample(0:1, nrow(veg), replace=TRUE)
#' stand@patches[[1]]@vegetation = initPatch(veg, stand@hexagon@supp[['inner.radius']])
initPatch <- function(vegetation=NULL, radius=1, samples=3) {
  vegetation$x = NA
  vegetation$y = NA
  phi <- runif(1) * 2 * pi
  r   <- runif(1) * stand@hexagon@supp[['inner.radius']]
  vegetation$x[1] = sin(phi) * r
  vegetation$y[1] = cos(phi) * r
  for (i in 2:nrow(vegetation)) {
    phi <- runif(samples) * 2 * pi
    ## slightly biased towards the center, since max distance below tends to place all points along the edge.
    ## Need to find the optimum values (depends also in number of samples)
    r   <- rbeta(samples, 0.97, 1.4) * stand@hexagon@supp[['inner.radius']]
    new.x <- sin(phi) * r
    new.y <- cos(phi) * r

    dist <- matrix(NA, i-1, samples)
    for (j in 1:samples)
      dist[,j] <- sqrt((new.x[j] - vegetation$x[1:(i-1)])^2 + (new.y[j] -vegetation$y[1:(i-1)])^2)

    if (i==2) {
      vegetation$x[i] = new.x[which.max(dist)]
      vegetation$y[i] = new.y[which.max(dist)]
    } else {
      dist = apply(dist, 2, function(x){
        x[x - vegetation$DBH[1:(i-1)] - vegetation$DBH[i] <= 0.03] = NA
        return(x)
      })
      dist = dist[apply(dist, 1, function(x){all(is.finite(x))}), ]
      vegetation$x[i] = new.x[which.max(colSums(dist))]
      vegetation$y[i] = new.y[which.max(colSums(dist))]
    }
  }
  return(vegetation)
}
