#' fill a polygon (number of vertices) with triangles
#'
#' Method 'circular' (default) used the most triangles so far by going round in the circle and connecting the next three vertices. 'fix' uses vertex id 1 and creates triangles to all other points round. 'planar' always flips the triangles.
#'
#' @param n number of vertices.
#' @param method Method how to organize the triangles 'circular', 'planar' and 'fix'.
#' @return A vector of indices for the polygon vertices
#' @export
#' @author Joerg Steinkamp \email{steinkamp.joerg@@gmail.com}
#' @examples
#' par(mfrow=c(2,2))
#' for (m in c("plan", "fix", "")) {
#'   faces <- sample(12:20, 1)
#'   vertices <- sapply(seq(0, 2*pi*(faces-1)/faces, length.out=faces), function(x){c(sin(x)*1, cos(x)*1)})
#'   tri = triClose(faces, method=m)
#'   plot(vertices[1,], vertices[2,], type="b")
#'   text(x=1.05*vertices[1,], y=1.05*vertices[2,], labels=1:faces, adj=0.5)
#'   for (i in seq(1, length(tri), 3))
#'     polygon(vertices[1,tri[i:(i+2)]], vertices[2,tri[i:(i+2)]], col=rgb(runif(1), runif(1), runif(1)))
#' }
#'
#' par(mfrow=c(2,2))
#' for (faces in c(6, 12, 13, 25)) {
#'   vertices <- sapply(seq(0, 2*pi*(faces-1)/faces, length.out=faces), function(x){c(sin(x)*1, cos(x)*1)})
#'   tri = triClose(faces, method=m)
#'   plot(vertices[1,], vertices[2,], type="b")
#'   text(x=1.05*vertices[1,], y=1.05*vertices[2,], labels=1:faces, adj=0.5)
#'   for (i in seq(1, length(tri), 3))
#'     polygon(vertices[1,tri[i:(i+2)]], vertices[2,tri[i:(i+2)]], col=rgb(runif(1), runif(1), runif(1)))
#' }
triClose <- function(n, method="circular"){
  if (is.data.frame(n)) {
    if (sum(colnames(n) %in% c("x", "y", "z"))==3) {
      n <- matrix(c(n$x, n$y, n$z), nrow=3, byrow=TRUE)
    } else if (sum(colnames(n) %in% c("x", "y"))==2) {
      n <- matrix(c(n$x, n$y), nrow=2, byrow=TRUE)
    } else if (ncol(n)==2) {
      n <- matrix(c(n[,1], n[,2]), nrow=2, byrow=TRUE)
    } else if (ncol(n)>2) {
      if (ncol(n)>3)
        warning("More than 3 columns in data.frame, taking the first three as x,y,z coordinates!")
      n <- matrix(c(n[,1], n[,2], n[,3]), nrow=3, byrow=TRUE)
    } else {
      stop("Don't know how to handle input 'n'!")
    }
  } else if (is.matrix(n)) {
    if (nrow(n)>3 && (ncol(n)==2 || ncol(n)==3)) {
      n=t(n)
    } else if (nrow(n)>3 && ncol(n)>3) {
      stop(paste0("Don't kow how to deal with a ",nrow(n), "x",ncol(n)," matrix!"))
    }
  }

  if (is.matrix(n) && !grepl("^cent", method))
    n <- ncol(n)

  if (is.matrix(n)) {
    stop("Not yet implemented!")
  } else if (n < 3) {
    stop("There are no area with less than 3 corners!")
  } else if (n>5 && grepl("^fix", method)) {
    id <- sapply(2:(n-1), function(x){return(c(1,x, x+1))})
    return(id)
  } else if (n>5 && grepl("^plan", method)) {
    id <- c(1, ceiling(n/2)+1, ceiling(n/2))
    for (i in 1:ceiling(n/2-2))
      id = append(id, c(id[length(id)], id[length(id)-2], id[length(id)-2] + (i %% 2) * 2 - 1))

    id = append(id, c(id[2], id[1], n))
    for (i in 1:ceiling(n/2-2))
      id = append(id, c(id[length(id)], id[length(id)-2], id[length(id)-2] + (i %% 2) * 2 - 1))

    return(id)
  } else {
    id <- c()
    for (i in seq(1, floor(n/2)*2, 2))
      id <- append(id, c(i, i+1, i+2))
    if (id[length(id)] == n + 1)
      id[length(id)] = 1
    if (n > 4) {
      inner.tri <- sapply(seq(1, floor((n-3)/2)*2, 2), function(x){c((x-1)*2+1, (x-1)*2+3, (x-1)*2+5)})
      inner.tri = apply(inner.tri, 2, function(x){
        if (sum(x>n)==1) {
          x[x>n] = 1
        } else if (sum(x>n)==2) {
          x[2] = x[2] - n - n %% 2
          x[3] = x[3] - n + 2 - n %% 2
        } else if (sum(x>n)==3) {
          x = (x - n - n %% 2) * 2 - 1
        }
        return(x)
      })

      while (sum(apply(inner.tri, 2, function(x) {sum(x>n)})))
        inner.tri[which(inner.tri>n)] = (inner.tri[which(inner.tri>n)] - n - n %% 2) * 2 - 1
      id = append(id, as.vector(inner.tri))
    }
    return(id)
  }
  stop("This line should never be reached!")
}

### calculate the vertices (x/y) and edges (from one index of vertices to the next)
#' Calculate a 3D hexagon
#'
#' @param area the area of the hexagon
#' @param outer.radius the outer radius of the hexagon
#' @param inner.radius the inner radius of the hexagon
#' @param z the height of the hexagon as 2 element vector
#' @return  a \code{\link{TriangBody-class}}
#' @importFrom methods new
#' @export
#' @author Joerg Steinkamp \email{steinkamp.joerg@@gmail.com}
getHexagon <- function(area=NA, outer.radius=NA, inner.radius=NA, z=c(0,1)) {
  if (!is.na(area)) {
    orad = sqrt(2.0 / 3.0 / sqrt(3.0) * area)
    irad = sqrt(3.0) / 2.0 * orad
  } else {
    stop("Not yet ready")
  }
  vertices <- sapply(seq(0.0, pi * 5.0/3.0, length.out=6), function(x){c(cos(x)*orad, sin(x)*orad, 0)})
  ind <- c(1,2,3,
           3,4,5,
           5,6,1,
           1,3,5)
  if (length(z) == 2) {
    vertices = cbind(vertices, vertices)
    vertices[3, 1:6] = z[1]
    vertices[3, 7:12] = z[2]
    ind = append(ind, ind + 6)
    for (i in 0:4)
      ind = append(ind, c(1, 2, 7, 2, 7, 8) + i)
    ind = append(ind, c(6, 1, 12, 1, 7, 12))
  }
  return(new("TriangBody", vertices=t(vertices), id=ind, supp=list(inner.radius=irad, outer.radius=orad, area=area)))
}


#' calculate a cone
#'
#' @param radius the outer radius of the cone
#' @param height the height of the cone
#' @param faces number of triangular sides
#' @param close logical should the bottom side be closed.
#' @return a \code{\link{TriangBody-class}}
#' @importFrom methods new
#' @export
#' @author Joerg Steinkamp \email{steinkamp.joerg@@gmail.com}
#' @examples
#' if (require(rgl)) {
#'   cone=getCone(faces=13, close=TRUE)
#'   triangles3d(cone@vertices[cone@id, ], col="green")
#' } else {
#'   message("the library 'rgl' is required for this example!")
#' }
getCone <- function(radius=0.5, height=1, faces=72, close=FALSE) {
  vertices <- sapply(seq(0, 2*pi*(faces-1)/faces, length.out=faces), function(x){c(sin(x)*radius, cos(x)*radius)})
  vertices = rbind(vertices, 0)
  vertices = cbind(vertices, c(0, 0, height))
  ind <- as.vector(sapply(1:faces, function(x) {c(x, x+1, NA)}))
  ind[3*faces-1] = 1
  ind[which(is.na(ind))] = faces+1

  if (close) {
    ind = append(ind, triClose(faces, method="plan"))
  }
  return(new("TriangBody", vertices=t(vertices), id=ind, supp=list(radius=radius, height=height)))
}


#' draw a quadrilateral ellipsoid
#'
#' I copied and modified it from 'Code demonstations' rgl::shapes3d (searched with "??ellipsoid" on commandline)
#'
#' @param rx radius in x direction
#' @param ry radius in y direction
#' @param rz radius in z direction
#' @param ctr 3-element vector. center of the ellipsoid
#' @param col the color to use.
#' @param n number of vertices along z-axis and around xy rotation
#' @param opacity the alpha value for the surface
#' @import rgl
#' @export
drawEllipsoid <- function(rx=1, ry=1, rz=1, ctr=c(0, 0, 0), n=72, col="green", opacity=opacity) {
  degvec <- seq(0, pi, length=n)
  ecoord2 <- function(p) {
    c(rx * cos(p[1]) * sin(p[2]),
      ry * sin(p[1]) * sin(p[2]),
      rz * cos(p[2])) }
  v <- apply(expand.grid(2 * degvec, degvec), 1, ecoord2)
  v[1, ] = v[1, ] + ctr[1]
  v[2, ] = v[2, ] + ctr[2]
  v[3, ] = v[3, ] + ctr[3]

  e <- expand.grid(1:(n-1),1:n)
  i1 <- apply(e,1,function(z)z[1]+n*(z[2]-1))
  i2 <- i1+1
  i3 <- (i1+n-1) %% n^2 + 1
  i4 <- (i2+n-1) %% n^2 + 1
  i <- rbind(i1,i2,i4,i3)
  quads3d(v[1,i],v[2,i],v[3,i], col=col, alpha=opacity)
}


