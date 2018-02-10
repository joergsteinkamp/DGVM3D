#' fill a polygon (number of vertices) with triangles
#'
#' Method 'circular' (default) used the most triangles so far by going round in the circle and connecting the next three vertices. 'fix' uses vertex id 1 and creates triangles to all other points round. 'planar' always flips the triangles.
#'
#' @param n number of vertices.
#' @param method Method how to organize the triangles 'circular', 'planar', 'fix' and 'center'.
#' @param center The center vertex ID for the central point (method 'center' only; default NA).
#' @return A vector of indices for the polygon vertices.
#' @export
#' @examples
#' par(mfrow=c(2,2))
#' for (m in c("plan", "fix", "center", "")) {
#'   faces <- sample(12:20, 1)
#'   vertices <- sapply(seq(0, 2*pi*(faces-1)/faces, length.out=faces),
#'                      function(x){c(sin(x), cos(x))})
#'   tri = triClose(faces, method=m)
#'   if (m == "center") {
#'     tri[is.na(tri)] = faces + 1
#'     vertices = cbind(vertices, c(mean(vertices[1,]), mean(vertices[2, ])))
#'   }
#'   plot(vertices[1,1:faces], vertices[2,1:faces], type="b")
#'   text(x=1.05*vertices[1,], y=1.05*vertices[2,], labels=1:faces, adj=0.5)
#'   for (i in seq(1, length(tri), 3))
#'     polygon(vertices[1,tri[i:(i+2)]], vertices[2,tri[i:(i+2)]],
#'             col=rgb(runif(1), runif(1), runif(1)))
#' }
#'
#' par(mfrow=c(2,2))
#' for (faces in c(6, 12, 13, 25)) {
#'   vertices <- sapply(seq(0, 2*pi*(faces-1)/faces, length.out=faces),
#'                      function(x){c(sin(x), cos(x))})
#'   tri = triClose(faces, method=m)
#'   plot(vertices[1,], vertices[2,], type="b")
#'   text(x=1.05*vertices[1,], y=1.05*vertices[2,], labels=1:faces, adj=0.5)
#'   for (i in seq(1, length(tri), 3))
#'     polygon(vertices[1,tri[i:(i+2)]], vertices[2,tri[i:(i+2)]],
#'             col=rgb(runif(1), runif(1), runif(1)))
#' }
triClose <- function(n, method="circular", center=NA){
  if (is.vector(n) && length(n) > 1) {
    ids <- n
    n <- length(n)
  } else if (is.vector(n) && length(n) == 1) {
    ids <- 1:n
  } else {
    stop(sprintf("Don't know how to deal with data.type '%s' of 'n'!", class(n)))
  }

  if (n <= 3) {
    stop("Minimum is 3.")
  }

  if (grepl("^cent", method)) {
    if (length(center) != 1)
      center = NA
    id <- sapply(1:n, function(x) {return(c(x, x %% n + 1, center))})
    return(ids[id])
  } else if (n > 5 && grepl("^fix", method)) {
    id <- sapply(2:(n - 1), function(x){return(c(1, x, x + 1))})
    return(ids[id])
  } else if (n > 5 && grepl("^plan", method)) {
    id <- c(1, ceiling(n / 2) + 1, ceiling(n / 2))
    for (i in 1:ceiling(n / 2 - 2))
      id = append(id, c(id[length(id)], id[length(id) - 2], id[length(id) - 2] + (i %% 2) * 2 - 1))
    id = append(id, c(id[2], id[1], n))
    for (i in 1:ceiling(n / 2 - 2))
      id = append(id, c(id[length(id)], id[length(id) - 2], id[length(id) - 2] + (i %% 2) * 2 - 1))
    return(ids[id])
  } else {
    id <- c()
    for (i in seq(1, floor(n / 2) * 2, 2))
      id <- append(id, c(i, i + 1, i + 2))
    if (id[length(id)] == n + 1)
      id[length(id)] = 1
    if (n > 4) {
      inner.tri <- sapply(seq(1, floor((n - 3) / 2) * 2, 2), function(x){c((x - 1) * 2 + 1, (x - 1) * 2 + 3, (x - 1) * 2 + 5)})
      inner.tri = apply(inner.tri, 2, function(x) {
        if (sum(x > n) == 1) {
          x[x > n] = 1
        } else if (sum(x > n) == 2) {
          x[2] = x[2] - n - n %% 2
          x[3] = x[3] - n + 2 - n %% 2
        } else if (sum(x > n) == 3) {
          x = (x - n - n %% 2) * 2 - 1
        }
        return(x)
      })
      while (sum(apply(inner.tri, 2, function(x) {sum(x > n)})))
        inner.tri[which(inner.tri > n)] = (inner.tri[which(inner.tri > n)] - n - n %% 2) * 2 - 1
      id = append(id, as.vector(inner.tri))
    }
    return(ids[id])
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
#' @examples
#' if (require(rgl)) {
#'   hexagon <- getHexagon(area=dgvm3d.options("patch.area"), z=c(0, -2))
#'   triangles3d(hexagon@vertices[hexagon@id, ], col="brown")
#' } else {
#'   message("the library 'rgl' is required for this example!")
#' }
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

#' Calculate an ellipsoid
#'
#' @param radius x/y radius
#' @param height z height
#' @param faces approx. number of faces. If two values given: 1.) around z-axis; 2.) along z-axis.
#' @return a \code{\link{TriangBody-class}}
#' @importFrom methods new
#' @export
#' @examples
#' if (require(rgl)) {
#'   ellipsoid=getEllipsoid(height=2)
#'   triangles3d(ellipsoid@vertices[ellipsoid@id, ], col="green")
#' } else {
#'   message("the library 'rgl' is required for this example!")
#' }
getEllipsoid <- function(radius=0.5, height=1, faces=c(6, 3)) {
  ## faces: 2 * faces[1] * faces[2]
  if (length(faces)==1) {
    faces = ceiling(faces/6)
    faces = c(faces*2, faces)
  }

  phi    <- seq(0, 2 * (1 - 1 / faces[1]) * pi, length.out=faces[1])
  theta  <- seq(pi / (2 * faces[2] - 1.5), pi - pi / (2 * faces[2] - 1.5), length.out=faces[2] + 1)

  vertices  <- data.frame(x=sin(phi) * sin(theta[1]) * radius,
                          y=cos(phi) * sin(theta[1]) * radius,
                          z=(cos(theta[1])+1)/2 * height)
  theta = theta[2:length(theta)]

  id <- NULL
  for (i in 1:(faces[2])) {
    if (i %% 2) {
      phi = phi + (phi[2] - phi[1]) / 2
    } else {
      phi = seq(0, 2 * (1 - 1 / faces[1]) * pi, length.out=faces[1])
    }

    layer = data.frame(x=sin(phi) * sin(theta[i]) * radius,
                       y=cos(phi) * sin(theta[i]) * radius,
                       z=(cos(theta[i])+1)/2 * height)
    vertices = rbind(vertices, layer)

    i1 = (i - 1) * faces[1] + 1:faces[1]
    i2 = (i - 1) * faces[1] + (i1 %% faces[1]) + 1
    if (i %% 2) {
      i3 = (i - 1) * faces[1] + (faces[1] + 1):(2 * faces[1])
      id = cbind(id, rbind(i1, i2, i3))
      id = cbind(id, rbind(i2, i3, (i1 %% faces[1]) + i*faces[1] + 1))
    } else {
      i3 = ((i - 1) * faces[1] + (faces[1] + 1):(2 * faces[1]))[c(2:faces[1], 1)]
      id = cbind(id, rbind(i1, i2, i3))
      id = cbind(id, rbind(i2, i3, i3[c(2:faces[1], 1)]))
    }
  }
  id = as.vector(id)

  ## close bottom
  id = append(id, triClose((nrow(vertices) - faces[1] + 1):nrow(vertices), "center"))
  id[is.na(id)] = nrow(vertices) + 1
  vertices = rbind(vertices, c(mean(vertices$x[(nrow(vertices) - faces[1] + 1):nrow(vertices)]),
                    mean(vertices$y[(nrow(vertices) - faces[1] + 1):nrow(vertices)]),
                    0))
  ## close top
  id = append(id, triClose(1:faces[1], "center"))
  id[is.na(id)] = nrow(vertices) + 1
  vertices = rbind(vertices, c(mean(vertices$x[1:faces[1]]),
                               mean(vertices$y[1:faces[1]]),
                               height))
  return(new("TriangBody", vertices=as.matrix(vertices), id=id, supp=list(radius=radius, height=height)))
}

## sunflower is from: https://stackoverflow.com/questions/28567166/uniformly-distribute-x-points-inside-a-circle
#' Calc the current radius
#'
#' @param k current value
#' @param n total number
#' @param b number of boundary points
#'
#' @return radius
sunflower.radius <- function(k, n, b) {
  if (k > n - b) {
    return(1)
  }
  sqrt(k - 1 / 2) / sqrt(n - (b + 1) / 2)
}
#' return the positions
#'
#' @param n total number of trees
#' @param alpha smoothing factor for boundary points
#'
#' @return position data.frame
#' @export
sunflower.disc <- function(n, alpha=0) {
  ret = data.frame()
  b = round(alpha * sqrt(n))
  phi = (sqrt(5) + 1) / 2
  for (k in 1:n) {
    r = sunflower.radius(k, n, b)
    theta = 2 * pi * k / phi^2
    ret = rbind(ret, data.frame(x=r * cos(theta),
                                y=r * sin(theta)))
  }
  return(ret)
}

#' Random distribution in a circle
#'
#' @param n total numer of trees
#' @param strict should the value 2pi be excluded
#'
#' @return data.frame of positions
#' @export
random.disc <- function(n, strict=FALSE) {
  r = runif(n, 0., 1.)
  theta = runif(n, 0., 2. * pi)
  if (strict) {
    while (any(theta == 2 * pi)) {
      theta[theta == 2. * pi] = runif(sum(theta == 2. * pi), 0., 2. * pi)
    }
  }
  data.frame(x=sqrt(r) * cos(theta),
             y=sqrt(r) * sin(theta))
}


#' row-wise distribution of points in a disc
#'
#' @param n number of points
#'
#' @return data.frame of x and y positions
#' @export
#'
#' @examples
#' par(mfrow=c(2,2), mai=c(0,0,0,0))
#' for (n in c(51, 250, 280, 1000)) {
#'   ret=row.disc(n)
#'   plot(sin(seq(0, 2*pi, length.out=361)), cos(seq(0, 2*pi, length.out=361)), type="l", axes = FALSE, ylab = "", xlab="")
#'   points(ret)
#' }
#' par(mfrow=c(2,2), mai=c(0,0,0,0))
#' for (n in c(120, 640, 1280, 2400)) {
#'   ret=row.disc(n)
#'   plot(sin(seq(0, 2*pi, length.out=361)), cos(seq(0, 2*pi, length.out=361)), type="l", axes = FALSE, ylab = "", xlab="")
#'   points(ret)
#' }
row.disc <- function(n) {
  warning("This method is still very buggy due to lazy use of round/floor/ceiling.")
  warning("See examples, to see the potentially wrong patterns.")
  d = sqrt(pi / n / 2)
  ret = data.frame()
  ## round the number of rows in half a circle to the lower even number
  nrows = floor(1 / d / 2) + 1
  drow = 1 / nrows
  for (i in 1:nrows) {
    theta = acos(1 - (i - 0.5) * drow)
    npr = floor(2 * sin(theta) / d) # number per row
    if (i == nrows && nrow(ret) + npr * 2 > n) {
      npr  = (n - nrow(ret)) / 2
    } else if (i == nrows && nrow(ret) + npr * 2 < n) {
      npr  = (n - nrow(ret)) / 2
    }
    if (npr <= 0)
      next
    # upper half (y > 0)
    ret = rbind(ret, data.frame(x=seq(-sin(theta) * (1 - d / 2),
                                      sin(theta) * (1 - d / 2), length.out = npr),
                                y=1 - (i - 0.5) * drow))
    ## if odd and first row, remove one value
    if (i == 1 && n %% 2)
      npr = npr - 1
    ## lower half (y < 0)
    ret = rbind(ret, data.frame(x=seq(-sin(theta) * (1 - d / 2),
                                      sin(theta)  * (1 - d / 2), length.out = npr),
                                y=(i - 0.5) * drow - 1 ))

  }
  return(ret)
}


