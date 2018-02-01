#' Get the shape of a single flame
#'
#' @param faces number of side and height
#' @param radius maximum width
#' @param dz increase in height per z-side
#' @param z.exp exponetial z factor
#' @param expand linear width (x/y) expend factor with height
#' @param turn twist the flame a bit
#'
#' @return list of vertices and ids to be used with rgl::triangles3d
#' @export
#'
#' @examples
#' \dontrun{
#' center = getFlame(dz=0.8)
#' triangles3d(center$vertices[center$id[, (2 * 20 + 1):150], ],
#'             col="#e6ffff", alpha=1, shininess=1,lit=FALSE)
#' inner = getFlame(dz=0.97, expand=2)
#' triangles3d(inner$vertices[inner$id[, (2 * 20 + 1):175], ],
#'             col="#f0ff00", alpha=0.6, shininess=1,lit=FALSE)
#' outer = getFlame(dz=1, expand=3)
#' triangles3d(outer$vertices[outer$id[, (2*20+1):200], ],
#'             col="#ce1301", alpha=0.3,shininess=10,lit=FALSE)
#' }
getFlame <- function(faces=10, radius=0.3, dz=1, z.exp=1.1, expand=1, turn=0) {
  phi    <- seq(0, 2 * (1 - 1 / faces) * pi, length.out=faces)
  vertices  <- data.frame(x=sin(phi) * radius * sin(1/faces*pi),
                          y=cos(phi) * radius * sin(1/faces*pi),
                          z=0)

  x.skrew <- sin(turn + 2 * pi) * radius / 3
  y.skrew <- sin(turn + pi * (1 / faces)) * radius / 3

  vertices$x = vertices$x * expand + mean(vertices$x)
  vertices$y = vertices$y * expand + mean(vertices$y)

  id <- NULL
  for (i in 1:faces) {
    if (i %% 2) {
      phi = phi + (phi[2] - phi[1]) / 2
    } else {
      phi = seq(0, 2 * (1 - 1 / faces[1]) * pi, length.out=length(phi))
    }

    ## the regular shrinkage
    x.reg <- sin(phi) * radius * expand * (faces - i) / faces * sin(i/faces*pi) #* (faces - i) / faces
    y.reg <- cos(phi) * radius * expand * (faces - i) / faces * sin(i/faces*pi) #* (faces - i) / faces
    x.skrew <- sin(turn + 2 * pi * (faces - i / faces)) * radius / 3
    y.skrew <- sin(turn + pi * (i / faces)) * radius / 3
    layer = data.frame(x=x.reg + x.skrew,
                       y=y.reg + y.skrew,
                       z=dz * z.exp^i - dz)

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

  return(list(vertices=vertices, id=id))
}

#' add Fire to the stand or succession
#'
#' @param stand the stand object
#' @param patch.id the ID of a patch if NULL all are used
#' @param limit define a lower bound below which no fire should be plotted
#'
#' @return NULL
#' @export
#' @importFrom stats rlnorm rnorm
#'
#' @examples
#' \dontrun{
#' stand=snapshot(dgvm3d.succession[[8]], patch.id=4, year=1905)
#' rgl.clear("lights")
#' rgl.light( theta = -25, phi = 30, specular = "#AAAAAA")
#' fire3D(stand)
#' }
fire3D <- function(stand=NULL, patch.id=NULL, limit=0.5) {
  if (is.null(patch.id))
    patch.id <- 1:length(stand@patches)

  for (i in patch.id) {
    if (nrow(stand@patches[[i]]@vegetation) == 0)
      next
    if (stand@patches[[i]]@vegetation$Fireprob[1] < limit)
      next
    fire.prob = stand@patches[[i]]@vegetation$Fireprob[1]
    inner.radius = stand@hexagon@supp$inner.radius
    ##    Fire = NULL
    for (j in 1:round(200*fire.prob^2)) {
      ## angle of each flame
      phi    <- runif(1) * 2 * pi
      ## fractional distance from center of each flame
      dist   <- rbeta(1, 1.5, 1)
      ## absolute distance from center in x/y direction
      offset <- c(sin(phi) * dist * inner.radius,
                  cos(phi) * dist * inner.radius) * fire.prob
      ## random twist of flame top
      turn   <- rnorm(1, sd=2)
      ## radius/height reduction depending on distance from center
      ##      Fire = append(Fire, sqrt(sum(offset^2)) / inner.radius)
      radius <- rlnorm(1, meanlog=-0.2 * (2 + sqrt(sum(offset^2))), sdlog=0.1)
      dz     <- 0.5 + rlnorm(1, meanlog=-0.4 * sqrt(sum(offset^2)), sdlog=0.3)
      ## center (whitish)
      x = getFlame(radius=radius, dz=dz*0.8, turn=turn)
      x$vertices$x = x$vertices$x + offset[1] + stand@patch.pos[i, 'x']
      x$vertices$y = x$vertices$y + offset[2] + stand@patch.pos[i, 'y']
      x$vertices$z = x$vertices$z + stand@patch.pos[i, 'z']
      triangles3d(x$vertices[x$id[, (2 * 20 + 1):150], ], col="#e6ffcc", alpha=1, shininess=1,lit=F)
      ## inner ( yellow)
      x = getFlame(radius=radius, dz=dz*0.97, turn=turn, expand=2)
      x$vertices$x = x$vertices$x + offset[1] + stand@patch.pos[i, 'x']
      x$vertices$y = x$vertices$y + offset[2] + stand@patch.pos[i, 'y']
      x$vertices$z = x$vertices$z + stand@patch.pos[i, 'z']
      triangles3d(x$vertices[x$id[, (2 * 20 + 1):175], ], col="#f0ff00", alpha=0.6, shininess=1,lit=F)
      ## outer ( red)
      x = getFlame(radius=radius, dz=dz, expand=3, turn=turn)
      x$vertices$x = x$vertices$x + offset[1] + stand@patch.pos[i, 'x']
      x$vertices$y = x$vertices$y + offset[2] + stand@patch.pos[i, 'y']
      x$vertices$z = x$vertices$z + stand@patch.pos[i, 'z']
      triangles3d(x$vertices[x$id[, (2*20+1):200], ], col="#ce1301", alpha=0.3,shininess=10,lit=F)
    }
  }
  return(invisible(NULL))
}
