library(DGVM3D)

rpm <- 3       #  rounds per minute
fps <- 10      # frames per second
duration <- 20 # seconds

## md <- file.path(tempdir(), "movie")
## dir.create(md)

ystart <- 1860
yend   <- 1959
location <-  "Chile - temperate/mediterranean forest"

dummy <- open3d(windowRect=c(0, 0, 800, 600), scale=c(1, 1, 1), FOV=1)
stand <- snapshot(dgvm3d.succession[[location]],  year = ystart)
rgl.clear("lights")
rgl.light( theta = -25, phi = 30, specular = "black", diffuse = "#FFFFFF")

rot.y = rotationMatrix(-pi / 3, 1, 0, 0)
for (i in 0:50) {
  rot.z = rotationMatrix(i * pi/360, 0, 0, 1)
  rgl.viewpoint(userMatrix = rot.y %*% rot.z, fov=1, zoom=0.75)
  ## fname = sprintf("movie%08d.png", i)
  ## rgl.snapshot(file.path(md, fname))
  ## system(paste0("mogrify -fill black -pointsize 36 -annotate +50+68 '", location, " ", ystart, "' ", file.path(md, fname)))
}

for (y in seq(ystart + 1, yend, 1)) {
  stand = updateStand(stand, dgvm3d.succession[[location]], y)
  clear3d()
  stand3D(stand)
  par3d(skipRedraw=TRUE)
  stand = plant3D(stand)
  fire3D(stand, limit=0.2)
  par3d(skipRedraw=FALSE)

  for (i in 1:50) {
    rot.z = rotationMatrix(((y - ystart) * 50 + i) * pi / 360, 0, 0, 1)
    rgl.viewpoint(userMatrix = rot.y %*% rot.z, fov=1, zoom=0.75)
    ## fname = sprintf("movie%08d.png", (y - ystart) * 50 + i)
    ## rgl.snapshot(file.path(md, fname))
    ## system(paste0("mogrify -fill black -pointsize 36 -annotate +50+68 '", location, " ", y, "' ", file.path(md, fname)))
  }
}
## owd = getwd()
## setwd(md)
## system("ffmpeg -y -f image2 -r 25 -i movie%08d.png -vcodec libx264 movie.mp4")
## setwd(owd)
