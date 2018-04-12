library(DGVM3D)

## pictures per year
ppy <- 10
## angle per year
apy = 0.5

## create a temporary directory
md <- file.path(tempdir(), "movie")
dir.create(md)

## how many years should be visualized
ystart <- 1860
yend   <- 2005
location <-  "Chile - temperate/mediterranean forest"

## initialize the window and the stand
dummy  <- open3d(windowRect=c(0, 0, 800, 600), scale=c(1, 1, 1), FOV=1)
stand  <- snapshot(dgvm3d.succession[[location]],  year=ystart)
nsoillayer <- sum(sapply(stand@patches, function(x) {length(x@soil) - 1}))

## set the lights to be not so reflective
rgl.clear("lights")
rgl.light( theta=-25, phi=30, specular="black", diffuse="#FFFFFF")

## do not change the bounding box and therefore the center of view.
## Sadly the viewpoint cannot be set with rgl, only the observer position with observer3d
par3d(ignoreExtent=TRUE)

## view a bit more from the top
rot.y = rotationMatrix(-pi / 3, 1, 0, 0)
for (i in 0:ppy) {
  ## start the rotation around the z-axis
  rot.z = rotationMatrix(i * pi * apy / 360, 0, 0, 1)
  rgl.viewpoint(userMatrix = rot.y %*% rot.z, fov=1, zoom=0.75)
  ## save an image and add a label
  fname = sprintf("movie%08d.png", i)
  rgl.snapshot(file.path(md, fname))
  system(paste0("mogrify -fill black -pointsize 36 -annotate +50+68 '", location, " ", ystart, "' ", file.path(md, fname)))
}

## continue with the next years
for (y in seq(ystart + 1, yend, 1)) {
  ## remove killed trees and establish new ones
  stand = updateStand(stand, dgvm3d.succession[[location]], y)
  ## remove the old vegetation
  if (nrow(rgl.ids()) > (nsoillayer))
    rgl.pop(id=rgl.ids()[(nsoillayer + 1):nrow(rgl.ids()), "id"])
  #stand3D(stand)
  ## first calculate image, then draw it. Speeds up everything by several orders of magnitude
  par3d(skipRedraw=TRUE)
  stand = plant3D(stand)
  fire3D(stand, limit=0.2)
  par3d(skipRedraw=FALSE)

  ## continue rotation around the z-axis
  for (i in 1:ppy) {
    rot.z = rotationMatrix(((y - ystart) * ppy + i) * pi * apy / 360, 0, 0, 1)
    rgl.viewpoint(userMatrix = rot.y %*% rot.z, fov=1, zoom=0.75)
    ## save the image and add a label
    fname = sprintf("movie%08d.png", (y - ystart) * ppy + i)
    rgl.snapshot(file.path(md, fname))
    system(paste0("mogrify -fill black -pointsize 36 -annotate +50+68 '", location, " ", y, "' ", file.path(md, fname)))
  }
}
## convert the images to a movie
owd = getwd()
setwd(md)
system("ffmpeg -y -f image2 -r 25 -i movie%08d.png -vcodec libx264 -pix_fmt yuv420p dgvm3d.mp4")
unlink(list.files(pattern="movie[0-9]{8}.png"))
message(file.path(md, "dgvm3d.mp4"))
setwd(owd)
