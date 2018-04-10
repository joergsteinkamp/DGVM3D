library(DGVM3D)
stand = initStand()
open3d(windowRect=c(0, 0, 800, 600), scale=c(1, 1, 1), FOV=0)
stand3D(stand)

rgl.clear("lights")
rgl.light( theta = -25, phi = 30, specular = "black", diffuse = "#FFFFFF")
axis3d("z", pos=c(-stand@hexagon@supp$outer.radius, 5*stand@hexagon@supp$inner.radius, NA))
rot.z = rotationMatrix(pi/6, 0, 0, 1)
rot.y = rotationMatrix(-pi/3, 1, 0, 0)
rgl.viewpoint(userMatrix = rot.y %*% rot.z, fov=1)

veg = data.frame(DBH=rep(0.45, 4))
veg$Height    = veg$DBH * 45
veg$Crownarea = veg$DBH * 25
veg$LeafType  = rep(1:2, each=2)
veg$ShadeType = rep(1:2, 2)
veg$LAI = rep(2, nrow(veg))
veg = rbind(veg, data.frame(DBH=-1, Height=-1, Crownarea=-1, LeafType=-1, ShadeType=3, LAI=0.5))

dgvm3d.options(establish.method = "random")
set.seed(65432)
stand@patches[[1]]@vegetation = establishTrees(veg, stand@hexagon@supp[['inner.radius']])
stand = plant3D(stand)
rgl.texts(0, 0, 30, "a")

par3d(ignoreExtent=TRUE)

rgl.snapshot("dgvm3d_09.png")
rgl.snapshot("dgvm3d_08.png")
rgl.snapshot("dgvm3d_07.png")

rgl.pop(id=rgl.ids()[c(12, 10), "id"])
rgl.snapshot("dgvm3d_06.png")
rgl.snapshot("dgvm3d_05.png")

rgl.pop(id=rgl.ids()[c(6, 8), "id"])
rgl.snapshot("dgvm3d_04.png")
rgl.snapshot("dgvm3d_03.png")

rgl.pop(id=rgl.ids()[5:8, "id"])
rgl.snapshot("dgvm3d_02.png")
rgl.snapshot("dgvm3d_01.png")

system("ffmpeg -y -f image2 -r 2 -i dgvm3d_%02d.png -vcodec libx264 -pix_fmt yuv420p dgvm3d.mp4")
## system("convert -loop 0 -delay 50 -transparent white dgvm3d_0* dgvm3d.gif")
unlink(list.files(pattern="dgvm3d_[0-9]{2}.png"))

########################
### tree arrangement ###
########################

veg = data.frame(DBH=rep(0.05, 250))
veg$Height    = veg$DBH * 35
veg$Crownarea = veg$DBH * 5
veg$LeafType  = sample(1:2, nrow(veg), replace=TRUE)
veg$ShadeType = sample(1:2, nrow(veg), replace=TRUE)
veg$LAI = rep(2, nrow(veg))
veg = rbind(veg, data.frame(DBH=-1, Height=-1, Crownarea=-1, LeafType=-1, ShadeType=3, LAI=0.5))
stand = initStand(npatch=3)

open3d(windowRect=c(0, 0, 800, 600), scale=c(1, 1, 1), FOV=0)
par3d(skipRedraw=TRUE)

stand3D(stand, 1)
dgvm3d.options(establish.method = "row")
stand@patches[[1]]@vegetation = establishTrees(veg, stand@hexagon@supp[['inner.radius']])
stand = plant3D(stand, 1)

stand3D(stand, 2)
dgvm3d.options(establish.method = "sunflower")
stand@patches[[2]]@vegetation = establishTrees(veg, stand@hexagon@supp[['inner.radius']])
stand = plant3D(stand, 2)

stand3D(stand, 3)
dgvm3d.options(establish.method = "random")
stand@patches[[3]]@vegetation = establishTrees(veg, stand@hexagon@supp[['inner.radius']])
stand = plant3D(stand, 3)

par3d(skipRedraw=FALSE)
rot.z = rotationMatrix(2*pi/3, 0, 0, 1)
rot.y = rotationMatrix(-pi/8, 1, 0, 0)
rgl.viewpoint(userMatrix = rot.y %*% rot.z, fov=1)
snapshot3d("stand_3.png")

###########
## Fire ###
###########

stand=initStand()
open3d(windowRect=c(0, 0, 800, 600), scale=c(1, 1, 1), FOV=0)
stand3D(stand)
stand@patches[[1]]@vegetation = establishTrees(veg, stand@hexagon@supp[['inner.radius']])
#
rgl.clear("lights")
rgl.light( theta = -25, phi = 30, specular = "black", diffuse = "#FFFFFF")
rot.y = rotationMatrix(-pi/3, 1, 0, 0)
rot.z = rotationMatrix(0, 0, 0, 1)
rgl.viewpoint(userMatrix = rot.y %*% rot.z, fov=1)

for (i in seq(1, 360, 3)) {
set.seed(123456)
   stand@patches[[1]]@vegetation$Fireprob=0.1 + i / 400
   stand3D(stand)
   axis3d("z", pos=c(-stand@hexagon@supp$outer.radius, 5*stand@hexagon@supp$inner.radius, NA))
   #rot.z = rotationMatrix(i * pi/360, 0, 0, 1)
   #rgl.viewpoint(userMatrix = rot.y %*% rot.z, fov=1)

   text3d(stand@hexagon@supp$outer.radius * c(1.0, 1.0, -1.0, -1.0, 0),
             stand@hexagon@supp$outer.radius * c(0, 2, 0, 2, 1),
             c(0, 0, 0, 0, 5), "a", alpha=0)

   fire3D(stand, limit=0)
   snapshot3d(sprintf("fire_%03i.png", (i-1)/3))
   rgl.clear()
 }

system("ffmpeg -y -f image2 -r 30 -i fire_%03d.png -vcodec libx264 -pix_fmt yuv420p fire.mp4")
unlink(list.files(pattern="fire_[0-9]{3}.png"))

