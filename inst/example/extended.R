if("package:DGVM3D" %in% search()) detach(name = "package:DGVM3D", unload = TRUE)
library(DGVM3D)

data("dgvm3d.locations")
succ=list()
for (i in 1:nrow(dgvm3d.locations)) {
  succ[[i]] = read.LPJ("~/WIP/Establishment/output/disturb2/vegstruct.out",
                       lon=dgvm3d.locations$Lon[i],
                       lat=dgvm3d.locations$Lat[i])
}


open3d(windowRect=c(10, 10, 1280, 1024), scale=c(1, 1, 1), FOV=0)
stand = snapshot(succ[[i]])
rgl.clear("lights")
rgl.light( theta = -25, phi = 30, specular = "#AAAAAA")
#axis3d("z", pos=c(stand@hexagon@supp$outer.radius*0.525, -0.25, NA))
axis3d("z", pos=c(-stand@hexagon@supp$outer.radius, 5*stand@hexagon@supp$inner.radius, NA))
##  axis3d("z", pos=c(stand@hexagon@supp$outer.radius*4.15, stand@hexagon@supp$inner.radius * 5.2, NA))
##  title3d(main =paste0("Chile (", dgvm3d.locations$Lon[i], "/", dgvm3d.locations$Lat[i],")"))
rot.z = rotationMatrix(pi/6, 0, 0, 1)
rot.y = rotationMatrix(-pi/3, 1, 0, 0)
rgl.viewpoint(userMatrix = rot.y %*% rot.z, fov=1)
rgl.snapshot("patch_y2000_loc13.png")


for (i in c(11,13)) {
  print(dgvm3d.locations$Name)
  open3d(windowRect=c(10, 10, 1280, 1024), scale=c(1, 1, 1), FOV=1)
  stand = snapshot(succ[[i]])
  rgl.viewpoint(0, -65, fov=1)
  rgl.clear("lights")
  rgl.light( theta = -25, phi = 30, specular = "#AAAAAA")
  axis3d("z", pos=c(stand@hexagon@supp$outer.radius*0.525, -0.25, NA))
##  axis3d("z", pos=c(stand@hexagon@supp$outer.radius*4.15, stand@hexagon@supp$inner.radius * 5.2, NA))
##  title3d(main =paste0("Chile (", dgvm3d.locations$Lon[i], "/", dgvm3d.locations$Lat[i],")"))

  movie3d(spin3d(axis = c(0, 0, 1), rpm = 1), 60, dir=tempdir(), convert=FALSE, clean=FALSE)
  rgl.close()
  system(paste0("ffmpeg -y -i ",tempdir(),"/movie%03d.png -s sxga -r ntsc -vcodec libx264 -pix_fmt yuv420p patch_y2000_loc",i,".mp4"))
  system(paste0("rm ",tempdir(),"/movie*.png"))

  succ.ts = succession(succ[[i]], init.year=1860, patch.id=c(7,9,10), years=seq(1865, 2005, 20))

  open3d(windowRect=c(100, 100, 1280, 1024), scale=c(1, 1, 1), FOV=1)
  stand3D(succ.ts)
  succ.ts=plant3D(succ.ts)
  rgl.clear("lights")
  rgl.light( theta = -25, phi = 30, specular = "#AAAAAA")

  rot.z = rotationMatrix(-pi/2, 0, 0, 1)
  rot.y = rotationMatrix(-pi/2.5, 0, 1, 0)
  rgl.viewpoint(userMatrix = rot.z %*% rot.y, fov=1)
  #  title3d(main =paste0(dgvm3d.locations$Name[i], " (", dgvm3d.locations$Lon[i], "/", dgvm3d.locations$Lat[i],")"))
  axis3d("z", pos=c(stand@hexagon@supp$outer.radius*0.525, -0.25, NA))

  movie3d(spin3d(axis = c(0, 0, 1), rpm = 1), 60, dir=tempdir(), convert=FALSE, clean=FALSE)
  rgl.close()
  system(paste0("ffmpeg -y -i ",tempdir(),"/movie%03d.png -s sxga -r ntsc -vcodec libx264 -pix_fmt yuv420p succ_loc",i,".mp4"))
  system(paste0("rm ",tempdir(),"/movie*.png"))
}

open3d(windowRect=c(100, 100, 1280, 1024), scale=c(1, 1, 1), FOV=1)
stand3D(succ.ts)
succ.ts = plant3D(succ.ts)
rgl.clear("lights")
rgl.light( theta = -25, phi = 30, specular = "#AAAAAA")

rot.z = rotationMatrix(-pi/2, 0, 0, 1)
rot.y = rotationMatrix(-pi/4, 0, 1, 0)
rgl.viewpoint(userMatrix = rot.z %*% rot.y, fov=1)
#  title3d(main =paste0(dgvm3d.locations$Name[i], " (", dgvm3d.locations$Lon[i], "/", dgvm3d.locations$Lat[i],")"))
axis3d("z", pos=c(stand@hexagon@supp$outer.radius*0.525, -0.25, NA))
snapshot3d("succ_loc11.png")



library(plyr)
library(ggplot2)
dens=ddply(dgvm3d.succession, .(Year, PID), summarise, dens=10*sum(N))
mn.dens=ddply(dens, .(Year), summarise, mn.dens=mean(dens))
dens$PID = as.factor(dens$PID)
ggplot(dens, aes(x=Year, y=dens)) + geom_line(aes(col=PID)) + geom_line(data=mn.dens, aes(y=mn.dens), size=2)


