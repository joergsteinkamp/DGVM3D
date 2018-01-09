if("package:DGVM3D" %in% search()) detach(name = "package:DGVM3D", unload = TRUE)
library(DGVM3D)






for (y in c(1865, 1900, 2000)) {
open3d(windowRect=c(10, 10, 1280, 1024), scale=c(1, 1, 1), FOV=0)
stand = snapshot(dgvm3d.succession[[1]], year=y)
rgl.clear("lights")
rgl.light( theta = -25, phi = 30, specular = "black", diffuse = "#FFFFFF")
#axis3d("z", pos=c(stand@hexagon@supp$outer.radius*0.525, -0.25, NA))
axis3d("z", pos=c(-stand@hexagon@supp$outer.radius, 5*stand@hexagon@supp$inner.radius, NA))
##  axis3d("z", pos=c(stand@hexagon@supp$outer.radius*4.15, stand@hexagon@supp$inner.radius * 5.2, NA))
##  title3d(main =paste0("Chile (", dgvm3d.locations$Lon[i], "/", dgvm3d.locations$Lat[i],")"))
rot.z = rotationMatrix(pi/6, 0, 0, 1)
rot.y = rotationMatrix(-pi/3, 1, 0, 0)
rgl.viewpoint(userMatrix = rot.y %*% rot.z, fov=1)
## rgl.snapshot("patch_y2000_loc01.png")


#for (i in 1:nrow(dgvm3d.locations)) {
#  print(i)
#  print(nrow(dgvm3d.succession[[i]][dgvm3d.succession[[i]]$Fireprob > 0.7, c("Year", "PID", "Fireprob")]))
#}
### 6/7/11
#dgvm3d.succession[[10]][dgvm3d.succession[[10]]$Fireprob > 0.8, c("Year", "PID", "Fireprob")]

open3d(windowRect=c(100, 100, 1280, 1024), scale=c(1, 1, 1), FOV=1)
stand=initStand()
stand3D(stand)
stand@patches[[1]]@vegetation = data.frame(Fireprob=1)
fire3D(stand)
rgl.clear("lights")
rgl.light( theta = -25, phi = 30, specular = "#AAAAAA")

open3d(windowRect=c(100, 100, 1280, 1024), scale=c(1, 1, 1), FOV=1)
stand=snapshot(dgvm3d.succession[[8]], patch.id = 4, year = 1905)
fire3D(stand)

rgl.clear("lights")
rgl.light( theta = -25, phi = 30, specular = "#AAAAAA")

rot.z = rotationMatrix(-pi/2, 0, 0, 1)
rot.y = rotationMatrix(-pi/4, 0, 1, 0)
rgl.viewpoint(userMatrix = rot.z %*% rot.y, fov=1)
#  title3d(main =paste0(dgvm3d.locations$Name[i], " (", dgvm3d.locations$Lon[i], "/", dgvm3d.locations$Lat[i],")"))
axis3d("z", pos=c(stand@hexagon@supp$outer.radius*0.525, -0.25, NA))
## snapshot3d("succ_loc11.png")





open3d(windowRect=c(100, 100, 1280, 1024), scale=c(1, 1, 1), FOV=1)
succession(dgvm3d.succession[[7]], patch.id = c(2, 6, 10), init.year = 1860, years = seq(1905, 2005, 10))
stand3D(stand)
stand=plant3D(stand)
fire3D(stand)

rgl.clear("lights")
rgl.light( theta = -25, phi = 30, specular = "#AAAAAA")

rot.z = rotationMatrix(-pi/2, 0, 0, 1)
rot.y = rotationMatrix(-pi/4, 0, 1, 0)
rgl.viewpoint(userMatrix = rot.z %*% rot.y, fov=1)
#  title3d(main =paste0(dgvm3d.locations$Name[i], " (", dgvm3d.locations$Lon[i], "/", dgvm3d.locations$Lat[i],")"))
axis3d("z", pos=c(stand@hexagon@supp$outer.radius*0.525, -0.25, NA))
## snapshot3d("succ_loc11.png")







for (i in 1:nrow(dgvm3d.locations)) {
  print(i)
  print(nrow(dgvm3d.succession[[i]][dgvm3d.succession[[i]]$Pft == "C3G" & dgvm3d.succession[[i]]$LAI > 2, c("Year", "PID", "Fireprob")]))
}
# 6/7/11
#dgvm3d.succession[[10]][dgvm3d.succession[[10]]$Fireprob > 0.8, c("Year", "PID", "Fireprob")]
i=6


dgvm3d.succession[[i]][dgvm3d.succession[[i]]$Pft == "C3G" & dgvm3d.succession[[i]]$LAI > 1, c("Year", "PID", "LAI","Fireprob")]

stand=succession(dgvm3d.succession[[6]], patch.id=c(2,12), init.year = 1860)
stand=plant3D(stand)
stand3D(stand)
fire3D(stand)





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


