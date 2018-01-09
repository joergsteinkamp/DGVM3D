## ---- eval=FALSE---------------------------------------------------------
#  dgvm3d.locations = read.table("gridlist.txt",
#                                col.names=c("Lon", "Lat", "Name"), sep="\t",
#                                stringsAsFactors=FALSE)
#  dgvm3d.succession=list()
#  for (i in 1:nrow(dgvm3d.locations)) {
#    dgvm3d.succession[[dgvm3d.locations$Name[i]]] =
#    read.LPJ("vegstruct.out",
#             lon=dgvm3d.locations$Lon[i],
#             lat=dgvm3d.locations$Lat[i])
#    dgvm3d.succession[[i]] = dgvm3d.succession[[i]][!(dgvm3d.succession[[i]]$Year %% 5) &
#                                                    dgvm3d.succession[[i]]$Year > 1859, ]
#  }

## ---- message=FALSE------------------------------------------------------
library(DGVM3D)
stand = initStand()
stand3D(stand)
snapshot3d("stand_1x1.png")

## ------------------------------------------------------------------------
stand = initStand(npatch=2)
stand3D(stand, 1)
veg = data.frame(DBH=rep(0.4, 50))
veg$Height    = veg$DBH * 35
veg$Crownarea = veg$DBH * 5
veg$LeafType  = sample(1:2, nrow(veg), replace=TRUE)
veg$ShadeType = sample(1:2, nrow(veg), replace=TRUE)
veg$LAI = rep(2, nrow(veg))
veg = rbind(veg, data.frame(DBH=-1, Height=-1, Crownarea=-1, LeafType=-1, ShadeType=3, LAI=0.5))
stand@patches[[1]]@vegetation = establishTrees(veg, stand@hexagon@supp[['inner.radius']])
stand = plant3D(stand)

stand3D(stand, 2)
veg = data.frame(DBH=rep(0.5, 100) * rgamma(100, 2.5, 9))
veg$Height    = veg$DBH * 35  * rbeta(nrow(veg),10,1)
veg$Crownarea = veg$DBH * 5 * rnorm(nrow(veg), 1, 0.1)
veg$LeafType  = sample(1:2, nrow(veg), replace=TRUE)
veg$ShadeType = sample(1:2, nrow(veg), replace=TRUE)
veg$LAI = rep(2, nrow(veg))
veg = rbind(veg, data.frame(DBH=-1, Height=-1, Crownarea=-1, LeafType=-1, ShadeType=-1, LAI=1.5))
stand@patches[[2]]@vegetation = establishTrees(veg, stand@hexagon@supp[['inner.radius']])
stand = plant3D(stand, 2)
snapshot3d("stand_1x2.png")

