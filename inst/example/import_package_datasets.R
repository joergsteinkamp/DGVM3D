if("package:DGVM3D" %in% search()) detach(name = "package:DGVM3D", unload = TRUE)
library(DGVM3D)

### Example how to import LPJ-GUESS data
dgvm3d.locations = read.table("/data/Dropbox.WIP/Establishment/output/default/gridlist.txt",
                              col.names=c("Lon", "Lat", "Name"), sep="\t",
                              stringsAsFactors=FALSE)
dgvm3d.succession=list()
for (i in 1:nrow(dgvm3d.locations)) {
  dgvm3d.succession[[dgvm3d.locations$Name[i]]] =
    read.LPJ("/data/Dropbox.WIP/Establishment/output/default/vegstruct.out",
             lon=dgvm3d.locations$Lon[i],
             lat=dgvm3d.locations$Lat[i])
  dgvm3d.succession[[i]] = dgvm3d.succession[[i]][!(dgvm3d.succession[[i]]$Year %% 5) &
                                                    dgvm3d.succession[[i]]$Year > 1859, ]
}

dgvm3d.locations = dgvm3d.locations[1:13, ]
for (i in 19:14)
  dgvm3d.succession[[i]] = NULL
save(dgvm3d.locations, file="~/Cloud/Dropbox/src/DGVM3D/data/dgvm3d.locations.RData", compress="xz")
save(dgvm3d.succession, file="~/Cloud/Dropbox/src/DGVM3D/data/dgvm3d.succession.RData", compress="xz")
