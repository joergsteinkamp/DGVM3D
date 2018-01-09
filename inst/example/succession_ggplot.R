if("package:DGVM3D" %in% search()) detach(name = "package:DGVM3D", unload = TRUE)
library(DGVM3D)


library(data.table)
library(ggplot2)
for (i in c(1, 7)) {
  dt <- data.table(dgvm3d.succession[[i]])
  ## mean over individuums
  dt = dt[, list(LAI=mean(LAI, na.rm=TRUE)), by=c("Year", "VID", "PID", "Pft")]
  ## sum over PFTs
  dt = dt[, list(LAI=sum(LAI, na.rm=TRUE)), by=c("Year", "PID", "Pft")]
  ## fill missing values with zeros
  full.dt = expand.grid(Year=unique(dt$Year), PID=unique(dt$PID), Pft=unique(dt$Pft))
  dt = merge(dt, full.dt, by=c("Year" , "PID", "Pft"), all = TRUE)
  dt[is.na(LAI), LAI:=0]

  gg <- ggplot(dt, aes(Year, LAI, group=Pft, fill=Pft))
  gg = gg + DGVMTools::dgvm.ggplot.theme("timeseries")
  gg = gg + geom_area(position="stack")
  gg = gg + facet_wrap(~PID, ncol=2)
  gg = gg + labs(title=paste0(dgvm3d.locations$Name[i], " (", dgvm3d.locations$Lon[i], "/", dgvm3d.locations$Lat[i], ")"))
  print(gg)

  dt = dt[, list(LAI=mean(LAI)), by=c("Year", "Pft")]
  gg <- ggplot(dt, aes(Year, LAI, group=Pft, fill=Pft))
  gg = gg + DGVMTools::dgvm.ggplot.theme("timeseries")
  gg = gg + geom_area(position="stack")
  gg = gg + labs(title=paste0(dgvm3d.locations$Name[i], " (", dgvm3d.locations$Lon[i], "/", dgvm3d.locations$Lat[i], ")"))
  print(gg)
}
