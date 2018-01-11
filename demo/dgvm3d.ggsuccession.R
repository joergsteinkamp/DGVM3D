library(DGVM3D)

if (!require(data.table) || !require(ggplot2)) {
  warning("You need the packages 'data.table' and 'ggplot2' installed, to run this demo!")
} else {
  location <- 'Canada - boreal forest'
  dt <- data.table(dgvm3d.succession[[location]])
  lon <- unique(dt$Lon)
  lat <- unique(dt$Lat)
  ## mean over individuums
  dt = dt[, list(LAI=mean(LAI, na.rm=TRUE)), by=c("Year", "VID", "PID", "Pft")]
  ## sum over PFTs
  dt = dt[, list(LAI=sum(LAI, na.rm=TRUE)), by=c("Year", "PID", "Pft")]
  ## fill missing values with zeros
  full.dt <- expand.grid(Year=unique(dt$Year), PID=unique(dt$PID), Pft=unique(dt$Pft))
  dt = merge(dt, full.dt, by=c("Year" , "PID", "Pft"), all = TRUE)
  dt[is.na(LAI), LAI:=0]
  gg <- ggplot(dt, aes(Year, LAI, group=Pft, fill=Pft))
  gg = gg + geom_area(position="stack")
  gg = gg + facet_wrap(~PID, ncol=2)
  gg = gg + labs(title=paste0(location, " (", lon, "/", lat, ")"))
  print(gg)

  dt = dt[, list(LAI=mean(LAI)), by=c("Year", "Pft")]
  gg <- ggplot(dt, aes(Year, LAI, group=Pft, fill=Pft))
  gg = gg + geom_area(position="stack")
  gg = gg + labs(title=paste0(location, " (", lon, "/", lat, ")"))
  print(gg)

  location <- "Africa - sahel"
  dt <- data.table(dgvm3d.succession[[location]])
  lon <- unique(dt$Lon)
  lat <- unique(dt$Lat)
  ## mean over individuums
  dt = dt[, list(LAI=mean(LAI, na.rm=TRUE)), by=c("Year", "VID", "PID", "Pft")]
  ## sum over cohorts
  dt = dt[, list(LAI=sum(LAI, na.rm=TRUE)), by=c("Year", "PID", "Pft")]
  ## fill missing values with zeros
  full.dt = expand.grid(Year=unique(dt$Year), PID=unique(dt$PID), Pft=unique(dt$Pft))
  dt = merge(dt, full.dt, by=c("Year" , "PID", "Pft"), all = TRUE)
  dt[is.na(LAI), LAI:=0]
  dt = dt[, list(LAI=mean(LAI)), by=c("Year", "Pft")]
  gg <- ggplot(dt, aes(Year, LAI, group=Pft, fill=Pft))
  gg = gg + geom_area(position="stack")
  gg = gg + labs(title=paste0(location, " (", lon, "/", lat, ")"))
  print(gg)
}
