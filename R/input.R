#' Prepare the output table from LPJ-GUESS for visualization
#'
#' Stand ID and Patch ID start counting at 0 in the standard output. Here the value of 1 is added, to be consistent with R.
#'
#' @param file the filename to be read
#' @param stand.id the stand ID default to 0.
#' @param patch.id if a single patch should be used (default all)
#' @param year if a single year should be used (default all)
#' @param lon if a single longitude should be used (default all). Should be defined, if more than one gridpoint is in the output.
#' @param lat as above
#' @param grass should grasses be included (so far they are not yet further processed).
#' @return individual vegetation data.frame with equal indivuduals from each cohort.
#' @importFrom utils read.table
#' @export
#' @author Joerg Steinkamp \email{steinkamp.joerg@@gmail.com}
read.LPJ <- function(file="vegstruct.out", stand.id=1, patch.id=NULL, year=NULL, lon=NULL, lat=NULL, grass=FALSE) {
  ## location.names <- read.table("/Users/jsteinkamp/WIP/Establishment/output/gridlist.txt", sep="\t", col.names=c("Lon", "Lat", "Name"))
  ## file = "/Users/jsteinkamp/WIP/Establishment/output/vegstruct.out"
  SID=PID=Year=Lon=Lat=Lifeform=NULL
  vegstruct <- read.table(file, header=TRUE)
  vegstruct$SID =   vegstruct$SID + 1
  vegstruct$PID =   vegstruct$PID + 1
  ## apply the filters
  vegstruct <- subset(vegstruct, SID==stand.id)
  if (!is.null(patch.id))
    vegstruct <- subset(vegstruct, PID==patch.id)
  if (!is.null(year))
    vegstruct <- subset(vegstruct, Year==year)
  if (!is.null(lon))
    vegstruct <- subset(vegstruct, Lon==lon)
  if (!is.null(lat))
    vegstruct <- subset(vegstruct, Lat==lat)
  if (!grass)
    vegstruct <- subset(vegstruct, Lifeform != 2)

  if (length(unique(vegstruct$Lon))>1 || length(unique(vegstruct$Lat))>1)
    warning("Several locations are defined. Sure that's what you want?")

  ## vegstruct$Lifeform[vegstruct$Lifeform==1] = "tree"
  ## vegstruct$Lifeform[vegstruct$Lifeform==2] = "grass"
  ##
  ## vegstruct$LeafType[vegstruct$LeafType==1] = "needle"
  ## vegstruct$LeafType[vegstruct$LeafType==2] = "broadleaved"
  ##
  ## vegstruct$PhenType[vegstruct$PhenType==1] = "evergreen"
  ## vegstruct$PhenType[vegstruct$PhenType==1] = "raingreen"
  ## vegstruct$PhenType[vegstruct$PhenType==1] = "summergreen"
  ## vegstruct$PhenType[vegstruct$PhenType==1] = "any"
  ##
  ## vegstruct$Pathway[vegstruct$Pathway==1] = "C3"
  ## vegstruct$Pathway[vegstruct$Pathway==2] = "C4"
  ##
  vegstruct$ShadeType = gapless.rank(vegstruct$ShadeType)
  ## if (max(vegstruct$ShadeType)==2) {
  ##   vegstruct$ShadeType[vegstruct$ShadeType==1] = "tolerant"
  ##   vegstruct$ShadeType[vegstruct$ShadeType==2] = "intolerant"
  ## } else if (max(vegstruct$ShadeType==3)) {
  ##   vegstruct$ShadeType[vegstruct$ShadeType==1] = "tolerant"
  ##   vegstruct$ShadeType[vegstruct$ShadeType==2] = "intermediate"
  ##   vegstruct$ShadeType[vegstruct$ShadeType==3] = "intolerant"
  ## }

  ## from cohort to individual trees
  veg = vegstruct[rep(seq_len(nrow(vegstruct)), times=vegstruct$N), ]
  veg$N = 1

  return(veg)
}


