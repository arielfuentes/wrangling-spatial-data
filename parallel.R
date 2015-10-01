readstops <- function(file2read, delim, f_1, f_2){
  require(data.table)
  require(dplyr)
#   require(sp)
#   require(rgdal)
  stops <- fread(input = file2read, sep = delim, header = T)
  stops <- stops[complete.cases(stops),]
  stops <- filter(.data = stops, 
                  fecha != f_1 & fecha != f_2)
#   stops.sp <- SpatialPointsDataFrame(cbind(stops$x_subida, stops$y_subida), 
#                                      select(.data = stops, count))
#   proj4string(obj = stops.sp) <- CRS("+init=epsg:32719")
#   stops.sp <- spTransform(stops.sp, CRS("+proj=longlat +datum=WGS84"))
#   stops.sp
}
subtb812 <- readstops(file2read = "bipeosdia.csv", delim = ";", 
                      f_1 = "2012-04-21", f_2 = "2012-04-22")
library(doParallel)
library(dplyr)
cl <- makeCluster(detectCores()) 
registerDoParallel(cl)
clusterExport(cl,ls())
clusterEvalQ(cl, library(sp))
ID.Split<-clusterSplit(cl,unique(subtb812$fecha))
a<-function(x) SpatialPointsDataFrame(cbind(subtb812$x_subida, subtb812$y_subida), 
                                      select(.data = subtb812, count))
system.time(m<-clusterApply(cl, x=ID.Split, fun=a))
