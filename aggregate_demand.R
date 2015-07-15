library(dplyr)
#library(readxl)
library(sp)
library(rgdal)

stops <- read.csv("stopsubtb0413.csv", header = T, sep = ";")
stops <- stops[c(-1, -nrow(stops)), 1:3]
row.names(stops) <- as.numeric(row.names(stops))-1
stops.sp <- SpatialPointsDataFrame(cbind(stops$x_subida, stops$y_subida), 
                                   as.data.frame(stops[, 1]))
proj4string(obj = stops.sp) <- CRS("+init=epsg:32719")
zonas <- readOGR(dsn = ".", layer = "zonas estraus", p4s = "+proj=longlat")
zonas_m <- spTransform(zonas, CRS("+init=epsg:32719"))
# zonas_spol <- SpatialPolygons(zonas_m@polygons, proj4string = zonas_m@proj4string)
# x1 <- zonas@data$COMUNA == "SANTIAGO"
# ID1 <- zonas[zonas@data$ID == '1', ]
# plot(zonas[x1, ])
TRX_zona <- over(zonas_m, stops.sp, fn = sum)
colnames(TRX_zona) <- "sube"
TRX_zona2 <- add_rownames(TRX_zona, var = "rowname")
# k <- as.data.frame(sapply(over(zonas_m, geometry(stops.sp), returnList = TRUE), length))
zonas_m_data <- add_rownames(zonas_m@data, var = "rowname")
zonas_m_data <- left_join(zonas_m_data, TRX_zona2, "rowname")
zonas_m@data <- left_join(zonas_m@data, zonas_m_data)

writeOGR(obj = zonas_m, dsn = ".", layer = "TRX_estraus", 
         driver = "ESRI Shapefile")
