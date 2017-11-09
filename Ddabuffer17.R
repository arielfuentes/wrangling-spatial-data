library(sp)
library(rgdal)
library(rgeos)
library(dplyr)
Dda <- read.csv("dda_par2017.csv", sep = ";")
row.names(Dda) <- as.numeric(row.names(Dda))-1
Dda.sp <- SpatialPointsDataFrame(cbind(Dda$x, Dda$y), as.data.frame(Dda[, 1]))
proj4string(obj = Dda.sp) <- CRS("+init=epsg:32719")
Rutas_ss <- readOGR(dsn = ".", layer = "rutasALicitar", p4s = "+proj=longlat")
rutas_ss <- spTransform(Rutas_ss, CRS("+init=epsg:32719"))
##################################################################################
f <- list()
for (i in 1:nrow(rutas_ss)){
  f[[i]] <- over(gBuffer(rutas_ss[i, ], width = 300), Dda.sp, fn = sum, returnList = F)
}
df <- do.call(rbind.data.frame, f)
df$rownames <- seq(1, 413, by = 1)
rutas_ss@data$rownames <- seq(1, 413, by = 1)
Ddda17ss <- left_join(df, rutas_ss@data)
write.csv(x = Ddda17ss, file = "Ddda_buffer17.csv", row.names = F,quote = F)
