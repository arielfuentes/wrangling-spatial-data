#############################################################
#libraries
library(RPostgreSQL)
library(sp)
library(rgeos)
############################################################
#query
m <- dbDriver("PostgreSQL")
con <- dbConnect(m, host = "localhost", user= "postgres", password="admin", 
                 dbname = "geo")
dbListConnections(m)
dbGetInfo(con)
summary(con)
dbListTables(con)
dbExistsTable(con, "zonas_estraus")

z_estraus <- dbGetQuery(con, "SELECT gid,
                        area,
                        area1,
                        zona,
                        zona_corr,
                        macroarea_,
                        macroarea,
                        comuna_id,
                        comuna, ST_AsText(geom) AS wkt_geometry from zonas_estraus")
#####################################################################
#spatial data frame
row.names(z_estraus$gid)
z_estraus <- as.data.frame(z_estraus)
dbDisconnect(con)

for (i in seq(nrow(z_estraus))) {
  if (i == 1) {
    estraus.sp = readWKT(z_estraus$wkt_geometry[i], z_estraus$gid[i])
    # If the PROJ4 string has been set, use the following instead
    # spTemp = readWKT(dfTemp$wkt_geometry[i], dfTemp$gid[i], p4s)
  }
  else {
    estraus.sp = rbind(
      estraus.sp, readWKT(z_estraus$wkt_geometry[i], z_estraus$gid[i])
      # If the PROJ4 string has been set, use the following instead
      # spTemp, readWKT(dfTemp$wkt_geometry[i], dfTemp$gid[i], p4s)
    )
  }
}

estraus.sp.df = SpatialPolygonsDataFrame(estraus.sp, z_estraus[-10])

###########################################################
#projection
proj4string(obj = estraus.sp.df) <- CRS("+proj=longlat +datum=WGS84")
summary(estraus.sp.df)
