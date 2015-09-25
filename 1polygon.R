library(rgeos)
library(sp)
qsql <- "SELECT gid, 
 ST_AsText(geom) AS wkt_geometry from disolve_zones"
geo_db <- function(qsql){
  require(RPostgreSQL)
  con <- dbConnect("PostgreSQL", host = "10.222.128.87", user= "postgres", password="estudios2015..", 
                   dbname = "geo")
  geo_db <- dbGetQuery(con, qsql)
}
shape.df <- as.data.frame(geo_db(qsql))
list_geom <- lapply(shape.df$wkt_geometry, "readWKT", p4s=CRS("+proj=longlat +datum=WGS84"))
coords <- mapply(spChFIDs, list_geom, as.character(shape.df[,1]))
S1Polygon <- function(wkt, p4ss){
  require(rgeos)
  require(sp)
  list_geom <- lapply(wkt, "readWKT", p4s=p4ss)
  coords <- mapply(spChFIDs, list_geom, as.character(shape.df[,1]))
  SPolygon <- SpatialPolygons(unlist(lapply(coords, function(x) x@polygons)),
                                                          proj4string=p4ss)
  SPolygon
}
buffer <- S1Polygon(wkt = shape.df$wkt_geometry, p4ss = CRS("+proj=longlat +datum=WGS84"))
