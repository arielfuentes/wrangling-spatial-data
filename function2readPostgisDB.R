qsql <- "SELECT gid, 
area,
area1,
zona,
zona_corr,
macroarea_,
macroarea,
comuna_id,
comuna, ST_AsText(geom) AS wkt_geometry from zonas_estraus"
geo_db <- function(qsql){
  require(RPostgreSQL)
  con <- dbConnect("PostgreSQL", host = "10.222.128.87", user= "postgres", password="estudios2015..", 
                   dbname = "geo")
  geo_db <- dbGetQuery(con, qsql)
  # dbDisconnect(con)
}
shape.df <- as.data.frame(geo_db(qsql))
SPolygons <- function(wkt, p4ss){
  require(rgeos)
  require(sp)
  list_geom <- lapply(wkt, "readWKT", p4s=p4ss)
  coords <- mapply(spChFIDs, list_geom, as.character(shape.df[,1]))
  SPolygonsdf <- SpatialPolygonsDataFrame(SpatialPolygons(unlist(lapply(coords, 
                                                                 function(x) x@polygons)),
                                                   proj4string=p4ss), 
                                   shape.df[, -ncol(shape.df)])
  SPolygonsdf
}

estraus <- SPolygons(wkt = shape.df$wkt_geometry, p4ss = CRS("+proj=longlat +datum=WGS84"))
