qsql <- "SELECT gid, 
 id,
  area,
  zona,
  com,
  ST_AsText(geom) AS wkt_geometry from centeodlatlon"
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

SLines <- function(wkt, p4ss){
  require(rgeos)
  require(sp)
  list_geom <- lapply(wkt, "readWKT", p4s=p4ss)
  coords <- mapply(spChFIDs, list_geom, as.character(shape.df[,1]))
  SLinesdf <- SpatialLinesDataFrame(SpatialLines(unlist(lapply(coords, function(x) x@lines)),
                                                          proj4string=p4ss), 
                                          shape.df[, -ncol(shape.df)])
  SLinesdf
}

SPoints <- function(wkt, p4ss){
  require(rgeos)
  require(sp)
  list_geom <- lapply(wkt, "readWKT", p4s=p4ss)
  SPointsdf <- SpatialPointsDataFrame(SpatialPoints(do.call(rbind, lapply(list_geom, data.frame, 
                                                                          stringsAsFactors=FALSE)),
                                                    proj4string = p4ss), 
                                      shape.df[, -ncol(shape.df)])
  SPointsdf
}
polygons.shp <- SPolygons(wkt = shape.df$wkt_geometry, p4ss = CRS("+proj=longlat +datum=WGS84"))
lines.shp <- SLines(wkt = shape.df$wkt_geometry, p4ss = CRS("+proj=longlat +datum=WGS84"))
points.shp <- SPoints(wkt = shape.df$wkt_geometry, p4ss = CRS("+proj=longlat +datum=WGS84"))
