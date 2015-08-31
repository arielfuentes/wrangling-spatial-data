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
for (i in seq(nrow(shape.df))) {
  require(rgeos)
  if (i == 1) {
    shape.sp = readWKT(shape.df$wkt_geometry[i], shape.df$gid[i])
  }
  else {
    shape.sp = rbind(
      shape.sp, readWKT(shape.df$wkt_geometry[i], shape.df$gid[i])
    )
  }
}

shape.polygon <- SpatialPolygonsDataFrame(shape.sp, shape.df[, -ncol(shape.df)])
shape.line <- SpatialLinesDataFrame(shape.sp, shape.df[, -ncol(shape.df)])
shape.point <- SpatialPointsDataFrame(shape.sp, shape.df[, -ncol(shape.df)])
