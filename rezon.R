#################################input
###############libraries
library(rgdal)
library(sp)
##########shapes
nodes <- readOGR(dsn = "input/nodos/Nodos-Red.shp", layer = "Nodos-Red", p4s = "+proj=longlat")
zones <- readOGR(dsn = "input/zonificacion690_sectra/base_zona_estudio_sectra_vf00.shp", 
                 layer = "base_zona_estudio_sectra_vf00")
######Change ofCoordinate Reference System
zones <- spTransform(zones, CRS("+proj=longlat"))
############n.pt must be a pair number
rezon <- function(pg.shp, pt.shp, n.pt, n.tree){
  #libraries
  library(sp)
  library(ClustGeo)
  library(dplyr)
  library(geosphere)
  library(dismo)
  library(rgeos)
  library(reshape2)
  #points per zone attribute
  pg.shp@data$within <- as.vector(cbind(unlist(lapply(sp::over(pg.shp, geometry(pt.shp), 
                                                         returnList = T), length))))
  #unchangeable zones
  pg.un <- pg.shp[pg.shp@data$within < n.pt, ]
  #changeable zones
  pg.ch <- pg.shp[pg.shp@data$within >= n.pt, ]
  #rm 
  rm(pg.shp)
  #elements index
  s = 1:nrow(pg.ch)
  row.names(pg.ch@data) <- c(seq(1:nrow(pg.ch)))
  #spatial query
  sp.qry <- lapply(X = s, FUN = function(x) {
    pt.shp[pg.ch[x, ], ]
  })
  #coordinates extraction
  xy <- lapply(X = sp.qry, FUN = function(x) sp::coordinates(x))
  #Distance Matrix
  d.mtx <- lapply(s, function(i){
    apply(X = xy[[i]], MARGIN = 1, 
          FUN = function(x) sp::spDistsN1(xy[[i]], x, longlat = T))
  })
  #distance formate (lower triangular matrix) 
  d.d.mtx <- lapply(X = d.mtx, FUN = function(x) stats::as.dist(x))
  #rm
  rm(d.mtx)
  #hierarchical clusters 
  tree.l <- lapply(X = d.d.mtx, FUN = function(x) ClustGeo::hclustgeo(D0 = x))
  #rm
  rm(d.d.mtx)
  #number of clusters
  p.l <- lapply(X = tree.l, FUN = function(x) stats::cutree(tree = x, k = n.pt/10))
  #rm
  rm(tree.l)
  #nodes labels
  pt.ll <- lapply(X = sp.qry, FUN = function(x) as.vector(x@data$SIMT))
  #Cluster categories
  Cr <- lapply(X = s, FUN = function(x) data.frame(SIMT = unlist(pt.ll[[x]]), Cr = unlist(p.l[[x]])))
  #rm
  rm(pt.ll, p.l)
  #Adding categories to Spatial list
  for (i in s){
    sp.qry[[i]]@data <- dplyr::left_join(sp.qry[[i]]@data, Cr[[i]])
  }
  #sp.qry2 <- lapply(X = s, FUN = function(x) dplyr::left_join(sp.qry[[x]]@data, Cr[[x]]))
  sp.qry.spt <- lapply(X = s, FUN = function(x) sp::split(sp.qry[[x]], sp.qry[[x]]@data$Cr))
  # sp.qry.ctd <- lapply(X = s, FUN = function(x) sapply(sp.qry.spt[[x]], 
  #                             FUN = function(y) geosphere::centroid(slot(y, "coords"))))
  sp.qry.ct <- lapply(X = s, FUN = function(x) sapply(X = sp.qry.spt[[x]], 
                              FUN = function(y) geosphere::centroid(slot(y, "coords"))))
  sp.qry.ct <- lapply(X = s, FUN = function(x) as.data.frame(t(sp.qry.ct[[x]])))
  for (i in s) {
    names(sp.qry.ct[[i]]) <- c("lon", "lat")
  }
  #rm
  rm(sp.qry.spt)
  #create voronoi polygons 
  sp.qry.vrn <- lapply(X = s, FUN = function(x) dismo::voronoi(sp.qry.ct[[x]], 
                   ext = c(as.vector(t(pg.ch[x]@bbox)))))
  #voronoi projection
  for (i in s) {
    crs(sp.qry.vrn[[i]]) <- "+proj=longlat"
  } 
  sp.qry.vrn
  #rm
  rm(sp.qry.ct)
  #delimited polygons
  sp.qry.itsc <- lapply(X = s, FUN = function(x) rgeos::gIntersection(pg.ch[x,],
                                                                      sp.qry.vrn[[x]], byid = T))
  #delimited spatial polygons dataframe
  sp.qry.itsc <- lapply(X = s, FUN = function(x)
   sp::SpatialPolygonsDataFrame(Sr = sp.qry.itsc[[x]],
                                data = data.frame(ID = row.names(sp.qry.itsc[[x]]), 
                                                  row.names = row.names(sp.qry.itsc[[x]]))))
  # sp.qry.itsc2 <- list()
  # for (i in s){
  #   sp.qry.itsc2[[i]] <- SpatialPolygonsDataFrame(Sr = sp.qry.itsc[[i]],
  #                           data = data.frame(ID = row.names(sp.qry.itsc[[i]]), 
  #                             row.names = row.names(sp.qry.itsc[[i]])))
  # }
  # as polygonal class 
  
  #data frame key
  ESTUDIO <- list()
  for (i in s){
    ESTUDIO[[i]] <- sapply(strsplit(as.character(sp.qry.itsc[[i]]@data$ID), " ", fixed = T), "[", 1)
    sp.qry.itsc[[i]]@data$ESTUDIO <- ESTUDIO[[i]]
  }
  sp.qry.pg <- do.call("rbind", c(args = sp.qry.itsc))
  pg.un@data <- as.data.frame(pg.un@data[,4])
  names(pg.un@data) <- "ESTUDIO"
  sp.qry.pg@data$ID <- NULL
  rezon.spg.df <- sp::rbind.SpatialPolygonsDataFrame(pg.un, sp.qry.pg)
  row.names(rezon.spg.df@data) <- seq(1:lengths(rezon.spg.df@data))
  rezon.spg.df@data$ID <- row.names(rezon.spg.df@data)
  rezon.spg.df
}

x <- rezon(pg.shp = zones, pt.shp = nodes, n.pt = 80)
rgdal::writeOGR(obj = x, layer = "zonas", driver = "ESRI Shapefile", dsn = "resultado/zonas.shp")
plot(x[[4]])
x@data
x[[1]]@data
