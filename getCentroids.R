getcentroids <- function(x1) {
  getcentroid <- function(x) {
    coords <- slot(x, "coords")
    numrows <- dim(coords)[1]
    ret <- if(numrows == 2) {
      matrix(apply(coords,2,mean), nrow=1)
    } else {
      if(numrows == 1) {
        coords
      } else {
        geosphere::centroid(coords)
      }
    }
    ret
  }
  
  r <- lapply(x1, function(x) as.data.frame(getcentroid(x)))
  
  ret <- matrix(unlist(r), ncol=2, byrow=TRUE)
  rownames(ret) <- names(r); colnames(ret) <- c("lon", "lat")
  
  ret
}

lapply(ct, getcentroids)
