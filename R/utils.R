#' Remove single cell feature noise
#'
#' @param rs.List List of ridge and swale feature matrices and rasters (from ridge.swale function)
#'
#' @return All combinations of ridge swale patterns with 'noise' removed, returns a list containing both
#' matrix and raster data structures.
#' @export

single.RM <- function(rs.List){

  inRas <- rs.List[[2]][[1]]
  rs.Mat <- rs.List[[1]]
  nms <- base::names(rs.Mat)

  rs.Mat <- abind::abind(rs.Mat, along = 3)
  rs.Mat[base::is.na(rs.Mat)] <- 0

  outMats <- base::list()
  for(k in 1:base::dim(rs.Mat)[3]){
    # get feature matrix
    tmp <- rs.Mat[,, k]

    # make all features equal to 1
    tmp[tmp != 0] <- 1

    for(i in 2:(base::nrow(tmp) - 1)){
      for(j in 2:(base::ncol(tmp) - 1)){
        if(tmp[i,j] == 1){
          cnt <- base::sum(c(tmp[(i - 1):(i + 1), (j - 1):(j + 1)]))

          if(cnt == 1){
            tmp[i, j] <- 0
          }

        }
      }
    }

    tmp <- tmp * rs.Mat[,, k]
    rs.Mat[,, k] <- tmp
    outMats[[k]] <- tmp
  }

  rs.Mat[rs.Mat == 0] <- NA

  # combine layers into a list and convert to rasters
  base::names(outMats) <- c("r.X", "r.Y", "s.X", "s.Y",
                            "r.XY", "s.XY", "rs.X", "rs.Y")

  outRasts <- rs.Mat
  outRasts <- terra::rast(outRasts, extent = terra::ext(inRas), crs = terra::crs(inRas))
  base::names(outRasts) <- c("r.X", "r.Y", "s.X", "s.Y",
                             "r.XY", "s.XY", "rs.X", "rs.Y")

  out.List <- base::list(outMats, outRasts)
  base::names(out.List) <- c('rs.Mats', 'rs.Rasts')
  return(out.List)

}


# -------------------------------------------------------------------------

#' Single function to extract unit vectors and cleaned ridge-swale features
#'
#' @param inRas Input DEM. (SpatRast)
#' @param win Window size, should be an odd value greater than 1.(default 5)
#' @param rm.S Boolean choice if you want to remove 'noise' from RS features. (default = TRUE)
#'
#' @return List of unit vector and ridge swale feature lists.
#' @export
UV.RS.extract <- function(inRas, win = 5, rm.S = TRUE){

  # Calculate the components of the surface normal vectors
  uv.List <- unitVectors(inRas = inRas, win = win)

  # Calculate ridge and swale features
  rs.List <- ridge.swale(inEl = uv.List$uv.Rasts$AvgEl, inX = uv.List$uv.Rasts$nX, inY = uv.List$uv.Rasts$nY)

  if(rm.S){
    rs.List <- single.RM(rs.List = rs.List)
  }

  out.List <- base::list(uv.List, rs.List)
  base::names(out.List) <- c("Unit.Vectors", "RS.Morph")

  return(out.List)

}
