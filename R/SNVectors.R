#' Surface Normal Unit Vectors of DEM
#'
#' @param inRas Input DEM SpatRaster.
#' @param win Window size, should be an odd value greater than 1.(default 5)
#' @param outType
#' Type of output:
#' mat - List of matrices
#' rast - Multiband SpatRaster
#' both - Matrices and rasters
#' default (both)
#'
#' @return XYZ components of the surface normal unit vectors, average elevation,
#' and approximations of surface area and slope.
#' @export
#'
#' @examples
#'f <- base::system.file("ex/elev_vinschgau.tif", package="terra")
#'r <- terra::rast(f)
#'a <- terra::disagg(r, 5)
#'r1 <- terra::resample(r, a, "bilinear")
#'
#'unitVec <- unitVectors(inRas = r1, win = 5, outType = 'both')
#'terra::plot(test$rasts, col=grDevices::gray(1:200/200))
unitVectors <- function(inRas, win = 5, outType = c('both', 'mat', 'rast')){

  stopifnot("Window size must be odd (e.g., 3, 5, etc.)." = (win %% 2 != 0))

  stopifnot("Input raster must be class of 'SpatRaster'." = base::class(inRas) == 'SpatRaster')

  outType <- base::match.arg(outType)

  # convert input raster to matrix and get raster resolution
  inx <- base::as.matrix(inRas, wide=TRUE)
  res <- terra::res(inRas)[1]

  # offsets from edges of raster extent
  wn <- win - 1

  # bounds of raster to process up to to avoid edge effects
  nrow1 <- base::nrow(inx) - wn
  ncol1 <- base::ncol(inx) - wn

  # output matrices initialized with zero or NA values
  outX <- base::matrix(0, base::nrow(inx), base::ncol(inx))
  outY <- base::matrix(0, base::nrow(inx), base::ncol(inx))
  outEl <- base::matrix(0, base::nrow(inx), base::ncol(inx))
  outZ <- base::matrix((wn * res)^2, base::nrow(inx), base::ncol(inx))

  # get x and y components of the surface and average elevation of corner points
  for (i in 1:nrow1){
    for (j in 1:ncol1){

      # corners of window
      a <- inx[i + wn, j]
      b <- inx[i + wn, j + wn]
      c <- inx[i,j]
      d <- inx[i, j + wn]

      # check if any of the corners is missing data
      chk <- a + b + c + d
      # if((!is.na(a)) & (!is.na(b)) & (!is.na(c)) & (!is.na(d))){
      if(!is.na(chk)){

        # vector creation from window corners
        va <- b - a
        vb <- c - a
        vc <- b - d
        vd <- c - d

        a <- inx[i + wn, j]
        b <- inx[i + wn, j + wn]
        c <- inx[i, j]
        d <- inx[i, j + wn]

        # components of surface normal vector
        outX[i + (wn/2), j + (wn/2)] <- 0.5 * (wn * res) * (vd - va)
        outY[i + (wn/2), j + (wn/2)] <- 0.5 * (wn * res) * (vc - vb)
        outEl[i + (wn/2), j + (wn/2)] <- (a + b + c + d) / 4
      }
    }
  }

  # get magnitude of vector to create unit vector
  # components (equivalent to surface area)
  den <- base::sqrt((outX ^ 2) + (outY ^ 2) + (outZ ^ 2))

  # unit vector components of surface normal vector
  unitX <- outX / den
  unitY <- outY / den
  unitZ <- outZ / den
  slope <- base::acos(unitZ)

  unitX[is.na(inx)] <- NA
  unitY[is.na(inx)] <- NA
  unitZ[is.na(inx)] <- NA
  outEl[is.na(inx)] <- NA
  den[is.na(inx)] <- NA
  slope[is.na(inx)] <- NA

  # combine layers into a list and convert to rasters
  outMats <- base::list(unitX, unitY, unitZ, outEl, den, slope)
  base::names(outMats) <- c("nX", "nY", "nZ", "AvgEl", "SurfArea", "Slope")

  outRasts <- abind::abind(unitX, unitY, unitZ, outEl, den, slope, along = 3)
  outRasts <- terra::rast(outRasts, extent = terra::ext(inRas), crs = terra::crs(inRas))
  base::names(outRasts) <- c("nX", "nY", "nZ", "AvgEl", "SurfArea", "Slope")

  if(outType == 'mat'){

    return(outMats)

  }else if(outType == 'rast'){

    return(outRasts)

  }else if(outType == 'both'){

    out <- base::list(outMats, outRasts)
    base::names(out) <- c('mats', 'rasts')
    return(out)

  }
}
