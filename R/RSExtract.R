#' Ridge-Swale morphometry extraction using surface normal vectors
#'
#' @param inEl Avg. Elevation post unit vector extraction. (SpatRaster)
#' @param inX X-component of the surface normal vectors. (SpatRaster)
#' @param inY Y-component of the surface normal vectors. (SpatRaster)
#'
#' @return All combinations of ridge swale patterns, returns a list containing both
#' matrix and raster data structures.
#' @export
#'
ridge.swale <- function(inEl, inX, inY){

  base::stopifnot("Input raster must be class of 'SpatRaster'." = base::class(inEl) == 'SpatRaster')
  base::stopifnot("Input raster must be class of 'SpatRaster'." = base::class(inY) == 'SpatRaster')
  base::stopifnot("Input raster must be class of 'SpatRaster'." = base::class(inX) == 'SpatRaster')

  inRas <- inEl
  inEl <- base::as.matrix(inEl, wide = TRUE)
  inX <- base::as.matrix(inX, wide = TRUE)
  inY <- base::as.matrix(inY, wide = TRUE)

  # variables for processing extent
  nr <- base::nrow(inEl)
  nc <- base::ncol(inEl)

  nrow1 <- nr - 1
  ncol1 <- nc - 1

  nrow2 <- nr - 2
  ncol2 <- nc - 2

  ncol3 <- nc - 3
  nrow3 <- nr - 3

  # Set values of the surface normal vectors to +/- 1 based on their sign
  px <- inX
  px[px > 0] <- 1
  px[px < 0] <- -1

  py <- inY
  py[py > 0] <- 1
  py[py < 0] <- -1

  # Initialize ridge and swale matrices for both directional components
  Mult_xx <- base::matrix(0, nr, nc)
  Mult_yy <- base::matrix(0, nr, nc)

  px[base::is.na(px)] <- 0
  py[base::is.na(py)] <- 0

  # Find where neighboring cells are pos. and neg. and set the value to a ridge or swale
  # based on orientation and elevation value
  # x-component

  for(i in 1:nr){
    for(j in 1:ncol3){
      # Ridge
      if((px[i, j] + px[i, j + 1] < 0) & (px[i, j + 2] + px[i, j + 3] > 0)){

        els <- c(inEl[i,j + 1], inEl[i, j + 2])
        pos <- c(j + 1, j + 2)

        Mult_xx[i, pos[base::which(els == base::max(els))]] <- 2
        # if(inEl[i,j + 1] > inEl[i, j + 2]){
        #
        #   Mult_xx[i,j+1] <- 2
        #
        # }else if(inEl[i,j+1] < inEl[i,j+2]){
        #
        #   Mult_xx[i,j+2] <- 2
        #
        # }
      }
      # Swale
      else if((px[i, j] + px[i,j+1] > 0) & (px[i,j+2] + px[i,j+3] < 0)){

        els <- c(inEl[i,j + 1], inEl[i, j + 2])
        pos <- c(j + 1, j + 2)


        Mult_xx[i, pos[base::which(els == base::min(els))]] <- 1

        # if(inEl[i,j+1] < inEl[i,j+2]){
        #
        #   Mult_xx[i,j+1] <- 1
        #
        # }else if(inEl[i,j+1] > inEl[i,j+2]){
        #
        #   Mult_xx[i,j+2] <- 1
        #
        # }
      }
    }
  }
  # y-component
  for(i in 1:nrow3){
    for(j in 1:nc){
      # Ridge
      if((py[i, j] + py[i + 1, j] > 0) & (py[i + 2, j] + py[i + 3, j] < 0)){

        els <- c(inEl[i + 1, j], inEl[i + 2, j])
        pos <- c(i + 1, i + 2)


        Mult_yy[pos[base::which(els == base::max(els))], j] <- 2

        # if(inEl[i+1,j] > inEl[i+2,j]){
        #   Mult_yy[i+1,j] <- 2
        # }else if(inEl[i+1,j] < inEl[i+2,j]){
        #   Mult_yy[i+2,j] <- 2
        # }
      }
      # Swale
      else if((py[i,j] + py[i+1,j]<0) & (py[i+2,j] + py[i+3,j] > 0)){
        els <- c(inEl[i + 1, j], inEl[i + 2, j])
        pos <- c(i + 1, i + 2)


        Mult_yy[pos[base::which(els == base::min(els))], j] <- 1

        # if(inEl[i+1,j] < inEl[i+2,j]){
        #   Mult_yy[i+1,j] <- 1
        # }else if(inEl[i+1,j] > inEl[i+2,j]){
        #   Mult_yy[i+2,j] <- 1
        # }
      }
    }
  }


  ##############################################################################

  cMultxx <- Mult_xx
  cMultyy <- Mult_yy

  tMultxx <- Mult_xx
  tMultyy <- Mult_yy

  cMultxx[cMultxx < 2] <- 0
  cMultyy[cMultyy < 2] <- 0

  cMultxx[cMultxx > 0] <- 1
  cMultyy[cMultyy > 0] <- 1

  tMultxx[tMultxx > 1] <- 0
  tMultyy[tMultyy > 1] <- 0

  cMultxy <- cMultxx + cMultyy
  cMultxy[cMultxy > 0] <- 1

  tMultxy <- tMultxx + tMultyy
  tMultxy[tMultxy > 0] <- 1

  ##############################################################################

  cMultxx[cMultxx == 0] = NA
  cMultyy[cMultyy == 0] = NA

  tMultxx[tMultxx == 0] = NA
  tMultyy[tMultyy == 0] = NA

  cMultxy[cMultxy == 0] = NA
  tMultxy[tMultxy == 0] = NA

  Mult_xx[Mult_xx == 0] = NA
  Mult_yy[Mult_yy == 0] = NA

  # out.List <- base::list(cMultxx, cMultyy, tMultxx, tMultyy, cMultxy, tMultxy, Mult_xx, Mult_yy)
  # base::names(out.List) <- c("ridgeX", "ridgeY", "swaleX", "swaleY",
  #                            "ridgeXY", "swaleXY", "ridge.swaleX", "ridge.swaleY")

  # combine layers into a list and convert to rasters
  outMats <- base::list(cMultxx, cMultyy, tMultxx, tMultyy, cMultxy, tMultxy, Mult_xx, Mult_yy)
  base::names(outMats) <- c("r.X", "r.Y", "s.X", "s.Y",
                            "r.XY", "s.XY", "rs.X", "rs.Y")

  outRasts <- abind::abind(cMultxx, cMultyy, tMultxx, tMultyy, cMultxy, tMultxy, Mult_xx, Mult_yy, along = 3)
  outRasts <- terra::rast(outRasts, extent = terra::ext(inRas), crs = terra::crs(inRas))
  base::names(outRasts) <- c("r.X", "r.Y", "s.X", "s.Y",
                             "r.XY", "s.XY", "rs.X", "rs.Y")

  out.List <- base::list(outMats, outRasts)
  base::names(out.List) <- c('rs.Mats', 'rs.Rasts')
  return(out.List)
}
