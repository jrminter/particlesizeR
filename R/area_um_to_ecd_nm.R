#' Calculate the equivalent circular diameter cut-off in nm from the
#' area in microns
#' 
#' A convenience function to make Fiji/ImageJ particle analysis easier.
#' Often we want to measure the diameter of nanoparticles in nm units
#' but work with images with scale factors in microns/px. The 
#' AnalyzeParticles function wants a lower area cutoff in microns.
#' 
#' @param area_um - the equivalent circular diameter of an object in nm.
#'        Example: 7.854
#' 
#' @return  ecd in nm.
#' 
#' @export
#' 
#' @examples
#' library(particlesizeR)
#' print(area_um_to_ecd_nm(7.853982e-05))
#' 
#' @export
area_um_to_ecd_nm <- function(area_um){
  area_part_sq_nm <- 1.0e6*area_um
  r_sq_nm2 <- area_part_sq_nm / pi
  r_nm <- sqrt(r_sq_nm2)
  ecd_nm <- 2.0*r_nm
  return(ecd_nm)
}