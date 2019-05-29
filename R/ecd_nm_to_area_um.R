#' Calculate the area cut-off in microns from the equivaent circular diameter in nm
#' 
#' A convenience function to make Fiji/ImageJ particle analysis easier.
#' Often we want to measure the diameter of nanoparticles in nm units
#' but work with images with scale factors in microns/px. The 
#' AnalyzeParticles function wants a lower area cutoff in microns.
#' 
#' @param ecd_nm - the equivalent circular diameter of an object in nm.
#'        Example: 10.0
#' 
#' @return area_um the area in microns. This Will print in exponential
#' notation.
#' 
#' @export
#' 
#' @examples
#' library(particlesizeR)
#' print(ecd_nm_to_area_um(10.0))
#' 
#' @export
ecd_nm_to_area_um <- function(ecd_nm){
  r_nm <- 0.5*ecd_nm
  a_part_nm <- pi * r_nm^2
  a_part_um <- a_part_nm*(1.0e-6)
  return(a_part_um)
}