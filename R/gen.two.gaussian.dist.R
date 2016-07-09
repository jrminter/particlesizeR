#' Generate a distribution from a two Gaussian mixture
#' 
#' @param p1 - the fractiona probability of the first Gaussian (< 1.0)
#' 
#' @param mu1 - the mean of the first Gaussian
#' 
#' @param s1 - the standard deviation of the first Gaussian
#' 
#' @param mu2 - the mean of the second Gaussian
#' 
#' @param s2 - the standard deviation of the second Gaussian
#' 
#' @param start - starting value of x (e.g. diameter)
#' 
#' @param stop - ending value of x (e.g. diameter)
#' 
#' @param step - step x (e.g. diameter)
#' 
#' @param rn.seed - the random number seed (default 42)
#' 
#' @return dist - a vector of samples
#' 
#' @importFrom stats dnorm
#' 
#' @export
#' 
#' @examples
#' # library(pkgname)
#' samples <- gen.two.gaussian.dist(0.5, 0.5, 1.0, 10.0, 3.0, 0, 20, 0.1)
#' 
#' @export
gen.two.gaussian.dist <- function(p1, mu1, s1, mu2, s2,
                                  start, stop, step, rn.seed=42){
  set.seed(rn.seed)
  x = seq(start, stop, step)
  dist = p1*dnorm(x, mu1, s1) + (1.0-p1)*dnorm(x, mu2, s2)
  return(dist)
}