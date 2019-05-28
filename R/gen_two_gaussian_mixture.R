#' Generate a sample from a two Gaussian mixture
#' 
#' @param n - the number of samples to generate
#' 
#' @param p1 - the fractional probability of the first Gaussian (< 1.0)
#' 
#' @param mu1 - the mean of the first Gaussian
#' 
#' @param s1 - the standard deviation of the first Gaussian
#' 
#' @param mu2 - the mean of the second Gaussian
#' 
#' @param s2 - the standard deviation of the second Gaussian
#' 
#' @param rn_seed - the random number seed (default 42)
#' 
#' @return rand.samples - a vector of n samples
#' 
#' @importFrom stats rnorm runif
#' 
#' @importFrom mixtools normalmixEM
#' 
#' @export
#' 
#' @examples
#' library(particlesizeR)
#' samples <- gen_two_gaussian_mixture(10000, 0.5, 0.5, 1.0, 10.0, 3.0)
#'
gen_two_gaussian_mixture <- function(n, p1, mu1, s1, mu2, s2, rn_seed=42){
  set.seed(rn_seed)
  #Sample N random uniforms U
  U <- runif(n)
  #Variable to store the samples from the mixture distribution
  rand_samples <- rep(NA, n)
  
  #Sampling from the mixture
  for(i in 1:n){
    if(U[i]< p1){
      rand_samples[i] = rnorm(1, mu1, s1)
    }else{
      rand_samples[i] = rnorm(1, mu2, s2)
    }
  }
  return(rand_samples)
}