% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_two_gaussian_mixture.R
\name{gen_two_gaussian_mixture}
\alias{gen_two_gaussian_mixture}
\title{Generate a sample from a two Gaussian mixture}
\usage{
gen_two_gaussian_mixture(n, p1, mu1, s1, mu2, s2, rn_seed = 42)
}
\arguments{
\item{n}{- the number of samples to generate}

\item{p1}{- the fractional probability of the first Gaussian (< 1.0)}

\item{mu1}{- the mean of the first Gaussian}

\item{s1}{- the standard deviation of the first Gaussian}

\item{mu2}{- the mean of the second Gaussian}

\item{s2}{- the standard deviation of the second Gaussian}

\item{rn_seed}{- the random number seed (default 42)}
}
\value{
rand.samples - a vector of n samples
}
\description{
Generate a sample from a two Gaussian mixture
}
\examples{
library(particlesizeR)
samples <- gen_two_gaussian_mixture(10000, 0.5, 0.5, 1.0, 10.0, 3.0)

}
