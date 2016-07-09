## ------------------------------------------------------------------------
library(particlesizeR)
library(mixtools)

## ------------------------------------------------------------------------
samples <- gen.two.gaussian.mixture(10000, 0.5, 3.0, 1.0, 12.0, 2.0)
samples <- samples[samples > 0]

## ---- fig.width=7--------------------------------------------------------
o.mar <- par("mar")
par(mar=c(4.1,4.1,0,0.5))
his <- hist(samples, breaks=250, freq=FALSE, main="")
par(mar=o.mar)

## ---- fig.width=7--------------------------------------------------------
o.mar <- par("mar")
par(mar=c(4.1,4.1,0,0.5))
x = seq(0,20,.1)
x1 <- min(samples)
x2 <- max(samples)
truth <- gen.two.gaussian.dist(0.5, 3.0, 1.0, 12.0, 2.0, 0, 20, 0.1)
ymax <- 1.1*max(max(his$density), max(truth[,2]))
plot(c(x1, x2), c(0., ymax), type='n', xlab='x', ylab='density')
points(his$mids, his$density, pch=19, cex=0.75)
lines(truth[, 1], truth[, 2],col="red",lwd=4)
par(mar=o.mar)

## ------------------------------------------------------------------------
out <- normalmixEM(samples, lambda=0.5, mu=c( 2, 9.), sigma=c(.8, 2.))

## ------------------------------------------------------------------------
fit <- out$lambda[1]*dnorm(x, out$mu[1], out$sigma[1]) + 
       out$lambda[2]*dnorm(x, out$mu[2],out$sigma[2])

## ---- fig.width=7, fig.height=5------------------------------------------
o.mar <- par("mar")
par(mar=c(4.1,4.1,0.5,0.5))
plot(c(x1, x2), c(0., ymax), type='n', xlab='x', ylab='density')#, 
#    main="Density Estimate of the Gaussian Mixture Model")
points(his$mids, his$density, pch=19, cex=0.75)
lines(truth[, 1], truth[, 2],col="red",lwd=6)
lines(x, fit, col="blue", lwd=2)
legend("topright", c("True Density","Estimated Density", "fit"),
       col=c("black","red", "blue"), lwd=c(NA, 4, 2), pch=c(19,NA,NA))
par(mar=o.mar)

