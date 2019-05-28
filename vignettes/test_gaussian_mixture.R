## ----setup, cache = F, echo = F, message = F, warning = F, tidy = F, comment=NA----
library(knitr)
options(width = 72)

## ----load_packages, message=FALSE-------------------------------------
library(particlesizeR)
library(mixtools)
library(ggplot2)

## ---------------------------------------------------------------------
samples <- gen_two_gaussian_mixture(10000, 0.5, 3.0, 1.0, 12.0, 2.0)
samples <- samples[samples > 0]

## ----first_plot, fig.width=7------------------------------------------
library(ggplot2)

df <- data.frame(samples=samples)
plt <- ggplot(df, aes(x=samples)) + geom_histogram(bins=250)
print(plt)

## ----examine_histo_head-----------------------------------------------
pg <- ggplot_build(plt)
head(pg$data[[1]])

## ----examine_histo_tail-----------------------------------------------
tail(pg$data[[1]])

## ----generate_data----------------------------------------------------
truth <- gen_two_gaussian_dist(0.5, 3.0, 1.0, 12.0, 2.0, 0, 20, 0.1)

verbose = FALSE

if (verbose == TRUE){
  print(c(min(truth), max(truth)))
  print(max(truth$dist))
  print(max(pg$data[[1]]$density))
  print(head(truth))
}

## ---- fig.width=7-----------------------------------------------------
pt_line_size <- 1
df_work <- data.frame(diam=pg$data[[1]]$x, density=pg$data[[1]]$density)
plt <- ggplot(df_work, aes(diam,density)) + 
       geom_point(size=pt_line_size, colour='darkblue' ) +
       geom_line(data=truth, aes(x,dist), colour='darkred', size=pt_line_size) +
       ylab("density") +
       xlab("diameter [nm]") +
       ggtitle("Diameter distribution of two Gaussian distributions") +
       theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
              plot.title = element_text(hjust = 0.5)) + # center the title
      NULL

print(plt)





## ----mixModel---------------------------------------------------------
out <- normalmixEM(samples, lambda=0.5, mu=c( 2, 9.), sigma=c(.8, 2.))

## ---------------------------------------------------------------------
x <- seq(from=0, to=20, by=0.1)
fit <- out$lambda[1]*dnorm(x, out$mu[1], out$sigma[1]) + 
       out$lambda[2]*dnorm(x, out$mu[2],out$sigma[2])

df_fit <- data.frame(ecd=x, fit=fit)

## ----finalPlot, fig.width=7, fig.height=5-----------------------------
pt_line_size <- 1
final_plt <- ggplot() +
             geom_point(data=df_work, aes(diam,density),
                        colour="darkblue", size=pt_line_size) +
             geom_line(data=df_fit, aes(ecd, fit),
                       colour="darkred", size=pt_line_size) +
             xlab("size [nm]") +
             ylab("density") +
             scale_x_continuous(breaks = seq(from = 0, to = 20, by = 1),
                               limits = c(0, 20)) +
             scale_y_continuous(breaks = seq(from = 0, to = 0.25, by = 0.05),
                               limits = c(0, 0.25)) +
             ggtitle("Diameter distribution from two Gaussian distributions") +
             labs(caption = 'jrminter@gmail.com / @jrminter') +
             theme(axis.text=element_text(size=12),
                   axis.title=element_text(size=12),
                   plot.title=element_text(hjust = 0.5)) + # center the title
             NULL
print(final_plt)


