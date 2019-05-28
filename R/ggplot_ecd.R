#' Plot a histogram of Equivalent Circular Diameter data
#'
#' Make a ggplot2 histogram plot of the ECD
#'
#' @param ecd vector - Contains the ECD
#' @param bin_width number - The width of the bin for the diameter histogram
#' @param title String - A title for the plot
#' @param xlab String - the label for the x axis, e.g. "diameter [nm]"
#' @param ylab String - the y label. Default = "Counts"
#' @param base_txt_pts String - the size in points. Default 12.
#' 
#' @importFrom stats median
#' @import ggplot2
#'
#' @return plt
#'
#' @examples
#' library(particlesizeR)
#' vec1 <- rnorm(1000, mean=50, sd=1)
#' vec2 <- rnorm(1000, mean=75, sd=1.2)
#' vec <- vec1 + vec2
#' ggplot_ecd(vec, 0.2, "test", "nm")
#'
#' @export
#'
#'

ggplot_ecd <- function(ecd, bin_width, title, xlab, ylab = "Counts",
                     base_txt_pts=12){
         med <- median(ecd)
         df <- data.frame(ecd=ecd)
  plt <- ggplot(df, aes(ecd)) +
         geom_histogram(binwidth=bin_width) +
         ggtitle(title) +
         theme(plot.title = element_text(lineheight=2,
                        size=base_txt_pts+2)) +
         labs(x=xlab, y=ylab) +
         ggtitle(title) +
         theme(axis.text=element_text(size=base_txt_pts),
               axis.title=element_text(size=base_txt_pts+2),
               plot.title = element_text(hjust = 0.5)) +
         geom_vline(xintercept=med,linetype=1, size=1.5, colour="blue") +
         NULL
        return(plt)
}
