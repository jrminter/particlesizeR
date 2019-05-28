#' Plot the circularity distribution
#'
#' Make a ggplot2 plot of the traditional circularity distribution
#'
#' @param circ vector - Containing the circularity
#' @param bin_width number - The width of the bin for the circularity histogram
#' @param title String - A title for the plot
#' @param xlab String - the label for the x axis, e.g. "diameter [nm]"
#' @param ylab String - the y label. Default = "Counts"
#' @param base_txt_pts String - the size in points. Default 12.
#'
#' @import ggplot2
#'
#' @return plt
#'
#' @examples
#'
#' library(particlesizeR)
#' # simulate a distribution
#' vec1 <- rnorm(1000, mean=0.90, sd=.05)
#' ggplot_circ(vec1, 0.01, "Circularity distribution", "Circularity", "Counts")
#'
#' @export
#'
#'

ggplot_circ <- function(circ, bin_width, title, xlab, ylab = "Counts",
                        base_txt_pts=12){
  df <- data.frame(circ=circ)
  plt <- ggplot(df, aes(circ)) +
         geom_histogram(binwidth=bin_width) +
         ggtitle(title) +
         theme(plot.title = element_text(lineheight=2, size=base_txt_pts+2)) +
         labs(x=xlab, y=ylab) +
         ggtitle(title) +
         theme(axis.text=element_text(size=base_txt_pts),
               axis.title=element_text(size=base_txt_pts+2),
               plot.title = element_text(hjust = 0.5)) +
         NULL
        return(plt)
}
