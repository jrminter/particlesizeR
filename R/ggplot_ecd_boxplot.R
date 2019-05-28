#' Plot a box plot of Equivalent Circular Diameter data
#'
#' Make a ggplot2 boxplot of the ECD
#'
#' @param ecd vector - Contains the ECD
#' @param c_fixed number default: 0.1 The width parameter
#' @param title String - A title for the plot. Default: "Box Plot"
#' @param ylab String - the y axis label. Default = "ECD [nm]"
#' @param base_txt_pts String - the size in points. Default 12.
#' 
#' @import ggplot2
#'
#' @return plt
#'
#' @examples
#' library(particlesizeR)
#' # simulate a distribution
#' vec <- rnorm(1000, mean=90, sd=2)
#' ggplot_ecd_boxplot(vec, 0.2, "test", "ECD [nm]", 12)
#'
#' @export


ggplot_ecd_boxplot <- function(ecd, c_fixed=0.1, title="Box Plot",
                              ylab = "ECD [nm]", base_txt_pts=12){
df <- data.frame(ecd=ecd)
the_plt <- ggplot(df, aes(x="", y=ecd)) + 
           geom_boxplot() +
           coord_fixed(c_fixed) +
           labs(y=ylab, x="") +
           ggtitle(title) +
           theme(axis.text=element_text(size=base_txt_pts),
                 axis.title=element_text(size=base_txt_pts+2),
                 plot.title = element_text(hjust = 0.5)) +
           NULL

return(the_plt)
}