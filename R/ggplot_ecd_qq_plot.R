#' Plot a quantile-quantile of Equivalent Circular Diameter data
#'
#' Make a ggplot2 Q-Q plot of the ECD
#'
#' @param ecd vector - Contains the ECD
#' @param c_fixed number default: 0.1 The width parameter
#' @param title String - A title for the plot. Default: "QQ Plot"
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
#' ecd <- rnorm(1000, mean=90, sd=2)
#' ggplot_ecd_qq_plot(ecd, 0.5, "QQ Plot", "ECD [nm]", 12)
#'
#' @export


ggplot_ecd_qq_plot <- function(ecd, c_fixed=0.5, title="QQ Plot",
                               ylab = "ECD [nm]", base_txt_pts=12){
df <- data.frame(ecd=ecd)
plt <- ggplot(df, aes(sample = ecd)) +
              stat_qq() +
              stat_qq_line() +
              labs(y=ylab, x="") +
              ggtitle(title) +
              coord_fixed(c_fixed) +
              theme(axis.text=element_text(size=base_txt_pts),
                 axis.title=element_text(size=base_txt_pts+2),
                 plot.title = element_text(hjust = 0.5)) +
              NULL
         
return(plt)
}
