#' Make a ggplot panel plot
#'
#' Make a ggplot2 boxplot of the ECD
#'
#' @param ecd_plt ggplot-plot of the ECD distribution
#' @param box_plt ggplot-box-plot of the ECD distribution
#' @param ecd_qq_plt ggplot-qq-plot of the ECD Quantile-Quantile distribution
#' @param plt_file string - the path to file to save, e.g. "~/my_panel_plt.png"
#' 
#' @import ggplot2
#' @import gridExtra
#' 
#' @return None. Save file
#' 
make_ggpanel_plot <- function(ecd_plt, box_plt, ecd_qq_plt, plt_file){
  
  out <- grid.arrange(ecd_plt, box_plt, ecd_qq_plt, nrow=1 )
  ggsave(filename=plt_file, out) 
  
}