#' Plotting a profile contour that has been changed into a polynomial model
#' with the added shinoto index points to automatically identify rim, middle
#' and bottom of the vessel for possible comparison with fragments
#' (note that this does not work properly when no turning point has been found,
#' the plot will be made, but might not be informative)
#'
#' @param shinoto_df a dataframe as returned by the find_shinoto_index()- function
#' @param title The title of the plot (ID of vessel)
#'
#' @return a plot (using ggplot) of the shinoto index values and the model curve for comparisons
#'
#' @export plot_shinoto_index
#' @import ggplot2


plot_shinoto_index <- function(shinoto_df, title) {
  require(ggplot2)

  shinotoindex <- shinoto_df[which(!is.na(shinoto_df$index)),]
  shinotoindex$index <- as.factor(shinotoindex$index)

  if (any(is.na(shinoto_df$vessel_area)) == FALSE) {
    vessel_area <- data.frame(c("rim", "middle", "bottom"))
    colnames(vessel_area) <- "area"
    vessel_area$xmin <- NA
    vessel_area$xmin[1] <- min(shinoto_df$x[grep("rim",
                                                 shinoto_df$vessel_area)])
    vessel_area$xmin[2] <- min(shinoto_df$x[grep("middle",
                                                 shinoto_df$vessel_area)])
    vessel_area$xmin[3] <- min(shinoto_df$x[grep("bottom",
                                                 shinoto_df$vessel_area)])
    vessel_area$xmax <- NA
    vessel_area$xmax[1] <- max(shinoto_df$x[grep("rim",
                                                 shinoto_df$vessel_area)])
    vessel_area$xmax[2] <- max(shinoto_df$x[grep("middle",
                                                 shinoto_df$vessel_area)])
    vessel_area$xmax[3] <- max(shinoto_df$x[grep("bottom",
                                                 shinoto_df$vessel_area)])
  } else {
    vessel_area <- NULL
  }

  cols <- c("RP" = "red", "SRP" = "blue",
            "WP" = "grey", "BoP" = "purple",
            "SFP" = "blue", "FP" = "red")

  plot <- ggplot() +
    geom_point(aes(x = shinoto_df$x,
                   y = shinoto_df$modelcontour),
               color = "black") +
    geom_vline(aes(xintercept = shinotoindex$x,
                   colour     = shinotoindex$index),
               alpha       = 0.4,
               show.legend = F,
               size        = 3) +
    geom_point(aes(x      = shinotoindex$x,
                   y      = shinotoindex$modelcontour,
                   colour = shinotoindex$index),
               alpha       = 0.8,
               show.legend = F) +
    scale_colour_manual(values = cols)  +
    geom_text(aes(x     = shinotoindex$x,
                  y     = shinotoindex$modelcontour+100,
                  label = shinotoindex$index),
              alpha = 0.8,
              angle = 90) +
    labs(y     = "Horizontal axis of vessel",
         x     = "Vertical axis of vessel",
         title = title) +
    ylim(-10,300) +
    coord_fixed() +
    theme(panel.background = element_rect(fill = "white", colour = "grey50"))

  #Only if 'vessel_area' is defined...
  if (length(vessel_area) != 0) {
    plot <- plot + geom_rect(aes(xmin = vessel_area$xmin,
                                 xmax = vessel_area$xmax,
                                 ymin = 200,
                                 ymax = 300,
                                 fill = vessel_area$area),
                             alpha = 0.3) +
      theme(legend.position = "bottom",
            legend.title    = element_blank()) +
      guides(fill = guide_legend(reverse=TRUE))
  }

  return(plot)
}


