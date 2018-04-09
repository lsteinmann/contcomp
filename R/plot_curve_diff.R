#' Returnes a Plot ot two curves and their differences
#'
#' Just aimed at conformity to be able to actually compare two vessels / contours / curves visually.
#'
#' @param model_df a dataframe with vessels / objects / cases as rows and observations as columns, ideally a combination of the shinoto_df$modelcontour-values as returned by the find_shinoto_index() function or possibly a transposed version of shapaardata_df as returned by load_shapaar_contours() / clean_contour_df()
#' @param item1 rowname(!) or indexvalue (not recommended) of a vessel / contour from model_df
#' @param item2 rowname(!) or indexvalue (not recommended) of a vessel / contour from model_df
#'
#' @return A plot.
#'
#' @export plot_curve_diff

# Plots two curves / series of points and their resulting difference-curve
# which is simply item1 - item2
plot_curve_diff <- function(df, item1, item2) {
  xmax = ncol(df)
  ymax = max(df)+50
  x = c(1:xmax)
  plot(x    = c(1, xmax),
       y    = c(0, ymax),
       col  = "white",
       main = paste("Comparison of the profile contours of\n", item1, " and ",
                    item2, sep = ""),
       xlab = "vertical axis of vessel",
       ylab = "horizontal axis of vessel")
  polygon(x   = c(x, rev(x)),
          y   = c(df[item1,], rev(df[item2,])),
          col = adjustcolor("goldenrod", alpha.f = 0.3))
  lines(x   = x,
        y   = df[item1,],
        col = "navy",
        lwd = 5)
  lines(x   = x,
        y   = df[item2,],
        col = "olivedrab",
        lwd = 5)
  lines(x   = x,
        y   = as.data.frame(abs(df[item1,] - df[item2,])),
        col = adjustcolor("hotpink4", alpha.f = 0.8),
        lwd = 5,
        lty = 4)
  legend(x = 0, y = ymax,
         legend = c(item1, item2, "absolute differences"),
         col = c("navy", "olivedrab", adjustcolor("hotpink4", alpha.f = 0.8)),
         lwd = c(5,5,5),
         lty = c(1,1,4))
}

