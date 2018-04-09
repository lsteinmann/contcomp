#' Define the areas (currently 'rim', 'middle', 'bottom') of a vessel according to / ispired by the notation developed by
#' M. Shinoto 2008 (Wege der Keramikklassifikation. Am Beispiel einer prähistorischen Irdenware aus Südjapan)
#' URL/Volltext: http://archiv.ub.uni-heidelberg.de/volltextserver/8868/
#'
#' Packages quantmod and stats are required
#' (although it should not be too hard to add a custom quantmod::findValleys-function?
#' however, I haven't done it.)
#'
#' @param contour_vec_nu a numeric/integer vector of the vessel profile as produced by shapAAR::contour_vector_bg()-function
#' or a column of the shapaardata_df as returned by load_shapaar_contours() / clean_contour_df() in this package.
#' @param desired_length_nu the desired numbers of values to be produced by the model (default = 1000)
#'
#' @return A dataframe for one vessel, with columns: $x = step on vertical axis of vessel; $modelcontour = corresponding y-value (horizontal axis of vessel); $head_notation / $foot_notation = not ready yet...; $index = 'interesting' points on the curve / vessel contour; $vessel_area: automated assesment of the area, i.e. rim, middle or foot.
#'
#' @export find_shinoto_index
#' @import quantmod

find_shinoto_index <- function(contour_vec_nu,
                               desired_length_nu = 1000) {

  #require(quantmod)
  #require(stats)
  # x_coord_nu : One vector of the actual x-values/coords needed to process the
  # raw contour as produced by shapAAR::contour_vector_bg()
  x_coord_nu <- 1:length(contour_vec_nu)

  # new_x_coord_nu : Another vector for the following model with the desired
  # number of x values / coords as specified in the function call
  new_x_coord_nu <- seq(0, length(contour_vec_nu), length=desired_length_nu)


  # iterates an arbitrary number of nonlinear regression model to pick out
  # the one that approximates the vessels contour best, producing a list
  # of [arbitrary numer, here 20] lm-objects
  fits_li <- list(type = any)
  for (degree in 1:20) {
    fit_lm <- lm(contour_vec_nu~poly(x_coord_nu,
                                     degree = degree,
                                     raw = TRUE))
    fits_li[[degree]] <- fit_lm
    fit_lm <- NULL
  }

  # produces dataframe with the summary values of anova() for each
  # regression model
  anova_fit_df <- eval(parse(text = paste("anova(",
                                       paste("fits_li[[",
                                             1:length(fits_li),
                                             "]]",
                                             sep = "",
                                             collapse = ","),
                                       ")")))

  # picks the rownames-value of the first occurence of the minimum
  # Sum of Squares-value (corresponding to degree parsed to poly())
  bestfitnr_nu <- as.numeric(rownames(
    anova_fit_df[match(min(na.omit(anova_fit_df$`Sum of Sq`)),
                                   anova_fit_df$`Sum of Sq`),]
    ))

  # regression model for the bestfit
  # ! I added a -2 to actually not use the first best fit, since it was
  # mostly 'too good' a fit, i.e. showing too many of the imperfections
  # produced by scaling the drawings to 100 or 500px etc.
  # it _is_ a bit arbitrary and I would encourage to play around with it?
  bestfit_lm <- lm(contour_vec_nu~poly(x_coord_nu,
                                       bestfitnr_nu,
                                       raw = TRUE))

  # the resultion new contour
  modelcontour_nu <- predict(bestfit_lm,
                             newdata = data.frame(x_coord_nu=new_x_coord_nu))

  # and a simpler model with the third polynomial used / explained later
  simplecontour_nu <- predict(
    lm(contour_vec_nu~poly(x_coord_nu, 3, raw = TRUE)),
                          data.frame(x_coord_nu=new_x_coord_nu)
    )


  # build the df that is going to be returned in the end
  shinoto_df <- data.frame(x = 1:desired_length_nu,
                           modelcontour = modelcontour_nu,
                           head_notation = NA,
                           foot_notation = NA,
                           index = NA,
                           vessel_area = NA)

  # (simplified) second derivation of the simple model
  # (values are the slope of the slope of the original simple contour)
  simplecontour_derivation_nu <- diff(diff(simplecontour_nu))

  # find the (approximate) intersection of the second derivation
  # with the 0 axis by determining changes in sign
  # note: this will not yield good results with fewer points, e.g. 100,
  # and also not work properly with a curve / model that is too 'wavy',
  # as would be the actual model, maybe the third polynomial is not the best
  # one for the task...? it seemed like the most 'workable' one after I tried
  # around a lot...
  changes_derivation_nu <- NULL
  for (i in 1:(length(simplecontour_derivation_nu)-1)) {
    if (sign(simplecontour_derivation_nu[i]) !=
        sign(simplecontour_derivation_nu[i+1])) {
      changes_derivation_nu <- c(changes_derivation_nu, i)
    }
    else {
      next
    }
  }

  # store the information in the $index column.
  shinoto_df$index[changes_derivation_nu] <- "WP"
  shinoto_df$index[1] <- "RP"
  shinoto_df$index[nrow(shinoto_df)] <- "FP"


  # now too find 'SFP' = Sonderfußpunkt and 'SRP' = Sonderrandpunkt,
  # which are striking changes in the curve / model produced by a visible foot
  # or certain treatment of the rim, which might be points of interest,
  # we will search for valleys in the last and first tenth of the vessel
  foot_start <- nrow(shinoto_df)-(nrow(shinoto_df)/10)
  foot_end <- nrow(shinoto_df)

  shinoto_sfp <- quantmod::findValleys(
    shinoto_df$modelcontour[foot_start:foot_end])
  if (length(shinoto_sfp) != 0 ) {
    shinoto_df$index[foot_start+shinoto_sfp] <- "SFP"
  }

  head_start <- 0
  head_end <- nrow(shinoto_df)/10

  shinoto_srp <- quantmod::findValleys(
    shinoto_df$modelcontour[head_start:head_end])
  if (length(shinoto_srp) != 0 ) {
    shinoto_df$index[shinoto_srp] <- "SRP"
  }


  # to catch the so called 'BoP' = Bodenpunkt, which should signify the
  # beginning of the vessel foot / something, this calculated the height on
  # the line between 'RP' and 'FP' (beginning and end of vessel) for each
  # possible triangle between 'RP', 'FP', and every point from the contour
  # (using Heron's formula to calculate the area and from that the height(l_a))
  # function get_euc_dist() (included)
  triangle_df <- data.frame(x  = 1:nrow(shinoto_df),
                               ha = NA)

  B_ma <- cbind(shinoto_df$x[grep("\\<FP\\>", shinoto_df$index)],
             shinoto_df$modelcontour[grep("\\<FP\\>", shinoto_df$index)])
  C_ma <- cbind(shinoto_df$x[grep("\\<RP\\>", shinoto_df$index)],
             shinoto_df$modelcontour[grep("\\<RP\\>", shinoto_df$index)])
  l_a_nu <- get_euc_dist(B_ma, C_ma)

  for (i in 1:nrow(shinoto_df)) {
    A_ma <- NULL
    A_ma <- cbind(shinoto_df$x[i], shinoto_df$modelcontour[i])
    l_b_nu <- get_euc_dist(A_ma, C_ma)
    l_c_nu <- get_euc_dist(A_ma, B_ma)

    co_x_nu <- c(A_ma[1], B_ma[1], C_ma[1])
    co_y_nu <- c(A_ma[2], B_ma[2], C_ma[2])
    flaeche_nu  <- (1/2) * abs((co_x_nu[1]*(co_y_nu[2]-co_y_nu[3])) +
                               (co_x_nu[2]*(co_y_nu[3]-co_y_nu[1])) +
                               (co_x_nu[3]*(co_y_nu[1]-co_y_nu[2])))

    h_nu <- (flaeche_nu * 2)/l_a_nu
    triangle_df$ha[i] <- h_nu

  }

  # the value with the maximum triangle height is the 'BoP'
  shinoto_df$index[triangle_df$x[which.max(triangle_df$ha)]] <- "BoP"



  # now the areas of the vessel need to be classified as Rim, Middle, ord Bottom,
  # which will currently only work if there is a 'WP' / turning point somewhere
  if ( "WP" %in% shinoto_df$index ) {
      shinoto_df$vessel_area[match("RP", shinoto_df$index):match("WP", shinoto_df$index)] <- "rim"
      shinoto_df$vessel_area[match(NA, shinoto_df$vessel_area):match("BoP", shinoto_df$index)] <- "middle"
      shinoto_df$vessel_area[match(NA, shinoto_df$vessel_area):match("FP", shinoto_df$index)] <- "bottom"
  } else {
    # I currently have no idea how to solve this,
    # if there is no turning point...
    print("ERROR: Missing turning point ('WP'). Could not define vessel areas.")
  }

  return(shinoto_df)
}
