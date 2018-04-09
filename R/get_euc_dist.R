#' Get the euclidean distance between two points in the coordinate system of the vessel, order is irrelevant
#'
#' @param coord1_nu one number 
#' @param coord2_nu one number
#' @return euclidean distance between coord1_nu and coord2_nu


get_euc_dist <- function(coord1_nu, coord2_nu) {
  xydist_nu <- sqrt(sum((coord1_nu - coord2_nu) ^ 2))
  return(xydist_nu)
}
