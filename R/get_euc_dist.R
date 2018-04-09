#' Calculate Distance between two points on the contour
#'
#' Get the euclidean distance between two points in the coordinate system of the vessel, order is irrelevant
#' (https://stackoverflow.com/questions/24746892/how-to-calculate-euclidian-distance-between-two-points-defined-by-matrix-contain)
#'
#' @param coord1_nu one number
#' @param coord2_nu one number
#' @return euclidean distance between coord1_nu and coord2_nu
#'
#' @export get_euc_dist


get_euc_dist <- function(coord1_nu, coord2_nu) {
  xydist_nu <- sqrt(sum((coord1_nu - coord2_nu) ^ 2))
  return(xydist_nu)
}
