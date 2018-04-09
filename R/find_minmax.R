#' Find the most similar or dissimilar curves
#'
#' @param manual_distance A square matrix with distance values in cells and rows/cols as objects.
#'
#' @return The colnames of the mot (dis)similar objects.
#'
#' @export find_max_dist
#' @export find_min_dist


# the two most dissimilar curves
find_max_dist <- function(manual_distance) {
  k <- arrayInd(which(manual_distance == max(manual_distance[!is.na(manual_distance)])), dim(manual_distance))
  print(colnames(manual_distance)[k[,1]])
}


# the two most similar curves
find_min_dist <- function(manual_distance) {
  k <- arrayInd(which(manual_distance == min(manual_distance[manual_distance!= 0])), dim(manual_distance))
  print(colnames(manual_distance)[k[,1]])
}

