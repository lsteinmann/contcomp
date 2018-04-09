#' (Custom) Distance Matrix for contour curves
#'
#' Converts a dataframe with vessels as rows and columns as observations into a custom
#' distance matrix (which is not a proper objects of class dist, I would be glad if someone
#' where to fix this) using the distance measure as provided by SimilarityMeasures::Frechet()
#' and returns the resulting matrix.
#'
#' Quote:
#' This algorithm calculates the Frechet distance.
#' The Frechet metric (or distance) is generally described in the
#' following way: A man is walking a dog on a leash, the man walks on
#' one curve while the dog walks on the other (Alt & Godau, 1995).
#' The dog and the man are able to vary their speeds, or even stop,
#' but not go backwards. The Frechet metric is the minimum leash length
#' required to complete the traversal of both curves. Please see the
#' references for more information.
#'
#' Source:
#' https://www.rdocumentation.org/packages/SimilarityMeasures/versions/1.4/topics/Frechet
#'
#' @param model_df a dataframe with vessels / objects / cases as rows and observations as columns, ideally a combination of the shinoto_df$modelcontour-values as returned by the find_shinoto_index() function or possibly a transposed version of shapaardata_df as returned by load_shapaar_contours() / clean_contour_df()
#'
#' @return a (distance) matrix (**not** of class dist) of model_df
#'
#' @export get_frechet_dist
#' @import SimilarityMeasures

get_frechet_dist <- function(model_df) {
  frechet_distance <- matrix(0, nrow = nrow(model_df), ncol = nrow(model_df))
  for (row in 1:nrow(model_df)) {
    item1 <- model_df[row,]
    for (compare_row in 1:nrow(model_df)) {
      diff <- SimilarityMeasures::Frechet(as.matrix(item1), as.matrix(model_df[compare_row,]))
      frechet_distance[row,compare_row] <- diff
    }
  }
  dimnames(frechet_distance) <- list(c(rownames(model_df)), c(rownames(model_df)))
  #frechet_distance <- convert_to_dist(frechet_distance) ## This does NOT work.
  return(frechet_distance)
}
