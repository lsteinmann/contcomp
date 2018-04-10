#' Clean the dataframe returned by load_shapaar_contours() from infinite values.
#'
#' This funtion replaces infinite values in a data frame with the values from either
#' * the row before or, if that is impossible,
#' * the first row that is not infinite.
#'
#' @param shapaardata_df A data frame with cases as columns and observations as rows that might contain unwanted infinite values.
#'
#' @return The same data frame, with infinites replaced.
#'
#' @export clean_contour_df




clean_contour_df <- function(shapaardata_df) {
  for (col in 1:ncol(shapaardata_df)) {
    if (is.infinite(sum(shapaardata_df[,col])) == TRUE) {
      print(paste("Column ", colnames(shapaardata_df[col]), " has infinite values ", sep=""))
      rownum <- NULL
      rowval <- NULL
      for (row in 1:nrow(shapaardata_df)) {
        if (is.infinite(shapaardata_df[row,col]) == TRUE) {
          rownum <- c(rownum, row)
          rowval <- c(rowval, shapaardata_df[row,col])
          if (length(shapaardata_df[(row-1),col]) == 0) {
            shapaardata_df[row,col] <- shapaardata_df[match(FALSE, is.infinite(shapaardata_df[,col])),col]
          } else {
            shapaardata_df[row,col] <- shapaardata_df[(row-1),col]
          }
          next
        } else {
          next
        }
      }
      # This is unnecessary, but you might want to know which images caused problems
      # to clean them up etc., who knows.
      print(paste("in rows ", rownum[1],
                  " to ", rownum[length(rownum)], ".", sep = ""))
      print("The value will be substituted with an adjacent value.")
      #print(paste("The end of the sequence now shows a jump from",       ## This is unnecessary.
      #            shapaardata_df[rownum[length(rownum)],col], " to ",
      #            shapaardata_df[rownum[length(rownum)+1],col],
      #            ". (If this says NA it means it was at the end.)", seq=""))
      #
      next
    } else {
      #print(paste("Column ", colnames(shapaardata_df[col]), " is ok.", sep="")) ## This is unnecessary.
      next }
    colnames(shapaardata_df[,col])
  }
  return(shapaardata_df)
}
