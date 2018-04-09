#' Load all txt-files from a folder
#'
#' The txt-files must contain a profile contour in the format:
#'     "x"
#'     "1";0
#'     "2";0
#'     etc...
#' as exported by the bulkIMGprep()-function from (a branch) of package
#' shapAAR (https://github.com/ISAAKiel/shapAAR). The files must all contain
#' the same number of rows.
#' (It is also possible to create such files manually from drawings, as long
#' as the format is kept.)
#'
#' @param p default at "./mydata/"; location of the txt-files to be loaded
#'
#' @return A data frame with k rows (number of rows in text files) and n cols (number of text files / contours) with filenames as colnames.
#'
#' @export load_shapaar_contours


load_shapaar_contours <- function(p = "./txtfiles/") {
  f <- list.files(path = p, pattern = paste("\\.", 'txt', "$", sep = ""))
  rows <- nrow(data.frame(read.table(paste(p, f[1], sep = ''), sep = ';')))
  shapaardata_df <- data.frame(matrix(NA, nrow = rows, ncol = 0))
  for (i in 1:length(f)) {
    new <- data.frame(read.table(paste(p, f[i], sep = ''), sep = ';'))
    colnames(new) <- f[i]
    shapaardata_df <- cbind(shapaardata_df, new)
  }
  return(shapaardata_df)
}
