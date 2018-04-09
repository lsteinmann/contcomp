bulkload_shapaar_contour <- function(p = "./data/prepared/") {
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


clean_bulkload_contour <- function(shapaardata_df) {
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
            shapaardata_df[row,col] <- shapaardata_df[(row+1),col]
          } else {
            shapaardata_df[row,col] <- shapaardata_df[(row-1),col]
          }
          next
        } else {
          next
        }
      }
      print(paste("in rows ", rownum[1], " to ", rownum[length(rownum)], ". Too bad.", sep = ""))
      print("The value will be substituted with the one from the last or the next row.")
      print(paste("The end of the sequence now shows a jump from", 
                  shapaardata_df[rownum[length(rownum)],col], " to ", 
                  shapaardata_df[rownum[length(rownum)+1],col], 
                  ". (If this says NA it means it was at the end.)", seq=""))
      next 
    } else { 
      #print(paste("Column ", colnames(shapaardata_df[col]), " is ok.", sep=""))
      next }
    colnames(shapaardata_df[,col])
  }
  return(shapaardata_df)
}
