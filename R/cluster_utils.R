

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


#' calculates the SD of the difference between two curves, there might
#' be a better way
calculate_curve_diff <- function(df, item1, item2, type = "sd"){
  diff <- NULL
  diff <- abs(model_df[item1,] - model_df[item2,])
  diff <- data.table::transpose(diff)
  if (type == "sd") {
    diff_value <- sd(diff$V1)
  }
  if (type == "sum") {
    diff_value <- sum(diff$V1)
  }
  if (type != "sum" & type != "sd") {
    stop('Invalid type. Choose "sum" or "sd".' )
  }
  return(diff_value)
}


#' converts a matrix as used in the curve_dist function into an (almost) proper
#' dist-object to be used with hclust etc.
#' so, I only tried to use a method proposed somewhere in this thread:
#' https://stackoverflow.com/questions/11343637/convert-a-dataframe-to-an-object-of-class-dist-without-actually-calculating-di
#' and in hindsight, it does not actually work, which is why I removed it from
#' the get_xxx_dist-functions... if somebody wants to try, though, and maybe
#' find a better way: be my guest. it would be nice to be able to use conventional 
#' clustering methods.
convert_to_dist <- function(manual_distance) {
  distdf <- reshape2::melt(manual_distance)
  
  dij2 <- with(distdf, value)
  nams <- with(distdf, unique(c(as.character(Var2), as.character(Var1))))
  attributes(dij2) <- with(distdf, list(Size = length(nams),
                                        Labels = nams,
                                        Diag = FALSE,
                                        Upper = FALSE,
                                        method = "user"))
  class(dij2) <- "dist"
  return(dij2)
}


# converts a data-frame with vessels/curves as rows and colums as single values
# into a distance matric using the 'distance' measure as provided by
# calculate_curve_diff() and returns the (almost proper) object of class dist
get_curve_dist <- function(model_df, type = "sd") {
  manual_distance <- matrix(0, nrow = nrow(model_df), ncol = nrow(model_df))
  for (row in 1:nrow(model_df)) {
    item1 <- rownames(model_df[row,])
    for (compare_row in 1:nrow(model_df)) {
      diff <- calculate_curve_diff(model_df, 
                                   item1, 
                                   rownames(model_df[compare_row,]),
                                   type = type)
      manual_distance[row,compare_row] <- diff
    }
  }
  dimnames(manual_distance) <- list(c(rownames(model_df)), c(rownames(model_df)))
  #manual_distance <- convert_to_dist(manual_distance)
  return(manual_distance)  
}

#' converts a data-frame with vessels/curves as rows and colums as single values
#' into a distance matrix using the 'distance' measure as provided by
#' SimilarityMeasures::Frechet() and returns the (almost proper) object of class dist
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
  #frechet_distance <- convert_to_dist(frechet_distance)
  return(frechet_distance)  
}

#' Procrustes Distance
#' this needs to be explained...
get_procrustes_dist <- function(model_df) {
  procrustes_distance <- matrix(0, nrow = nrow(model_df), ncol = nrow(model_df))
  for (row in 1:nrow(model_df)) {
    item1 <- data.frame(x = 1:1000, 
                        y = data.table::transpose(model_df[row,]))
    for (compare_row in 1:nrow(model_df)) {
      item2 <- data.frame(x = 1:1000, 
                          y = data.table::transpose(model_df[compare_row,]))
      diff <- shapes::procdist(item1, item2, type = "full")
      procrustes_distance[row,compare_row] <- diff
    }
  }
  dimnames(procrustes_distance) <- list(c(rownames(model_df)), c(rownames(model_df)))
  #procrustes_distance <- convert_to_dist(procrustes_distance)
  return(procrustes_distance)  
}



# not sure if working ------------------------------

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

