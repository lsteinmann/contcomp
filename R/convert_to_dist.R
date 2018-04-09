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
