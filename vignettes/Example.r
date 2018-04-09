library(ggplot2)
library(cluster)
library(ggdendro)
library(ggimage)
library(shapAARadd)
library(data.table)
library(reshape2)
library(SimilarityMeasures)


shapaardata_df <- load_shapaar_contours("./inst/doc/")
head(shapaardata_df)


any(is.infinite(rowSums(shapaardata_df)))
shapaardata_df <- clean_contour_df(shapaardata_df)
head(shapaardata_df)



# Shinoto-Index

for (item in 1:ncol(shapaardata_df)) {
  print(paste("Processing entry: ", colnames(shapaardata_df[item]), sep=""))
  wip <- NULL
  sec <- NULL
  name <- NULL
  name <- gsub(".png.txt", "", colnames(shapaardata_df[item]))
  wip <- shapaardata_df[,item]
  sec <- find_shinoto_index(wip)
  png(paste("./inst/plots/", name, ".png", sep=""),
      width = 750)
  print(plot_shinoto_index(sec, name))
  dev.off()
  print(paste("Entry ", colnames(shapaardata_df[item]), " finished.", sep=""))
}






model_df <- data.frame(x=c(1:1000))
for (col in 1:ncol(shapaardata_df)) {
  print(paste("Preparing ", colnames(shapaardata_df[col]), sep = ""))
  wip <- find_shinoto_index(shapaardata_df[,col])
  model_df[,col] <- wip$modelcontour
  print("Done.")
  rm(wip)
}
model_df <- data.table::transpose(model_df)
rownames(model_df) <- gsub(".png.txt", "", colnames(shapaardata_df))


attrib_df <- NULL
attrib_df <- read.csv("./inst/doc/attrib.csv", sep=";", encoding = "UTF-8")
attrib_df$image <- paste("./inst/img/", attrib_df$file, "_thumb.png", sep = "")
attrib_df$file <- gsub(".png", "", attrib_df$file)
colnames(attrib_df) <- c("label", "type", "attribute", "origin", "conservation", "thumb")
attrib_df$label <- as.factor(attrib_df$label)
attrib_df <- attrib_df[attrib_df$label %in% rownames(model_df),]
rownames(attrib_df) <- attrib_df$label

## Plot all the curves together as an overview
dev.off()
x = 1:1000
plot(x = c(1,1000),
     y = c(0,250),
     col = "white")
for (row in 1:nrow(model_df)) {
  lines(x = x,
        y = model_df[row,],
        col = adjustcolor(c(attrib_df$type[row]), alpha.f = 0.3),
        lty = c(attrib_df$type[row]),
        lwd = 4)
}

## get some custom distance matrices
# this may take some time.
curve_dist <- get_curve_dist(model_df = model_df, type = "sd")
frechet_dist <- get_frechet_dist(model_df = model_df)
procrustes_dist <- get_procrustes_dist(model_df = model_df)

# Compare the Methods

curve_coords <- as.data.frame(cmdscale(curve_dist))
curve_coords$label <- rownames(curve_coords)
colnames(curve_coords) <-  c("x", "y", "label")
curve_coords <- merge(curve_coords, attrib_df)
head(curve_coords)

frechet_coords <- as.data.frame(cmdscale(frechet_dist))
frechet_coords$label <- rownames(frechet_coords)
colnames(frechet_coords) <-  c("x", "y", "label")
frechet_coords <- merge(frechet_coords, attrib_df)
head(frechet_coords)

procrustes_coords <- as.data.frame(cmdscale(procrustes_dist))
procrustes_coords$label <- rownames(procrustes_coords)
colnames(procrustes_coords) <-  c("x", "y", "label")
procrustes_coords <- merge(procrustes_coords, attrib_df)
head(procrustes_coords)

ggplot(data = curve_coords, aes(x = x, y = y)) +
  geom_image(aes(image = thumb)) +
  geom_point(aes(colour = type))

ggplot(data = frechet_coords, aes(x = x, y = y)) +
  geom_image(aes(image = thumb)) +
  geom_point(aes(colour = type))

ggplot(data = procrustes_coords, aes(x = x, y = y)) +
  geom_image(aes(image = thumb)) +
  geom_point(aes(colour = type))




# compare individual vessels
plot_curve_diff(model_df, 'Eilmann_Abb4f', 'Kopcke_Kat54')
plot_curve_diff(model_df, 'Kopcke_Kat56', 'Eilmann_Abb4f')
plot_curve_diff(model_df, 'Kopcke_Kat56', 'Furtw_III-27')


# heatmap
palette <- colorRampPalette(c('chartreuse4','white'))(256)
heatmap(frechet_dist, Rowv=F, scale='none', col=palette)


# dendrogram
clust <- diana(frechet_dist, metric = "euclidean")
dendr <- dendro_data(clust)
dendr$labels <- merge(dendr$labels, attrib_df)

ggplot() +
  geom_segment(data=segment(dendr), aes(x = x,
                                        y = y,
                                        xend = xend,
                                        yend = yend)) +
  geom_text(data=label(dendr), aes(x = x,
                                   y = y-3000,
                                   label = label,
                                   colour = type),
            size=3,
            hjust = 0) +
  geom_image(data=label(dendr), aes(x, y-3, image = thumb), size = .015) +
  coord_flip() +
  scale_size_identity()


