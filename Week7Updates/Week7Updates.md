Week 7 Updates
================
2017-07-14 12:30:00 CDT

Steganography
-------------

This week we learned about receiver operating characteristic (ROC) curves and how to plot them using MATLAB. A ROC curve is a way to compare diagnostic tests as the discrimination threshold is varied. The ROC curve is created by plotting the true positive rate against the false positive rate. Each point on the curve represents a sensitivity/specificity pair. The closer the curve is to the upper left corner, the higher the overall accuracy of the test.

We created four matrices. Matrix A contained all of the "pvals" from all of the cover and stego images. Each row of the matrix is all of the pvals from a single image. Matrix B is a matrix of 1s and 0s where a 1 is returned if the pval in matrix A is higher than the threshold. A 0 is returned if the pval is lower than the threshold. Matrix C is a matrix of the count of how many pvals in Matrix B are greater than the threshold in each image. Matrix D is a matrix of 1s and 0s where a 1 classifies the image as a stego and a 0 classifies the image as a cover. We created 50 rocTable matrices, where the first column is the threshold, the second column is the false positive rate, and the third and final column is the true positive rate. The 50 tables were used to create 50 ROC curves.

![ROC curve 2](ROC%20curve%202.png)

Shoeprints
----------

During the past week, the shoe print group has been developing cluster plots, dendrograms, and other figures to better display our data. Using the skills learned last week (Pokémon data set), we were able to plot and begin interpreting a new data set given to us by our principal investigators. This new data set included ten sets of shoes (two replicates of each). Each shoe was scanned five times and these scans comprise our new data set.

This data was applied to the following r-code.

``` r
library(EBImage)
library(solefinder)
library(dplyr)
library(tidyr)
library(purrr)
library(GGally)
library(plotly)

filenames <- list.files(pattern = "tiff")
filenames[3]

"img" <- filenames [40]

img <- readImage(img)
colorMode(img) = Grayscale
img_crop = img[160:1800, 200:4400,]
display(img_crop, method = "raster")


GrayCrop <- function(x){
  img <- readImage(x)
  img <- channel(img, mode="gray")
  img_crop = img[160:1800, 200:4400]
  img_neg = max(img_crop) - img_crop
}

GrayCrop(filenames[35])

filenames <- list.files(pattern = "tiff")

Hu <- function(x) {
  out <- compute_moments(GrayCrop(filenames[x]))
  data_frame(argument = filenames[x], 
             Hu1 = out[1],Hu2 = out[2],Hu3 = out[3],Hu4 = out[4],
             Hu5 = out[5],Hu6 = out[6],Hu7 = out[7],Hu8 = out[8])
}

map_df (1, Hu)

d <- map_df(1:100, Hu)
```

This code breaks the data down and computes the eight Hu moments for each image. This process took roughly half an hour due to the size of the data set.

The new data file was then applied to the following r-code.

``` r
library(tidyr)

shoeprints <- 
  d %>%
  mutate(argument = gsub(".tiff", "", argument)) %>%
  separate(argument, c("type", "id", "side", "rep"), sep = "_")

shoeprints

library(purrr)
library(EBImage)
library(dplyr)
library(GGally)
library(plotly)
library(tidyr)

ggpairs(shoeprints, columns = 4:12, ggplot2::aes(colour= id))

ggpairs(data = shoeprints[, -(1:3)],x=id, y=id)


(ggparcoord(data = shoeprints, columns = c(5:12), 
            groupColumn = 1, 
           title ="Parallel Coord. Plot of Shoeprints Data", 
           mapping = ggplot2::aes(size = 1, linetype = as.factor(id))) +
 ggplot2::scale_size_identity())  %>% ggplotly

library(plotly)


##Sample
shoe.samp <- shoeprints[sample(1:dim(shoeprints)[1], 50), ]

###Cluster

names <- c( "Hu1", "Hu1")

X<- as.matrix(shoe.samp[, names])

group <- shoe.samp$id
numClusters <- 2

sum(complete.cases(X))
apply(X, 2, function(s) sum(is.na(s)))

clusters_1 <- kmeans(X, centers = numClusters)

clusters_1$cluster
clusters_2 <- kmeans(X, centers = X[sample(1:(nrow(X)), size = numClusters),])

clusters_2$cluster

###Compute Uclidian distances

distances <- apply(X, 1, function(x)
  apply(X, 1, function(y) sqrt(sum((x - y)^2))))

distances[1:5, 1:5]

distances <- dist(X, method = "euclidean")
```

This code both organized the data and created a dendrogram. Running further code allowed us to organize the data into a cluster plot. This plot grouped the Hu moments based on commonalities. Our goals for the coming week are to further organize the data and break it down by both individual and shoe type.

``` r
single <- hclust(distances, method = "single")
plot(single)

(cl <- kmeans(X, 5, nstart = 25))
plot(X, col = cl$cluster) 
points(cl$centers, col = 1:5, pch = 8)
cl

library(ggplot2)
library(plotly)
a<-ggplot(shoe.samp) + geom_point(aes(x = Hu1, y = Hu1, color = as.factor(id)))
ggplotly(a)


### qplot to gg plot
ggplot() +
  geom_sf(data = states) + 
  geom_point(data = floats, aes(x = Longitude, y = Latitude, colour = callSign)) +   
  geom_point(aes(x, y), shape = "x", size = 5, data = rig) + 
  geom_text(aes(x, y), label = "Shoeprints", 
            size = 5, data = rig, hjust = -0.1) + 
  xlim(c(-91, -80)) + ylim(c(22,32))
```

Bullets and Casings
-------------------

This week I was taught by Allison Mark how to use a confocal microscope for scanning bullets. It's a very repetitive process for each land, but definitely enjoyable. First, we make the land come into focus at 5x magnification. Next, we go to the 20x magnification and make each side of the land come into focus. Next, we go to the center and make it come into focus, then slightly above so it's out of focus again. Then we add 0.0065 to that and increase the z-axis to that amount and add 0.006 to that number and change the z-scale (don't know exactly what that is) to that number. Then we go to the left image and make the z-axis equal zero. Then we autolight it and add 20% of that brightness level. Then we hit aquire and get a scan of the land.
