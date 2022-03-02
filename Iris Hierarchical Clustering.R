# https://www.r-bloggers.com/hierarchical-clustering-in-r-2/
remove.packages("ggplot2")
install.packages("ggplot2", dep = TRUE) #install proto package
library(ggplot2)

clusterPlot <- function(type) {
  clusters <- hclust(dist(iris[, 3:4]), method = type)
  plot(clusters)
  
  clusterCut <- cutree(clusters, 3)
  show(table(clusterCut, iris$Species)) # show required, else will not print

  ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$Species)) + 
    geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clusterCut) + 
    scale_color_manual(values = c('black', 'red', 'green'))
}

clusterPlot('complete')
clusterPlot('average')
clusterPlot('single')
clusterPlot('ward.D')
