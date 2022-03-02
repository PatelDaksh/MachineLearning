# Define input array with angles from 60deg to 300deg converted to radians
install.packages("ggplot2", dependencies = TRUE, repos = "http://cran.us.r-project.org")
install.packages("ggplot2")
library(ggplot2)

i <- seq(0, 360, 5)
theta <- i * pi / 180
r1 <- 5
r2 <- 10
set.seed(10)
x1 <- r1 * cos(theta)
y1 <- r1 * sin(theta) + rnorm(length(x1), 0, 0.2)
data_1 <- data.frame(x1, y1, 1)
names(data_1) <- c("xv", "yv", "class")
x2 <- r2 * cos(theta)
y2 <- r2 * sin(theta) + rnorm(length(x2), 0, 0.2)
data_2 <- data.frame(x2, y2, 2)
names(data_2) <- c("xv", "yv", "class")
data <- rbind(data_1, data_2)
plot(data[,1:2])

clusterPlot <- function(type) {
  clusters <- hclust(dist(data[,1:2]), method = type)
  plot(clusters)
  
  clusterCut <- cutree(clusters, 2)
  show(table(clusterCut, data$class)) # show required, else will not print
  
  ggplot(data, aes(xv, yv, color = as.factor(data$class))) + 
      geom_point(alpha = 0.4, size = 3.5) +
      geom_point(col = clusterCut) + 
      scale_color_manual(values = c('black', 'red'))
}

dev.off()
clusterPlot('complete')
dev.off()
clusterPlot('average')
dev.off()
clusterPlot('single')
dev.off()
clusterPlot('ward.D')
