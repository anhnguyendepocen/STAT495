library(proxy)

set.seed(76)
points <- data_frame(
  x1 = c(runif(25), runif(25, min=1, max=2)),
  x2 = c(runif(25), runif(25, min=1, max=2))
)

centroids <- data_frame(
  x1 = c(0.25, 0.25),
  x2 = c(0.75, 0.75)
)

# Hint
D <- proxy::dist(x=points[, c("x1","x2")], y=centroids[, c("x1","x2")])

results <- kmeans(points, centers=2)
results$cluster

points$cluster <- as.factor(results$cluster)
results$centers


ggplot(points, aes(x=x, y=y, col=cluster)) +
  geom_point()
