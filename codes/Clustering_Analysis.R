# ============== Cluster Analysis ==============

# store the coordinates and clusters from kmeans clustering 
coords <- cluster_analysis[, c("x", "y")]
clusters <- cluster_analysis$cluster

# convert the cluster column from factor to numeric 
clusters <- as.numeric(clusters)

# calculate the cluster statistics 
distances <- dist(coords)
cluster_stats <- cluster.stats(distances, clusters)

# Extract information from cluster_stats 
cluster_size <- cluster_stats$cluster.size
average_distance <- cluster_stats$average.distance
average_within <- cluster_stats$average.within
average_between <- cluster_stats$average.between
dunn2_index <- cluster_stats$dunn2 

# store results in a df 

cluster_results <- data.frame(
  Cluster = 1:length(cluster_size), 
  Average_distance = average_distance,
  Cluster_size = cluster_size, 
  Average_within = average_within, 
  Average_between = average_between, 
  Dunn2_Index = dunn2_index
)