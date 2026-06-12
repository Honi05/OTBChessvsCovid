# Map each kmeans cluster id (1..k, arbitrary order from kmeans()$centers'
# row order) to its rank (1..k) by ascending back-transformed value of
# `rating_col`, so cluster numbering is stable across re-fits and easy to
# interpret ("cluster 1" = lowest rating, "cluster k" = highest).
cluster_rank_by_rating <- function(centers, rating_col, rating_center, rating_scale) {
  rating_centroids <- centers[, rating_col] * rating_scale + rating_center
  as.integer(rank(rating_centroids, ties.method = "first"))
}
