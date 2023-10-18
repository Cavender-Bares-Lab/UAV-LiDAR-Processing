voxels_counting <- function(cloud, edge_sizes = NULL, min_size, length_out = 10, bootstrap = FALSE, R = NULL) {
  
  colnames(cloud) <- c("X", "Y", "Z")
  cloud_touse <- cloud
  
  results <- foreach(i = 1:length(edge_sizes), .inorder = FALSE, .combine= rbind) %do% {
      
      vox <- voxels(cloud_touse, edge_length = c(edge_sizes[i], edge_sizes[i], edge_sizes[i]), obj.voxels = FALSE)
      summary <- summary_voxels(vox, edge_length = c(edge_sizes[i], edge_sizes[i], edge_sizes[i]), bootstrap = bootstrap, R = R)
      return(summary)
  }
    
  results <- results[order(Edge.X)]
  return(results)
  
}