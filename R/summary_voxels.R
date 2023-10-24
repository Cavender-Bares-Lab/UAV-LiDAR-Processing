voxels_counting <- function(pc, edge_sizes = NULL, min_size, length_out = 10, bootstrap = FALSE, R = NULL) {
  
  colnames(pc) <- c("X", "Y", "Z")
  cloud_touse <- pc
  
  results <- foreach(i = 1:length(edge_sizes), .inorder = FALSE, .combine= rbind) %do% {
    
    vox <- voxels(cloud_touse, edge_length = c(edge_sizes[i], edge_sizes[i], edge_sizes[i]), obj.voxels = FALSE)
    summary <- summary_voxels(vox, edge_length = c(edge_sizes[i], edge_sizes[i], edge_sizes[i]))
    return(summary)
  }
  
  results <- results[order(Edge.X)]
  return(results)
  
}

summary_voxels <- function(voxels, edge_length = NULL) {
  
  if(class(voxels)[1] != "voxels") {
    if(is.null(edge_length) == TRUE) {
      stop("edge_length need to be defined")
    }
    
    colnames(voxels) <- c("X", "Y", "Z", "N")
    Edge.length <- edge_length
    
  } else if(class(voxels)[1] == "voxels") {
    Edge.length <- voxels$parameter
    voxels <- voxels$voxels
  }
  
  profile <- voxels[, .N, by = Z]
  
  frame <- data.table(Edge.X = Edge.length[1], 
                      Edge.Y = Edge.length[2], 
                      Edge.Z = Edge.length[3], 
                      Hill0 = hill(voxels$N, 0),
                      Hill1 = hill(voxels$N, 0.9999),
                      Hill2 = hill(voxels$N, 2), 
                      ENL_Hill0 = hill(profile$N, 0),
                      ENL_Hill1 = hill(profile$N, 0.999),
                      ENL_Hill2 = hill(profile$N, 2))
  
  return(frame)
  
}

hill <- function(n_points, q) {
  p <- n_points/sum(n_points)
  return((sum(p^q)^(1 / (1 - q))))
}
