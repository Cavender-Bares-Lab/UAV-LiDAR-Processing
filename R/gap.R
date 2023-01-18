gap <- function(temp, z_res, k) {
  
  profile <- temp[, .(weigth = sum(.SD),
                      count = .N), 
                      by = "Z_box", .SDcols=c("weight")]
  
  #Get gpag
  profile <- profile[order(-Z_box)]
  total <- sum(profile$count)
  profile$pgap <- 1 - cumsum(profile$weigth)/total
  profile <- profile[order(Z_box)]
  profile <- profile[-1,]
  profile <- profile[, c("Z_box", "pgap")]
  profile <- rbind(profile, data.table(Z_box = (mh+z_res), pgap = 1.0))
  
  #L/LAI
  profile$L <- log(profile$pgap)/log(min(profile$pgap))

  #Get LAD
  profile$PAVD <- (shift(profile$L)-profile$L)
  
  profile$PAVD <- 

}
