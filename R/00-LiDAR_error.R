dates <- c("2022-01-01", "2022-04-10", "2022-05-18", "2022-06-12", "2022-07-06", "2022-08-03", "2022-09-07", "2022-09-18")
root <- "/home/antonio/Documents/LiDAR/rasters"

collect <- data.table()

for(i in 1:length(dates)) {
  
  dsf <- rast(paste0(root, "/", dates[i], "_height.tif"))
  gcp <- vect(paste0(root, "/", dates[i], ".gpkg"))
  crs(dsf) <- crs(gcp)
  
  center <- centroids(gcp, inside = TRUE)
  cor <- crds(center)
  center <- as.data.frame(gcp)
  center <- as.data.table(cbind(center, cor))
  colnames(center) <- c("ID", "X", "Y")
  center$ID <- as.character(center$ID)
  
  pixels <- as.data.table(extract(dsf, gcp, cells=FALSE, xy=FALSE))
  colnames(pixels) <- c("ID", "Z")
  pixels$ID <- as.character(pixels$ID)
  
  pixels <- merge(center, pixels, by = "ID", all.x = TRUE, all.y = TRUE)
  
  pixels$date <- dates[i]
  
  collect <- rbind(collect, pixels)
  
}

#Ojo plot "2022-08-03"

collect <- subset(collect, Z <= 270)
collect <- subset(collect, Z >= 250)
collect$ID <- as.numeric(collect$ID)
#collect <- subset(collect, date != "2022-01-01")

fwrite(collect, paste0(root, "/elevation.csv"))

#Get true ground
ground <- subset(collect, date == "2022-01-01")
mean_z <- ground[, mean(Z), by = c("ID")]
colnames(mean_z)[2] <- "Z-smooth"

#Merge
frame <- merge(collect, mean_z, by = "ID", all.x = TRUE, all.y = TRUE)
frame$deltaZ <- frame$Z - frame$`Z-smooth`
frame <- subset(frame, date != "2022-01-01")

ggplot(frame) +
  geom_point(aes(x = date, y = deltaZ, fill = as.character(ID)), shape = 21) +
  geom_smooth(aes(x = date, y = deltaZ, fill = as.character(ID)), method = "lm", se = FALSE) +
  facet_grid(~ ID)

frame <- subset(frame, ID != 10)
frame <- subset(frame, ID != 5)
frame <- subset(frame, ID >= 7)

mean_difference <- frame[, mean(deltaZ), by = c("ID", "date")]
colnames(mean_difference)[3] <- "deltaZ"

ggplot(mean_difference) +
  geom_point(aes(x = date, y = deltaZ, fill = as.character(ID)), shape = 21)
  

mean_difference <- frame[, mean(deltaZ), by = c("date")]
colnames(mean_difference)[2] <- "deltaZ"

ggplot(mean_difference) +
  geom_point(aes(x = date, y = deltaZ), shape = 21) 

mean_difference$deltaZ <- round(mean_difference$deltaZ, 3)

fwrite(mean_z, paste0(root, "/elevation_mean.csv"))
