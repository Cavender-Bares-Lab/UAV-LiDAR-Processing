input_file <- "F:/point_clouds/2022-04-10_FAB2_normalized.las"
clip_path <- "data/FAB2_plots.gpkg"

clip <- st_read(dsn = clip_path)
sub_clip <- subset(clip, id == "1015")

point_cloud <- readLAS(input_file)

pc <- clip_roi(point_cloud, sub_clip)

writeLAS(pc, "data/PIBA-1050.las", index = FALSE)
