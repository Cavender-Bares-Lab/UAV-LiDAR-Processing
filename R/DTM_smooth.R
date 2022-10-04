library(terra)
dtm <- rast("Z:/9-UAV/LiDAR/2022-04-10_FAB1-2/L4/FAB2/2022-04-10_FAB1_DTM.tif")

fdtm <- focal(dtm, w= c(3, 3), fun= function(x, ...) quantile(x, c(0.01, 0.1, 0.25, 0.5), ...), na.rm = TRUE) 

terra::ext(dtm)

x_limit_min <- ext(dtm)[1]
x_limit_max <- ext(dtm)[2]
y_limit_min <- ext(dtm)[3]
y_limit_max <- ext(dtm)[4]

x <- (x_limit_max-x_limit_min)/10
y <- (y_limit_max-y_limit_min)/10
r <- rast(nrows= y, ncols= x, xmin= x_limit_min, xmax= x_limit_max, ymin= y_limit_min, ymax = y_limit_max)
crs(r) <- crs(dtm)
x10 <- resample(dtm, r, method= "q1")

x <- (x_limit_max-x_limit_min)/9
y <- (y_limit_max-y_limit_min)/9
r <- rast(nrows= y, ncols= x, xmin= x_limit_min, xmax= x_limit_max, ymin= y_limit_min, ymax = y_limit_max)
crs(r) <- crs(dtm)
x9 <- resample(dtm, r, method= "q1")

x <- (x_limit_max-x_limit_min)/8
y <- (y_limit_max-y_limit_min)/8
r <- rast(nrows= y, ncols= x, xmin= x_limit_min, xmax= x_limit_max, ymin= y_limit_min, ymax = y_limit_max)
crs(r) <- crs(dtm)
x8 <- resample(dtm, r, method= "q1")

x <- (x_limit_max-x_limit_min)/7
y <- (y_limit_max-y_limit_min)/7
r <- rast(nrows= y, ncols= x, xmin= x_limit_min, xmax= x_limit_max, ymin= y_limit_min, ymax = y_limit_max)
crs(r) <- crs(dtm)
x7 <- resample(dtm, r, method= "q1")

x <- (x_limit_max-x_limit_min)/6
y <- (y_limit_max-y_limit_min)/6
r <- rast(nrows= y, ncols= x, xmin= x_limit_min, xmax= x_limit_max, ymin= y_limit_min, ymax = y_limit_max)
crs(r) <- crs(dtm)
x6 <- resample(dtm, r, method= "q1")

x <- (x_limit_max-x_limit_min)/5
y <- (y_limit_max-y_limit_min)/5
r <- rast(nrows= y, ncols= x, xmin= x_limit_min, xmax= x_limit_max, ymin= y_limit_min, ymax = y_limit_max)
crs(r) <- crs(dtm)
x5 <- resample(dtm, r, method= "q1")


x <- (x_limit_max-x_limit_min)/1.5
y <- (y_limit_max-y_limit_min)/1.5
r <- rast(nrows= y, ncols= x, xmin= x_limit_min, xmax= x_limit_max, ymin= y_limit_min, ymax = y_limit_max)
crs(r) <- crs(dtm)
new <- resample(dtm, r, method= "q1")
new <- resample(new, dtm, method= "cubicspline")


clip_terra <- vect("data/large_boundaries/FAB1_large.gpkg")
dtm_new <- terra::crop(new, clip_terra, mask = TRUE)
writeRaster(new, "FAB1_DTM_master.tif", overwrite=TRUE)
