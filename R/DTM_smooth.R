library(terra)
dtm <- rast("/media/antonio/antonio_ssd/point_clouds/2022-04-10_FAB2_DTM.tif")

fdtm <- focal(dtm, w= c(3, 3), fun= function(x, ...) quantile(x, c(0.01, 0.1, 0.25, 0.5), ...), na.rm = TRUE) 

extent(dtm)

x <- (484833.3-485273.2)/10
y <- (5027966-5028176)/10
r <- rast(nrows= y, ncols= x, xmin= 484833.3, xmax= 485273.2, ymin= 5027966, ymax = 5028176)
crs(r) <- crs(dtm)
x10 <- resample(dtm, r, method= "q1")

x <- (485273.2-484833.3)/9
y <- (5028178-5027966)/9
r <- rast(nrows= y, ncols= x, xmin= 484833.3, xmax= 485273.2, ymin= 5027966, ymax = 5028178)
crs(r) <- crs(dtm)
x9 <- resample(dtm, r, method= "q1")

x <- (485273.2-484833.3)/8
y <- (5028178-5027966)/8
r <- rast(nrows= y, ncols= x, xmin= 484833.3, xmax= 485273.2, ymin= 5027966, ymax = 5028178)
crs(r) <- crs(dtm)
x8 <- resample(dtm, r, method= "q1")

x <- (485273.2-484833.3)/7
y <- (5028178-5027966)/7
r <- rast(nrows= y, ncols= x, xmin= 484833.3, xmax= 485273.2, ymin= 5027966, ymax = 5028178)
crs(r) <- crs(dtm)
x7 <- resample(dtm, r, method= "q1")

x <- (485273.2-484833.3)/6
y <- (5028178-5027966)/6
r <- rast(nrows= y, ncols= x, xmin= 484833.3, xmax= 485273.2, ymin= 5027966, ymax = 5028178)
crs(r) <- crs(dtm)
x6 <- resample(dtm, r, method= "q1")

x <- (485273.2-484833.3)/5
y <- (5028178-5027966)/5
r <- rast(nrows= y, ncols= x, xmin= 484833.3, xmax= 485273.2, ymin= 5027966, ymax = 5028178)
crs(r) <- crs(dtm)
x5 <- resample(dtm, r, method= "q1")


x <- (485273.2-484833.3)/0.1
y <- (5028178-5027966)/0.1
r <- rast(nrows= y, ncols= x, xmin= 484833.3, xmax= 485273.2, ymin= 5027966, ymax = 5028178)
crs(r) <- crs(dtm)
new <- resample(x10, r, method= "cubicspline")

plot(dtm - x10)

dtm_old <- rast("/media/antonio/antonio_ssd/point_clouds/2022-04-10_FAB2_DTM.tif")

dtm_crop <- terra::crop(dtm, clip_terra, mask = TRUE)
dtm_new <- terra::crop(new, clip_terra, mask = TRUE)

d1 <- dtm_crop - dtm_new
d2 <- dtm_old - dtm_new

writeRaster(new, "FAB2_DTM_sum2.tif", overwrite=TRUE)

new <- new + 2
