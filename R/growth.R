
# Revius year
data <- subset(data, year(measurement_date) >= 2021)
data$year <- year(data$measurement_date)

#
y2021 <- subset(data, year == 2021)
y2021 <- y2021[, c("block", "plot", "row", "column", "V.conoid_conoidoid_infill")]
y2022 <- subset(data, year == 2022)
y2022 <- y2022[, c("block", "plot", "row", "column", "V.conoid_conoidoid_infill")]

colnames(y2021)[5] <- "y2021"
colnames(y2022)[5] <- "y2022"

frame <- merge(y2021, y2022, by = c("block", "plot", "row", "column"), all.x = TRUE, all.y = TRUE)
frame <- na.exclude(frame)

growth <- frame[, .(y2021 = sum(y2021, na.rm = TRUE),
          y2022 = sum(y2022, na.rm = TRUE)),
          by = "plot"]


growth$RGR <- ((growth$y2022 - growth$y2021)/growth$y2021)*100
growth[is.infinite(RGR) == TRUE, RGR := NA]
fwrite(growth, paste0(root_path, "/growth.csv"))

growth <- fread(paste0(root_path, "/growth.csv"))

frame <- merge(frame, growth, by = "plot", all.x = TRUE, all.y = FALSE)
frame <- subset(frame, type == "Taxonomic")

plot(frame$RGR ~ frame$CV_SEI_horizontal)
summary(lm(log10(frame$RGR) ~ log10(frame$CV_SEI_vertical)))
