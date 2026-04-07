# reading shape files
library(sf)

new <- read_sf('./PlottingData/data/ore/CallAreas_OR_Outline_2022_04_22.shp')
new <- new['Call_Area_']
new <- st_zm(new, drop=TRUE)
colnames(new) <- c('Area_Name', 'geometry')
old <- readRDS('./PlottingData/WindCallBoundary.RData')
saveRDS(old, './PlottingData/WindCallOld_0622.rds')
old <- old[2, ]
new <- st_transform(new, crs=st_crs(old))
comb <- rbind(old['Area_Name'], new['Area_Name'])
saveRDS(comb, './PlottingData/WindCallBoundary.RData')
plot(comb$geometry)
