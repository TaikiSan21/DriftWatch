# geojson to kml
library(geojsonsf)

file1 <- 'Target_gridcells.geojson'
file2 <- '14nmi_from_200m.geojson'

# just need to make a DriftName field to identify these by 
# and this gets referred to later to manually change the color
# in hthe gpsToKml function
data1 <- geojson_sf(file1)
data1$DriftName <- 'TargetGridcells'
data2 <- geojson_sf(file2)
data2$DriftName <- '14nmiFrom200m'
plot(data1)
plot(data2)

library(ggplot2)

ggplot(data1) + geom_sf()
ggplot(data2) + geom_sf()

saveRDS(combo[c('geometry', 'DriftName')], 'NEFSC_NewGEOJSON.rds')
cont <- readRDS('NEFSC_DepthContours.rds')
ggplot(cont) + geom_sf()

data1 <- data1[names(data2)]
combo <- rbind(data1, data2)
combo <- readRDS('NEFSC_NewGEOJSON.rds')
ggplot(combo) + geom_sf()

library(sf)
st_write(combo, 'Test.kml', append=FALSE, dataset_options=c('NameField=DriftName'))
textKml <- readr::read_file('Test.kml')
    
kmlShapefile <- gpsToKml(db, 
                         drift=paste0('CalCurCEAS_', c('002', '003')),
                         filename='Testoo',
                         contour = 'NEFSC_NewGEOJSON.rds')


plotos <- plotSpeedSummary(db, days=360, units='kmh', gpsFormat='deciminute', filename='TestSpeedo')

doGdriveUpload(plotos, destination = as_id('1KEl82vascis6mqUcLHBv2-_MhFuodud2'))
