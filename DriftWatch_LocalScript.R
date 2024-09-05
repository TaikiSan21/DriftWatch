# Driftwatch Local Script
# Wherever DriftWatch lives
dwFolder <- '.'
# Load functions
source(file.path(dwFolder, 'DriftWatchFunctions.R'))
# Load secrets - keys for API download
secrets <- read_yaml(file.path(dwFolder, '.secrets/secrets.yaml'))

db <- file.path(dwFolder, 'DWGPS.sqlite3')

# we shouldn't need to download the SPOT GPS - only used by SWFSC
# cat('\nUpdating GPS from SPOT API...')
# updateSpot <- addAPIToDb(key=secrets$spot_key, db=db, source='spot')

cat('\nUpdating GPS from Lonestar API...')
updateLs <- addAPIToDb(key=secrets$lonestar_swfsc, db=db, source='lonestar')
updateLs <- addAPIToDb(key=secrets$lonestar_nefsc, db=db, source='lonestar')

# googledrive::drive_auth(cache='.secrets', scopes="https://www.googleapis.com/auth/drive")
cat('\nChecking for new deployments on worksheet...')

updatedDrifts <- checkDeploymentUpdates(sheetId='17gWl_J9qpYhUaFhH7gRvNyWmK_bMK_H504vf0I-34Gk',
                                        db=db)

# extra arbitrary points can be added to the windy KML - just add a name and decimal coordinates
nrsLocs <- data.frame(name=c('NRS08_2022'),
                      Latitude=c(39.923967),
                      Longitude=c(-67.012467))

# write in the names of the drifts you want to upload in "drift"
# then upload the file from "filename" to Windy upload
# will have drift track + a point at the most recent position
kmlShapefile <- gpsToKml(db, 
                         drift=paste0('CalCurCEAS_', c('002', '003')),
                         filename='CalCurKML',
                         extraLocs=nrsLocs)
# leaving out "drift = ..." just gets all the data
kmlShapefile <- gpsToKml(db, 
                         filename='AllKML', 
                         extraLocs=nrsLocs,
                         contour='NEFSC_DepthContours.rds')

# plot speed of recent buoys - this will show all buoys of last 7 days
# can shrink window if too much irrelevant info
plotSpeedSummary(db, days=7, units='kmh')
