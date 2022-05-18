# recreate spot script

# Gets directory script is being called from
cmd.args <- commandArgs()
m <- regexpr("(?<=^--file=).+", cmd.args, perl=TRUE)
script.dir <- dirname(regmatches(cmd.args, m))
if(length(script.dir) == 0) stop("can't determine script dir: please call the script with Rscript")
if(length(script.dir) > 1) stop("can't determine script dir: more than one '--file' argument detected")
setwd(script.dir)
source('DriftWatchFunctions.R')
# Destination folder
gdriveDest <- '~/DriftWatch/'

cat('\n------------Script version', thisVersion(), 'run on', as.character(Sys.time()), '---------------')
cat('\nDirectory set to', script.dir)
db <- 'SPOTGPS_Logger.sqlite3'
spotId <- '09m8vfKzAyrx3j1sSqVMCDamuAJKln1ys'
cat('\nUpdating GPS from SPOT API...')
updateSpot <- addAPIToDb(spotId, db=db, source='spot')
lonestarKey <- 'f22e32e6ba5953978f0875c86f07c5ffae24b2bc'
cat('\nUpdating GPS from Lonestar API...')
updateLs <- addAPIToDb(lonestarKey, db=db, source='lonestar')
# Upload to gdrive
cat('\nUploading DB to gdrive...')
with_drive_quiet({
    drive_auth(email='taiki.sakai@noaa.gov')
    drive_upload(db, path=gdriveDest, overwrite = TRUE)
})

# current 2=RTOFSNOWCAST, 4=HYCOM
useCurrent <- 4
cat('\nMaking individual drift plots...')
driftPlots <- doDriftPlots(db, verbose=T, current=useCurrent)

cat('\nMaking last 2 weeks plot...')
noPlot <- c('ADRIFT_004', 'ADRIFT_008')
recentDrifts <- getDbDeployment(db, days=14)
recentDrifts <- recentDrifts[!(recentDrifts$DriftName %in% noPlot), ]
twoWeekPlot <- plotAPIDrift(recentDrifts, filename='Last14Days.png', current=FALSE)

cat('\nMaking test deployment worksheet plots...')
testDepPlots <- plotTestDeployments(current=useCurrent)

cat('\nUploading plots to drive...')
doGdriveUpload(c(driftPlots, twoWeekPlot), paste0(gdriveDest, 'DriftPlots/'))

doGdriveUpload(testDepPlots, paste0(gdriveDest, 'TestDeploymentPlots/'))
# with_drive_quiet({
#     allPngs <- list.files(pattern='png$', full.names = TRUE, recursive = FALSE)
#     for(i in seq_along(allPngs)) {
#         modtime <- file.info(allPngs[i])$mtime
#         diff <- as.numeric(difftime(Sys.time(), modtime, units='hours'))
#         if(diff > 1) next
#         drive_upload(allPngs[i], path=gdriveDest, overwrite = TRUE)
#     }
# })

cat('\nAll completed successfully!')
