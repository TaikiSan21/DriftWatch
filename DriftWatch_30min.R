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
gdriveDest <- as_id('1AnL8LXi9deg5gQpCICp3rnn5YfTb6iSz')

cat('\n---------Script version', thisVersion(), 'run on', as.character(Sys.time()), 'with R version', R.version$version.string, '---------------')
with_drive_quiet({
    drive_auth(email=secrets$email, cache='.secrets')
})
cat('\nDirectory set to', script.dir)
db <- 'SPOTGPS_Logger.sqlite3'

cat('\nUpdating GPS from SPOT API...')
updateSpot <- addAPIToDb(db=db, source='spot')

cat('\nUpdating GPS from Lonestar API...')
updateLs <- addAPIToDb(db=db, source='lonestar')
# Upload to gdrive
cat('\nChecking for new deployments on worksheet...')
updatedDrifts <- checkDeploymentUpdates(db=db)
updateGpsCsv(db, csvDir='GPS_CSV', id='1xiayEHbx30tFumMagMHJ1uhfmOishMn2', dataPath='PlottingData',
             force=updatedDrifts)
cat('\nUploading DB to gdrive...')
with_drive_quiet({
    drive_upload(db, path=gdriveDest, overwrite = TRUE)
})

cat('\nSending GPS update text messages...')
doTextUpdates(db)
warns <- warnings()
if(length(warns) > 0) {
    print(warns)
}
cat('\nAll completed successfully!')