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

cat('------------Script version', thisVersion(), 'run on', as.character(Sys.time()), '---------------\n')
cat('Directory set to', script.dir, '\n')
db <- 'SPOTGPS_Logger.sqlite3'
spotId <- '09m8vfKzAyrx3j1sSqVMCDamuAJKln1ys'
cat('Updating GPS from API...\n')
update <- addAPIToDb(id=spotId, db=db)
# Upload to gdrive
cat('Uploading DB to gdrive...\n')
with_drive_quiet({
    drive_auth(email='taiki.sakai@noaa.gov')
    drive_upload(db, path=gdriveDest, overwrite = TRUE)
})

# current 2=RTOFSNOWCAST, 4=HYCOM
useCurrent <- 2
cat('Making individual drift plots...\n')
doDriftPlots(db, verbose=T, current=useCurrent)

cat('Making last 2 weeks plot...\n')
recentDrifts <- getDbDeployment(db, days=14)
plotAPIDrift(recentDrifts, filename='Last14Days.png', current=FALSE)

cat('Making test deployment worksheet plots...\n')
plotTestDeployments(current=useCurrent)

cat('Uploading plots to drive...\n')
with_drive_quiet({
    allPngs <- list.files(pattern='png$', full.names = TRUE, recursive = FALSE)
    for(i in seq_along(allPngs)) {
        modtime <- file.info(allPngs[i])$mtime
        diff <- as.numeric(difftime(Sys.time(), modtime, units='hours'))
        if(diff > 1) next
        drive_upload(allPngs[i], path=gdriveDest, overwrite = TRUE)
    }
})

cat('All completed successfully!')
