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

cat('\n---------Script version', thisVersion(), 'run on', as.character(Sys.time()), 'with R version', R.version$version.string, '-----------')

cat('\nDirectory set to', script.dir)
db <- 'SPOTGPS_Logger.sqlite3'
# spotId <- '09m8vfKzAyrx3j1sSqVMCDamuAJKln1ys'
# cat('\nUpdating GPS from SPOT API...')
# updateSpot <- addAPIToDb(spotId, db=db, source='spot')
# lonestarKey <- 'f22e32e6ba5953978f0875c86f07c5ffae24b2bc'
# cat('\nUpdating GPS from Lonestar API...')
# updateLs <- addAPIToDb(lonestarKey, db=db, source='lonestar')
# # Upload to gdrive
# cat('\nChecking for new deployments on worksheet...')
# checkDeploymentUpdates(db=db)
# cat('\nUploading DB to gdrive...')
with_drive_quiet({
    drive_auth(email='taiki.sakai@noaa.gov', cache='.secrets')
    #     drive_upload(db, path=gdriveDest, overwrite = TRUE)
})
cat('\nChecking for GPS CSV updates...')
updateGpsCsv(db, csvDir='GPS_CSV', id='1xiayEHbx30tFumMagMHJ1uhfmOishMn2', dataPath='PlottingData')
# current 2=RTOFSNOWCAST, 4=HYCOM, 3=HFRADAR
cat('\nUpdating sanctuary summary...')
sanctSumm <- createSanctSummary(db, 'PlottingData')
write.csv(sanctSumm, file='SanctuarySummary.csv', row.names = FALSE)
doGdriveUpload('SanctuarySummary.csv', gdriveDest)
useCurrent <- 4
# now doing both hycom and hfradar for deployed plots
cat('\nChecking HYCOM data...')
hycomOK <- try(updateNc(file='PlottingData/HYCOMGLBycurrent.nc', id=PAMmisc::getEdinfo()[['HYCOM']], vars=c(F, F, F, T, T)))
# hycomOK <- FALSE
cat('\nChecking HFRADAR data...')
hfradarOK <- try(updateNc(file='PlottingData/HFRADARcurrent.nc', id='ucsdHfrW6', vars=c(T, T, rep(F, 5))))

wcofsOK <- try(updateNc(file='PlottingData/WCOFS_ROMS.rds', vars=TRUE))
# wcofsOK <- TRUE

if(inherits(hycomOK, 'try-error') ||
   file.size('PlottingData/HYCOMGLBycurrent.nc') < 1e3) {
    cat('\nWARNING: Problem with downloading HYCOM data')
    hycomOK <- FALSE
}

if(inherits(hfradarOK, 'try-error') ||
   file.size('PlottingData/HFRADARcurrent.nc') < 1e3) {
    cat('\nWARNING: Problem with downloading HFRADAR data')
    hfradarOK <- FALSE
}

if(inherits(wcofsOK, 'try-error') |
   file.size('PlottingData/WCOFS_ROMS.rds') < 1e3) {
    cat('\nWARNING: Problem with download WCOFS data')
    wcofsOK <- FALSE
}

cat('\nMaking individual drift plots...')
driftPlots <- character(0)
if(hycomOK) {
    driftPlots <- doDriftPlots(db, verbose=T, current=4, outDir='./DriftPlots/', dataPath = 'PlottingData')
}
if(hfradarOK) {
    driftPlots <- c(driftPlots,
                    doDriftPlots(db, verbose=T, current=3, outDir = './DriftPlots/', dataPath = 'PlottingData'))
}
cat('\nMaking last 2 weeks plot...')

noPlot <- c('ADRIFT_004', 'ADRIFT_008')

recentDrifts <- getDbDeployment(db, days=14, verbose=FALSE)
recentDrifts <- recentDrifts[!(recentDrifts$DriftName %in% noPlot), ]
if(nrow(recentDrifts) > 0) {
    twoWeekPlot <- plotAPIDrift(recentDrifts, filename='./DriftPlots/Last14Days.png', current=FALSE, dataPath = 'PlottingData')
} else {
    twoWeekPlot <- NULL
}
cat('\nMaking test deployment worksheet plots...')
testDepPlots <- character(0)
if(hycomOK) {
    cat('\nWith HYCOM...')
    testDepPlots <- plotTestDeployments(current=4, driftData=getDbDeployment(db, verbose=FALSE),
                                        outDir = './TestDeploymentPlots/', dataPath = 'PlottingData')
}
if(hfradarOK) {
    cat('\nWith HFRADAR...')
    testDepPlots <- c(testDepPlots,
                      plotTestDeployments(current=3, driftData=getDbDeployment(db, verbose=FALSE), 
                                          outDir = './TestDeploymentPlots/', dataPath = 'PlottingData'))
}
if(wcofsOK) {
    cat('\nWith WCOFS...')
    testDepPlots <- c(testDepPlots,
                      plotTestDeployments(current=6, driftData=getDbDeployment(db, verbose=FALSE), 
                                          outDir = './TestDeploymentPlots/', dataPath = 'PlottingData'))
}
# cat('\nMaking CCC plots...')
# ccc <- getDbDeployment(db, drift=paste0('ADRIFT_0', 19:26))
# cccPlotHYCOM <- plotAPIDrift(ccc, filename = 'AllCCC_HYCOM.png', current=4)
# cccPlotHFRADAR <- plotAPIDrift(ccc, filename = 'AllCCC_HFRADAR.png', current=3)
# driftPlots <- c(driftPlots, cccPlotHYCOM, cccPlotHFRADAR)

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
warns <- warnings()
if(length(warns) > 0) {
    print(warns)
}
cat('\nAll completed successfully!', as.character(Sys.time()))
