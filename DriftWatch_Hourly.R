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

cat('\n---------Script version', thisVersion(), 'run on', as.character(Sys.time()), 'with R version', R.version$version.string, '-----------')

cat('\nDirectory set to', script.dir)
db <- 'SPOTGPS_Logger.sqlite3'

with_drive_quiet({
    drive_auth(email=secrets$user_email, cache='.secrets',
               scopes="https://www.googleapis.com/auth/drive")
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
    driftPlots <- c(driftPlots,
                    doDriftPlots(db, verbose=T, current=4, outDir='./DriftPlots/', dataPath = 'PlottingData'))
}
if(hfradarOK) {
    driftPlots <- c(driftPlots,
                    doDriftPlots(db, verbose=T, current=3, outDir = './DriftPlots/', dataPath = 'PlottingData'))
}
if(wcofsOK) {
    driftPlots <- c(driftPlots,
                    doDriftPlots(db, verbose=T, current=6, outDir = './DriftPlots/', dataPath = 'PlottingData'))
}
combDrift <- combineCurrents(driftPlots, outDir='./DriftPlots/')
cat('\nMaking last 2 weeks plot...')

noPlot <- c('ADRIFT_004', 'ADRIFT_008')

recentDrifts <- getDbDeployment(db, days=14, verbose=FALSE)
recentDrifts <- recentDrifts[!(recentDrifts$DriftName %in% noPlot), ]
if(nrow(recentDrifts) > 0) {
    twoWeekPlot <- plotAPIDrift(recentDrifts, 
                                filename='./DriftPlots/Last14Days.png', 
                                xlim=2,
                                current=FALSE, 
                                dataPath = 'PlottingData')
} else {
    twoWeekPlot <- NULL
}

cat('\nUploading drift plots...')
doGdriveUpload(c(combDrift, twoWeekPlot), as_id('10LOJhEUO6mfjY0LmXwmm2iu0JH70uj6x')) # drift plots folder

cat('\nMaking test deployment worksheet plots...')
testDepPlots <- character(0)
if(hycomOK) {
    cat('\nWith HYCOM...')
    testDepPlots <- c(testDepPlots,
                      plotTestDeployments(current=4, driftData=getDbDeployment(db, verbose=FALSE),
                                        outDir = './TestDeploymentPlots/', dataPath = 'PlottingData'))
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
combTestDep <- combineCurrents(testDepPlots, outDir='./TestDeploymentPlots/')
# cat('\nMaking CCC plots...')
# ccc <- getDbDeployment(db, drift=paste0('ADRIFT_0', 19:26))
# cccPlotHYCOM <- plotAPIDrift(ccc, filename = 'AllCCC_HYCOM.png', current=4)
# cccPlotHFRADAR <- plotAPIDrift(ccc, filename = 'AllCCC_HFRADAR.png', current=3)
# driftPlots <- c(driftPlots, cccPlotHYCOM, cccPlotHFRADAR)

cat('\nUploading test plots to drive...')
doGdriveUpload(combTestDep, as_id('1fbufTIudlMbbUcHcrfvN0J35_mw4jE7V')) # test deployment folder
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
