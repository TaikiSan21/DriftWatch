

db <- 'SPOTGPS_Logger.sqlite3'
spotId <- '09m8vfKzAyrx3j1sSqVMCDamuAJKln1ys'
update <- addAPIToDb(id=spotId, db=db)




# Deploy sheet
newDep <- drive_find('Deployment Details', type='spreadsheet')
if(nrow(newDep) > 1) {
    warning('More than one Deployment Details sheet found, using first (Found ', nrow(newDep), ')')
    newDep <- newDep[1,]
}
gs4_auth(email='taiki.sakai@noaa.gov')
newDepSheet <- read_sheet(newDep, sheet='NEW DEPLOYMENT TO SAVE', range='A:AQ', col_types='c')
depDetail <- read_sheet(newDep, sheet='deployDetails', col_types='c')
depSheets <- bind_rows(sheetToDbDepFormat(depDetail, FALSE),
                       sheetToDbDepFormat(newDepSheet, TRUE))

con <- dbConnect(db, drv=SQLite())
dbDep <- dbReadTable(con, 'deploymentData')
dbDep$Start <- as.POSIXct(dbDep$Start, format='%Y-%m-%d %H:%M:%S', tz='UTC')
dbDep$End <- as.POSIXct(dbDep$End, format='%Y-%m-%d %H:%M:%S', tz='UTC')
newDeps <- !depSheets$DriftName %in% dbDep$DriftName
if(any(newDeps)) {
    toAdd <- depSheets[newDeps, ]
    toAdd$Id <- max(dbDep$Id) + 1:nrow(toAdd)
    toAdd <- toAdd[colnames(dbDep)]
    added <- dbAppendTable(con, 'deploymentData', toAdd)
    dbClearResult(added)
}

# Adjusting these to do DataStart / RecoveryDate as preferred
# Anne is checking mismatche issues
for(i in 1:nrow(dbDep)) {
    newVals <- character(0)
    thisDrift <- depSheets[depSheets$DriftName == dbDep$DriftName[i], ]
    if(nrow(thisDrift) == 0) next
    if(!identical(thisDrift$DeploymentSite, dbDep$DeploymentSite[i]) &
       !is.na(thisDrift$DeploymentSite)) {
        newVals <- c(newVals, paste0('DeploymentSite = "', thisDrift$DeploymentSite, '"'))
    }
    if(!identical(thisDrift$Start, dbDep$Start[i]) &
       !is.na(thisDrift$Start)) {
        newVals <- c(newVals, paste0('Start = "', thisDrift$Start, '"'))
    }
    if(!identical(thisDrift$End, dbDep$End[i]) &
       !is.na(thisDrift$End)) {
        newVals <- c(newVals, paste0('End = "', thisDrift$End, '"'))
    }
    if(length(newVals) == 0) next
    newVals <-paste0(newVals, collapse=', ')
    sqlUpd <- sprintf('UPDATE deploymentData SET %s WHERE DriftName = "%s";', newVals, dbDep$DriftName[i])
    cat(sqlUpd, '\n')
}
# THESE DONT ALWAYS ALIGN WITH WHAT WE HAVE NOT SURE IF WE SHOULD ACTUALLY DO
# THE DATASTART/END THING BECAUSE WE HAD ONES THAT STOPPED RECORDING BUT
# STILL DRIFTED

dbDisconnect(con)



depGps <- getDbDeployment(db)
plotAPIDrift(depGps[depGps$DriftName == 'ADRIFT_011',], current = 4, filename = 'Test12.png')

library(taskscheduleR)
hrlyScript <- normalizePath('DriftWatch_Hourly.R')
taskscheduler_create(taskname = "DriftWatchHourly", rscript = hrlyScript,
                     schedule = "HOURLY", starttime = format(Sys.time() + 62, "%H:%M"),
                     startdate = format(Sys.Date(), '%m-%d-%Y'))

'UPDATE table_name
SET column1 = value1, column2 = value2, ...
WHERE condition;'


# HYCOM NOTES
#* THREDDS servers update/restart daily between 2:00 AM - 3:00 AM, and 12:00 PM - 1:00 PM EST/EDT.
#* Also dont subset more than 1 day at a time is their request wit NCSS
