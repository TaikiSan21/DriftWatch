drive_auth(cache='.secrets', email=TRUE)
gs4_auth(token = drive_token())

# create sheet for each user with
SHEETID <- as_id('1qJQ2f7b8qCo90ewtYlPCmlX2_iiav-qxaLaWErhlmlE')
sched <- read_sheet(ss=SHEETID, sheet='Schedule')
tz(sched$Start_Local) <- 'America/Los_Angeles'
tz(sched$Stop_Local) <- 'America/Los_Angeles'
# sched$Start_Local <- as.POSIXct(sched$Start_Local, format='%Y-%m-%d %H:%M:%S', tz='America/Los_Angeles')
# sched$Stop_Local <- as.POSIXct(sched$Stop_Local, format='%Y-%m-%d %H:%M:%S', tz='America/Los_Angeles')
contact <- read_sheet(ss=SHEETID, sheet='Contacts')
dayMax <- 1
for(i in 1:nrow(sched)) {
    if(with_tz(sched$Start_Local[i], 'UTC') > nowUTC()) next
    if(with_tz(sched$Stop_Local[i], 'UTC') < nowUTC()) next
    gps <- getDbDeployment(db, drift=sched$Deployment_Name, days=dayMax)
    if(nrow(gps) == 0) {
        message <- paste0('No update in last ', dayMax, ' day(s)')
    } else {
        info <- getLastUpdate(gps, gpsFmt = 'dms')
        message <- with(info,
                        paste0('Drift ', drift, ' Last Update ',
                               date, ': ',
                               position[1], ', ', position[2], ' - ',
                               'Bearing ', direction, ' Degrees ', speed, ' Knots')
        )
        
    }
    print(message)
    emOut <- sendTurboEmail(to=contact$Email[contact$Id %in% sched$Recipient_Id], message=message)
}



lat <- 122.999
fmtCoord(lat, 'dms')
as.character(round(lat, 3))
gps <- getDbDeployment(db, drift='ADRIFT_024')
gps <- arrange(gps, desc(UTC))
getLastUpdate(gps, last=2)

ggplot(head(gps, 3)) +
    geom_point(aes(x=Longitude, y=Latitude, col=UTC))

library(taskscheduleR)
taskscheduler_create(taskname = "DriftWatch_30min", rscript = normalizePath('DriftWatch_30min.R'), 
                     schedule = "HOURLY", starttime = format(Sys.time() + 62, "%H:%M"), startdate = format(Sys.Date(), '%m-%d-%Y'))
