# cces missing 9-12-18 to 10-3-18 in my db, uploaded from hard drive CCES folder

gps <- getDbDeployment(db)

gaps <- bind_rows(lapply(split(gps, gps$DriftName), function(x) {
    tmp <- list(start=x$UTC[1:(nrow(x)-1)], 
                end = x$UTC[2:nrow(x)],
                drift = x$DriftName[2:nrow(x)])
    tmp$hours <- as.numeric(difftime(tmp$end, tmp$start, units='hours'))
    tmp
    # list(drift=x$DriftName[1],
         # mins = as.numeric(difftime(x$UTC[2:nrow(x)], x$UTC[1:(nrow(x)-1)], units='mins')))
}))

library(ggplot2)
gaps %>% 
    mutate(isBig = hours > 2) %>% 
    filter(isBig) %>% 
    ggplot() +
    geom_point(aes(x=drift, y=hours, col=drift))

unique(gaps$drift[gaps$mins > 3 * 60])
View(gaps[gaps$hours > 2,])

# AllTracks from Anne's email fixing
AllTracks$hour <- NA
AllTracks <- arrange(AllTracks, dateTime)
AllTracks$hour[1:(nrow(AllTracks)-1)] <- as.numeric(
    difftime(AllTracks$dateTime[2:nrow(AllTracks)],
             AllTracks$dateTime[1:(nrow(AllTracks)-1)],
             units = 'hours'))
range(AllTracks$hour, na.rm=T)

spotMap <- select(gps, DeviceName, DeviceId, DriftName) %>% distinct() %>% 
    filter(grepl('CCES', DriftName))

AllTracks <- left_join(AllTracks, spotMap, by=c('spotID'='DeviceId'))

noNa <- AllTracks[!is.na(AllTracks$DeviceName), ]
unique(AllTracks$station[is.na(AllTracks$DeviceName)])

tms <- as.POSIXct(c('2018-11-10  04:47:52', '2018-11-17  16:02:05',
             '2018-09-12  19:04:59', '2018-10-03  17:01:08'), format='%Y-%m-%d  %H:%M:%S', tz='UTC')

nov <- noNa[noNa$dateTime > tms[1] & noNa$dateTime < tms[2], ]
sep <- noNa[noNa$dateTime > tms[3] & noNa$dateTime < tms[4], ]
nrow(nov)
nrow(sep)

adds <- bind_rows(nov, sep)
adds <- select(adds, lat, long, dateTime, spotID, DeviceName)
adds <- distinct(adds)
con <- dbConnect(db, drv=SQLite())
dbGps <- dbReadTable(con, 'gpsData')
idMax <- max(dbGps$Id)
# colnames(adds)
# colnames(dbGps)
adds$Id <- 1:nrow(adds) + idMax
adds <- select(adds, Id, Latitude = lat, Longitude = long,
               UTC = dateTime, DeviceName, DeviceId = spotID)
adds$Message <- NA
adds$UTC <- format(adds$UTC, format='%Y-%m-%d %H:%M:%S')
dbAppendTable(con, 'gpsData', adds)

hm <- getDbDeployment(db, 'CCES_004')
plot(hm$UTC)
dbDisconnect(con)
