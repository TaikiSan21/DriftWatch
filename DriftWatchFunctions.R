suppressPackageStartupMessages({
    library(dplyr)
    library(RSQLite)
    # library(plotKML)
    library(xml2)
    library(marmap)
    library(sf)
    library(viridis)
    library(PAMmisc)
    library(googledrive)
    library(tibble)
    library(geosphere)
    library(readr)
    library(googlesheets4)
    library(ncdf4)
    library(httr)
    library(rjson)
    library(readxl)
    library(lubridate)
    library(yaml)
    library(RNetCDF)
    library(ggplot2)
    library(patchwork)
    library(png)
    library(grid)
    library(gridExtra)
    library(stringr)
})
# Updated 1-19-22 post collapse v 1.0
# Updated 2-7-2022 fkin etopos
# Updated 5-2-2022 adding Lonestar
# Updated 6-10-2022 Trying to sort out crashes on first load of the day and custom plots on test dep sheet
# Updated 6-14-2022 DB now has Lonestar names instead of dumb ID
# Updated 9-22-2022 To do GPS CSV. Also moved GPS downloads to 30 minutes w/texting
# Updated 10-10-2023 Forgot these, but currently combining diff currents into 1 plot

# this stores api keys and PWs for things - not to be shared
secrets <- read_yaml('.secrets/secrets.yaml')
thisVersion <- function() {
    '1.5'
}

getSpotAPIData <- function(id=secrets$spot_key, start=1) {
    if(is.null(id)) {
        id <- secrets$spot_key
    }
    call <- makeSpotCall(id, where=start)
    xml <- try(read_xml(call))
    if(inherits(xml, 'try-error')) {
        return(NULL)
    }
    df <- spotXmlToDf(xml)
    if(is.null(df)) {
        return(NULL)
    }
    df
}

getLonestarAPIData <- function(key=secrets$lonestar_key, start=NULL, days=30) {
    if(is.null(key)) {
        key <- secrets$lonestar_key
    }
    if(is.null(start)) {
        start <- nowUTC() - days * 24 * 3600
    }
    keyChar <- paste0('?key=', key)
    routeURL <- 'https://fleetone.lonestartracking.com/api/v1/route/list.json'
    if(days > 30) {
        days <- 30
    }
    end <- start + days * 24 * 3600
    fromChar <- paste0('&from=', PAMmisc:::fmtPsx8601(start))
    tillChar <- paste0('&till=', PAMmisc:::fmtPsx8601(end))
    fromTil <- GET(paste0(routeURL, keyChar, fromChar, tillChar))
    if(fromTil$status_code != 200) {
        cat('\nLonestar API access attempt was not successful')
        return(NULL)
    }
    ftJSON <- fromJSON(rawToChar(fromTil$content))
    
    lsDf <- lsToDf(ftJSON)
    if(is.null(lsDf) ||
       nrow(lsDf) == 0) {
        return(NULL)
    }
    # lsDf <- mapLonestarId(lsDf, db)
    lsDf
}

mapLonestarId <- function(x, db) {
    if(is.null(x) ||
       nrow(x) == 0) {
        return(x)
    }
    con <- dbConnect(db, drv=SQLite())
    on.exit(dbDisconnect(con))
    map <- dbReadTable(con, 'lonestarMap')
    for(i in unique(x$DeviceId)) {
        if(!i %in% map$API_Id) next
        x$DeviceName[x$DeviceId == i] <- map$DeviceName[map$API_Id == i]
    }
    x
}
getLonestarAPISingle <- function(key=secrets$lonestar_key, unit, time) {
    if(length(time) > 1) {
        cat('\nCan only download one manual time per request')
        time <- time[1]
    }
    if(length(unit) > 1) {
        result <- bind_rows(lapply(unit, function(u) {
            getLonestarAPISingle(key=key, unit=u, time=time)
        }))
        result$Id <- 1:nrow(result)
        return(result)
    }
    if(is.null(key)) {
        key <- secrets$lonestar_key
    }
    keyChar <- paste0('?key=', key)
    routeURL <- 'https://fleetone.lonestartracking.com/api/v1/unit_data/history_point.json'
    unitChar <- paste0('&unit_id=', unit)
    timeChar <- paste0('&datetime=', PAMmisc:::fmtPsx8601(time))
    includeChar <- '&include[]=position'
    fromTil <- GET(paste0(routeURL, keyChar, unitChar, timeChar, includeChar))
    if(fromTil$status_code != 200) {
        cat('\nLonestar API access attempt was not successful')
        return(NULL)
    }
    ftJSON <- fromJSON(rawToChar(fromTil$content))
    
    lsDf <- singleToDf(ftJSON)
    if(is.null(lsDf) ||
       nrow(lsDf) == 0) {
        return(NULL)
    }
    lsDf
}

singleToDf <- function(x) {
    position <- x$data$units[[1]]$position
    result <- list(UTC=position$gmt, 
                   Latitude=position$value$lat,
                   Longitude=position$value$lng)
    result$UTC <- as.POSIXct(result$UTC, format='%Y-%m-%d %H:%M:%S', tz='UTC')
    result$DeviceId <- as.character(x$data$units[[1]]$unit_id)
    result$Message <- NA
    result$DeviceName <- NA
    result$Id <- 1
    result <- bind_rows(result)
    result[c('Id', 'Latitude', 'Longitude', 'UTC', 'DeviceName', 'DeviceId', 'Message')]
}

# input json from ls API
lsToDf <- function(x, start=TRUE) {
    result <- dplyr::bind_rows(lapply(x$data$units, function(u) {
        data <- dplyr::bind_rows(lapply(u$routes, function(r) {
            startEnd <- ifelse(start, 'start', 'end')
            result <- r[[startEnd]][c('time', 'lat', 'lng')]
            # cat(r$route_id, '\n')
            if(is.null(result)) {
                return(NULL)
            }
            names(result) <- c('UTC', 'Latitude', 'Longitude')
            result$Id <- r$route_id
            result
        }))
        data$UTC <- as.POSIXct(data$UTC, format='%Y-%m-%dT%H:%M:%SZ', tz='UTC')
        data$DeviceName <- as.character(u$unit_id)
        data$DeviceName <- NA
        data$DeviceId <- as.character(u$unit_id)
        data$Message <- NA
        data
    }))
    if(is.null(result) ||
       nrow(result) == 0) {
        return(result)
    }
    result <- result[c('Id', 'Latitude', 'Longitude', 'UTC', 'DeviceName', 'DeviceId', 'Message')]
    result
}

getAPIData <- function(key=NULL, db, source=c('spot', 'lonestar', 'manualLS'), unit=NULL, time=NULL, progress=FALSE) {
    source <- match.arg(source)
    switch(source,
           'spot' = {
               start <- 1
               df <- getSpotAPIData(key, start=start)
               nFours <- 60
               tryMax <- 15
           },
           'lonestar' = {
               days <- 29
               start <- nowUTC() - days * 24 * 3600
               df <- getLonestarAPIData(key, start=start, days=days)
               nFours <- 15
               tryMax <- 12
           },
           'manualLS' = {
               df <- getLonestarAPISingle(key, unit=unit, time=time)
               tryMax <- 1
               nFours <- 15
           }
    )
    if(is.character(db)) {
        con <- dbConnect(db, drv=SQLite())
        on.exit(dbDisconnect(con))
        dbDf <- dbReadTable(con, 'gpsData')
    } else {
        dbDf <- db
    }
    isDupe <- checkDupeCoord(df, dbDf)
    if(all(isDupe)) {
        return(NULL)
    }
    toAdd <- df[!isDupe, ]
    # toAdd$UTC <- as.character(toAdd$UTC)
    # go again if everything was new - means more to get
    # if(start == 1) {
    # tryMax <- 10
    nTry <- 1
    while(!any(isDupe) &&
          nTry < tryMax) {
        # cat(sum(isDupe))
        cat(paste0('\nWaiting between ', source, ' API calls...\n'))
        if(progress) {
            pb <- txtProgressBar(min=0, max=nFours, initial=0, style=3)
        }
        for(i in 1:nFours) {
            Sys.sleep(4)
            if(progress) {
                setTxtProgressBar(pb, value=i)
            }
        }
        switch(source,
               'spot' = {
                   start <- start + 40
                   newData <- getSpotAPIData(key, start=start)
               },
               'lonestar' = {
                   start <- start - days * 24 * 3600
                   newData <- getLonestarAPIData(key, start=start, days=days)
               }
        )
        
        if(is.null(newData) ||
           nrow(newData) == 0) {
            break
        }
        isDupe <- checkDupeCoord(newData, db)
        if(all(isDupe)) {
            break
        }
        newData <- newData[!isDupe, ]
        # newData$UTC <- as.character(newData$UTC)
        toAdd <- distinct(bind_rows(toAdd, newData))
        if(any(isDupe)) {
            break
        }
        nTry <- nTry + 1
    }
    badCoords <- toAdd$Latitude < -90 | toAdd$Longitude < -180
    toAdd <- toAdd[!badCoords, ]
    toAdd <- arrange(toAdd, UTC)
    toAdd$UTC <- format(toAdd$UTC, format='%Y-%m-%d %H:%M:%S')
    toAdd
}

# x is key or id for API
addAPIToDb <- function(key='', db, source=c('spot', 'lonestar', 'manualLS'), unit=NULL, time=nowUTC()) {
    con <- dbConnect(db, drv=SQLite())
    on.exit(dbDisconnect(con))
    dbDf <- dbReadTable(con, 'gpsData')
    dbDf$UTC <- pgDateToPosix(dbDf$UTC)
    source <- match.arg(source)
    # map given name back to API ID to give to manualLS mode
    if(!is.null(unit)) {
        map <- dbReadTable(con, 'lonestarMap')
        
        for(i in seq_along(unit)) {
            if(!unit[i] %in% map$DeviceName) next
            unit[i] <- map$API_Id[map$DeviceName == unit[i]]
        }
    }
    apiData <- getAPIData(key, dbDf, source, unit=unit, time=time)
    apiData <- mapLonestarId(apiData, db)
    if(!is.null(apiData) &&
       nrow(apiData) > 0) {
        overlapId <- apiData$Id %in% dbDf$Id
        if(any(overlapId)) {
            possId <- 1:(nrow(apiData)+nrow(dbDf))
            possId <- possId[!possId %in% dbDf$Id]
            apiData$Id[overlapId] <- possId[1:sum(overlapId)]
        }
        dbAppendTable(con, 'gpsData', apiData)
    }
    dbLog <- dbReadTable(con, 'gpsLogger')
    logId <- ifelse(nrow(dbLog) == 0, 1, max(dbLog$Id) + 1)
    now <- nowUTC()
    sourceName <- switch(source,
                         'spot' = 'SPOT API',
                         'lonestar' = 'Lonestar API',
                         'manualLS' = 'Lonestar Manual API'
    )
    logAppend <- data.frame(Id = logId, UTC = format(now, format='%Y-%m-%d %H:%M:%S'),
                            RowsAdded = ifelse(is.null(apiData), 0, nrow(apiData)),
                            Source = sourceName)
    dbAppendTable(con, 'gpsLogger', logAppend)
    apiData
}

psxToSpot <- function(x) {
    x <- format(x,format='%Y-%m-%d %H:%M:%S')
    x <- gsub(' ', 'T', x)
    x <- paste0(x, '-0000')
    x
}

makeSpotCall <- function(id, where=1, date=FALSE) {
    spotBase <- 'https://api.findmespot.com/spot-main-web/consumer/rest-api/2.0/public/feed/'
    spotTail <- '/message.xml'
    mainCall <- paste0(spotBase, id, spotTail)
    if(isFALSE(date)) {
        if(!is.null(where) &&
           is.numeric(where) &&
           where > 1) {
            mainCall <- paste0(mainCall, '?start=', where)
        }
        return(mainCall)
    }
    if(isTRUE(date)) {
        if(!is.null(where) &&
           inherits(where, 'POSIXct') &&
           length(where) == 2) {
            mainCall <- paste0('?startDate=',
                               psxToSpot(where[1]),
                               '&endDate=',
                               psxToSpot(where[2]))
        }
        return(mainCall)
    }
    NULL
}

spotXmlToDf <- function(x) {
    xmlMessages <- xml_find_all(x, '//messages/message')
    if(length(xmlMessages) == 0) {
        return(NULL)
    }
    xmlMessages <- as_list(xmlMessages)
    
    df <- bind_rows(lapply(xmlMessages, function(i) {
        as.list(unlist(i))[c('id', 'messengerId', 'messengerName', 'messageType', 'latitude',
                             'longitude', 'dateTime', 'batteryState')]
    }))
    if(is.null(df) ||
       nrow(df) == 0 ||
       ncol(df) < 8) {
        return(NULL)
    }
    df$dateTime <- as.POSIXct(df$dateTime, format='%Y-%m-%dT%H:%M:%S+0000', tz='UTC')
    names(df) <- c('Id', 'DeviceId', 'DeviceName', 'MessageType', 'Latitude', 'Longitude', 'UTC', 'BatteryState')
    df$Message <- paste0('MessageType: ',df$MessageType, ', Battery State: ', df$BatteryState)
    df <- df[c('Id', 'Latitude', 'Longitude', 'UTC', 'DeviceName', 'DeviceId', 'Message')]
    df$Latitude <- as.numeric(df$Latitude)
    df$Longitude <- as.numeric(df$Longitude)
    df$Id <- as.numeric(df$Id)
    df
}

# gpxToDf <- function(x) {
#     gpx <- plotKML::readGPX(x)
#     format <- '%Y-%m-%dT%H:%M:%SZ'
#     result <- do.call(rbind, lapply(gpx$tracks, function(x) {
#         tmp <- vector('list', length = length(x))
#         for(i in seq_along(x)) {
#             df <- x[[i]][, c('lon', 'lat', 'time')]
#             df$DeviceName <- names(x)[i]
#             tmp[[i]] <- df
#         }
#         bind_rows(tmp)
#     }))
#     colnames(result) <- c('Longitude', 'Latitude', 'UTC', 'DeviceName')
#     result$UTC <- as.POSIXct(result$UTC, tz='UTC', format=format)
#     badCoords <- result$Latitude < -90 | result$Longitude < -180
#     result[!badCoords, ]
# }

# addGpxToDb <- function(gpx, db) {
#     gpxDf <- gpxToDf(gpx)
#     con <- dbConnect(db, drv=SQLite())
#     on.exit(dbDisconnect(con))
#     dbDf <- dbReadTable(con, 'gpsData')
#     dbDf$UTC <- pgDateToPosix(dbDf$UTC)
#     isDupe <- checkDupeCoord(gpxDf, dbDf)
#     if(!all(isDupe)) {
#         toAdd <- gpxDf[!isDupe, ]
#         gpsAppend <- dbDf[FALSE, ]
#         gpsAppend[1:nrow(toAdd), ] <- NA
#         gpsAppend$Latitude <- toAdd$Latitude
#         gpsAppend$Longitude <- toAdd$Longitude
#         gpsAppend$DeviceName <- toAdd$DeviceName
#         gpsAppend <- arrange(gpsAppend, UTC)
#         gpsAppend$UTC <- format(toAdd$UTC, format='%Y-%m-%d %H:%M:%S')
#         ixPoss <- 1:(nrow(dbDf) + nrow(toAdd))
#         ixPoss <- ixPoss[!(ixPoss %in% dbDf$Id)]
#         gpsAppend$Id <- ixPoss[1:nrow(toAdd)]
#         dbAppendTable(con, 'gpsData', gpsAppend)
#     } else {
#         gpsAppend <- NULL
#     }
#     dbLog <- dbReadTable(con, 'gpsLogger')
#     logId <- ifelse(nrow(dbLog) == 0, 1, max(dbLog$Id) + 1)
#     now <- nowUTC()
#     logAppend <- data.frame(Id = logId, UTC = format(now, format='%Y-%m-%d %H:%M:%S'),
#                             RowsAdded = ifelse(all(isDupe), 0, nrow(gpsAppend)),
#                             Source = 'GPX')
#     dbAppendTable(con, 'gpsLogger', logAppend)
#     gpsAppend
# }

nowUTC <- function() {
    now <- Sys.time()
    attr(now, 'tzone') <- 'UTC'
    now
}

# returns which rows are duplicated in the first df
checkDupeCoord <- function(x, y, coords=c('UTC', 'Longitude', 'Latitude')) {
    if(is.null(x) ||
       nrow(x) == 0) {
        return(logical(0))
    }
    xCoords <- x[coords]
    yCoords <- y[coords]
    duplicated(rbind(xCoords, yCoords), fromLast=TRUE)[1:nrow(xCoords)]
}

getDbGps <- function(db, days=NULL) {
    con <- dbConnect(db, drv=SQLite())
    on.exit(dbDisconnect(con))
    gps <- dbReadTable(con, 'gpsData')
    gps$UTC <- pgDateToPosix(gps$UTC)
    if(is.null(days)) {
        return(gps)
    }
    now <- nowUTC()
    filter(gps, UTC >= (now - days * 24 * 3600))
}

getDeploymentData <- function(db) {
    con <- dbConnect(db, drv=SQLite())
    on.exit(dbDisconnect(con))
    deployment <- dbReadTable(con, 'deploymentData')
    # browser()
    deployment$Start <- pgDateToPosix(deployment$Start)
    deployment$End <- pgDateToPosix(deployment$End)
    deployment$DataStart <- pgDateToPosix(deployment$DataStart)
    deployment$DataEnd <- pgDateToPosix(deployment$DataEnd)
    deployment
}

allZero <- function(x) {
    if(is.na(x)) {
        return(TRUE)
    }
    hour(x) == 0 &&
        minute(x) == 0 &&
        second(x) == 0
}

getDbDeployment <- function(db, drift=NULL, days=NULL, knotsLimit=4, verbose=TRUE) {
    con <- dbConnect(db, drv=SQLite())
    on.exit(dbDisconnect(con))
    gps <- dbReadTable(con, 'gpsData')
    gps$UTC <- pgDateToPosix(gps$UTC)
    deployment <- getDeploymentData(db)
    if(!is.null(drift)) {
        deployment <- deployment[deployment$DriftName %in% drift, ]
    }
    noStart <- is.na(deployment$Start)
    deployment <- deployment[!noStart, ]
    if(is.null(deployment) ||
       nrow(deployment) == 0) {
        return(NULL)
    }
    if(any(is.na(deployment$End))) {
        deployment$End[is.na(deployment$End)] <- nowUTC()
    }
    allGps <- bind_rows(lapply(deployment$DriftName, function(x) {
        thisDep <- deployment[deployment$DriftName == x, ]
        if(thisDep$Start == thisDep$End) {
            return(NULL)
        }
        if(allZero(thisDep$End)) {
            thisDep$End <- thisDep$End + 24*3600 - 1
        }
        thisGps <- gps[gps$DeviceName %in% gsub(' ', '', strsplit(thisDep$DeviceName, ',')[[1]]), ]
        thisGps <- thisGps[thisGps$UTC >= thisDep$Start &
                               thisGps$UTC <= thisDep$End, ]
        if(nrow(thisGps) == 0) {
            if(verbose) {
                cat('\nNo data in database for drift', x)
            }
            return(NULL)
        }
        thisGps$recordingEffort <- FALSE
        if(!is.na(thisDep$DataStart) &&
           !is.na(thisDep$DataEnd)) {
            thisGps$recordingEffort <- thisGps$UTC >= thisDep$DataStart &
                thisGps$UTC <= thisDep$DataEnd
        }
        thisGps <- dropBySpeed(thisGps, knots=knotsLimit)
        thisGps <- arrange(thisGps, UTC)
        thisGps$DriftName <- x
        thisGps$DeploymentSite <- unique(thisDep$DeploymentSite)
        if(nrow(thisGps) > 1) {
        ix1 <- 1:(nrow(thisGps)-1)
        ix2 <- 2:nrow(thisGps)
        thisGps$bearing <-
            c(bearing(matrix(c(thisGps$Longitude[ix1],
                               thisGps$Latitude[ix1]), ncol=2),
                      matrix(c(thisGps$Longitude[ix2],
                               thisGps$Latitude[ix2]), ncol=2)),
              NA)
        thisGps$distance <- c(distGeo(
            matrix(c(thisGps$Longitude[ix1], thisGps$Latitude[ix1]), ncol=2),
            matrix(c(thisGps$Longitude[ix2], thisGps$Latitude[ix2]), ncol=2)),
            NA)
        thisGps$bearing <- thisGps$bearing %% 360
        } else {
            thisGps$bearing <- NA
            thisGps$distance <- NA
        }
        thisGps
    }))
    if(is.null(days)) {
        return(allGps)
    }
    filter(allGps, UTC >= (nowUTC() - days * 24 * 3600))
}

calcKnots <- function(x) {
    if(nrow(x) == 1) {
        return(NA)
    }
    ix1 <- 1:(nrow(x)-1)
    ix2 <- 2:(nrow(x))
    # diff <- sqrt((x$Latitude[ix1] - x$Latitude[ix2])^2 +
    # (x$Longitude[ix1] - x$Longitude[ix2])^2)
    diff <- distGeo(
        matrix(c(x$Longitude[ix1], x$Latitude[ix1]), ncol=2),
        matrix(c(x$Longitude[ix2], x$Latitude[ix2]), ncol=2))
    # tDiff <- abs(as.numeric(x$UTC[ix1]) - as.numeric(x$UTC[ix2]))
    tDiff <- abs(as.numeric(difftime(x$UTC[ix1], x$UTC[ix2], units='secs')))
    diff <- ifelse(tDiff == 0, 0, diff / tDiff)
    # convert from dec.deg/s to ~km/h 111km/dd
    # c(NA, diff) * 111 * 3600
    # convert m/s to knots * 1.94
    c(NA, diff) * 1.94
}

dropBySpeed <- function(x, knots=4) {
    if(nrow(x) == 1) {
        return(x)
    }
    if(length(unique(x$DeviceName)) > 1) {
        return(
            bind_rows(lapply(split(x, x$DeviceName), function(d) {
                dropBySpeed(d, knots)
            }))
        )
    }
    x <- arrange(x, UTC)
    tDiff <- as.numeric(difftime(x$UTC[2:nrow(x)], x$UTC[1:(nrow(x)-1)], units='secs'))
    x <- x[c(TRUE, abs(tDiff) > 5),] 
    x$knots <- calcKnots(x)
    whichHigh <- which(x$knots > knots)
    if(length(whichHigh) == 0) {
        # x$knots <- NULL
        return(x)
    }
    # check for consecutives and drop them
    consec <- union(intersect(whichHigh, whichHigh+1),
                    intersect(whichHigh, whichHigh-1))
    if(length(consec) > 0) {
        return(dropBySpeed(x[-consec, ], knots))
    }
    # if(nrow(x) %in% whichHigh) {
    #     return(dropBySpeed(x[-nrow(x), ], knots))
    # }
    # non-consec ones are ambiguous which of 2 points are the outlier, so drop by
    # which has the higher z-score
    zLat <- abs((x$Latitude - mean(x$Latitude, na.rm=TRUE)) / sd(x$Latitude, na.rm=TRUE))
    zLon <- abs((x$Longitude - mean(x$Longitude, na.rm=TRUE)) / sd(x$Longitude, na.rm=TRUE))
    drop <- numeric()
    for(i in seq_along(whichHigh)) {
        z_curr <- max(zLat[whichHigh[i]], zLon[whichHigh[i]])
        z_bef <- max(zLat[whichHigh[i]-1], zLon[whichHigh[i]-1])
        drop[i] <- ifelse(z_curr > z_bef, whichHigh[i], whichHigh[i]-1)
    }
    x <- x[-drop, ]
    dropBySpeed(x, knots)
}

plotBathy <- function(gps, etopo='etopo180.nc', xlim=NULL, ylim=NULL, size=4, bathy=TRUE) {
    
    rangeDf <- makeRangeDf()
    
    if(!file.exists(etopo)) {
        updateNc(etopo, 'etopo180', vars=TRUE)
    }
    if(is.null(xlim)) {
        xlim <- range(gps$Longitude)
    }
    if(is.null(ylim)) {
        ylim <- range(gps$Latitude)
    }
    if(length(xlim) == 1) {
        xlim <- range(gps$Longitude) + c(-1, 1) * xlim
    }
    if(length(ylim) == 1) {
        ylim <- range(gps$Latitude) + c(-1, 1) * ylim
    }
    if(xlim[2] < rangeDf$Longitude[1] ||
       xlim[1] > rangeDf$Longitude[2] ||
       ylim[2] < rangeDf$Latitude[1] ||
       ylim[1] > rangeDf$Latitude[2]) {
        cat('\nPlot is entirely out of range: ',
            paste0(xlim, collapse=' -> '), ', ',
            paste0(ylim, collapse=' -> '), ' vs. (-135 -> -108, 20 -> 55)',
            sep='')
        return(NULL)
    }
    xlim[xlim < rangeDf$Longitude[1]] <- rangeDf$Longitude[1]
    xlim[xlim > rangeDf$Longitude[2]] <- rangeDf$Longitude[2]
    ylim[ylim < rangeDf$Latitude[1]] <- rangeDf$Latitude[1]
    ylim[ylim > rangeDf$Latitude[2]] <- rangeDf$Latitude[2]
    
    bathyData <- as.bathy(raster::raster(etopo))
    
    bathyData <- try(subsetBathy(bathyData, x=xlim, y=ylim, locator = FALSE), silent=TRUE)
    if(inherits(bathyData, 'try-error')) {
        cat('\nBathymetry subset failed for coordinates:',
            paste0(xlim, collapse=' -> '), ', ',
            paste0(ylim, collapse=' -> ')
        )
        return(NULL)
    }
    wid <- nrow(bathyData)
    xmar <- 1.24
    ht <- ncol(bathyData)
    ymar <- 1.84
    if(wid <= ht) {
        width <- size
        height <- size * ht / wid
    } else {
        height <- size
        width <- size * wid / ht
    }
    # if(!is.null(filename)) {
    #     tryOpen <- suppressWarnings(try(
    #         png(filename, height = height + ymar, width = width + xmar, units='in',res=300)
    #     ))
    #     if(inherits(tryOpen, 'try-error')) {
    #         warning('Unable to create image "', filename, '", file appears to be open already')
    #         return(NULL)
    #     }
    #     on.exit(dev.off())
    # }
    # Setting up bathy data and legend
    if(bathy) {
        depthPal <- list(c(0, max(bathyData), grey(.7), grey(.9), grey(.95)),
                         c(min(bathyData), 0, "darkblue", "lightblue"))
        # bVals <- round(seq(from=0, to=abs(min(bathyData)), length.out=7), 0)
        # bVals[c(2,4,6)] <- NA
        # bCols <- colorRampPalette(c('lightblue', 'darkblue'))(7)
    } else {
        depthPal <- list(c(0, max(bathyData), grey(.7), grey(.9), grey(.95)),
                         c(min(bathyData), -200, "skyblue1", 'skyblue1'),
                         c(-200, 0,"lightblue", "lightblue"))
        # bVals <- round(c(0, NA, 200, NA, abs(min(bathyData))), 0)
        # bCols <- c(rep('lightblue', 3), rep('skyblue1', 2))
    }
    
    # plot(bathyData, image = TRUE, land = TRUE, axes = T, lwd=0.3,
    #      bpal = depthPal, lty=1,
    #      shallowest.isobath=-100, deepest.isobath=-200, step=100, drawlabels=T)
    plot(bathyData, image = TRUE, land = TRUE, axes = T, lwd=0.3,
         bpal = depthPal, lty=1,
         shallowest.isobath=0, deepest.isobath=0, step=100, drawlabels=F)
    # lastLegend <- legend(x='topright', legend=c('Start', 'End', unique(drift[[labelBy]])),
    #                      col=c('black', 'grey', rep('black', length(mapLabs))), pch=c(15, 17, mapLabs), merge=FALSE,
    #                      seg.len = 1, cex=1, plot=FALSE)
    # useCex <- min(.2 * diff(xlim) / lastLegend$rect$w, 1)
    # lastLegend <- legend(x='topright', legend=c('Start', 'End', unique(drift[[labelBy]])),
    #                      col=c('black', 'grey', rep('black', length(mapLabs))), pch=c(15, 17, mapLabs), merge=FALSE,
    #                      seg.len = 1, cex=useCex)
    # 
    # if(legend) {
    #     le
    # }
    # if(bathy) {
    #     lastLegend <- myGradLegend(lastLeg=lastLegend, vals=bVals, cols=bCols,
    #                                title='      Depth (m)', cex=useCex, tCex=.67)
    # }
}

plotAPIDrift <- function(drift, etopo = 'etopo180.nc', filename=NULL, bathy=TRUE, sl=TRUE, wca=TRUE,
                         nms=FALSE, current=4, wind=FALSE, swell=FALSE, wave=FALSE, depth=0, time=nowUTC(),
                         size = 4, xlim=1, ylim=.5, labelBy='DriftName', title=NULL,
                         dataPath='PlottingData', simple=FALSE) {
    if(is.null(etopo)) {
        etopo <- file.path(dataPath, 'etopo180.nc')
    } else {
        etopo <- file.path(dataPath, etopo)
    }
    rangeDf <- makeRangeDf()
    
    if(!file.exists(etopo)) {
        updateNc(etopo, 'etopo180', vars=TRUE)
    }
    if(is.null(xlim)) {
        xlim <- range(drift$Longitude)
    }
    if(is.null(ylim)) {
        ylim <- range(drift$Latitude)
    }
    if(length(xlim) == 1) {
        xlim <- range(drift$Longitude) + c(-1, 1) * xlim
    }
    if(length(ylim) == 1) {
        ylim <- range(drift$Latitude) + c(-1, 1) * ylim
    }
    if(xlim[2] < rangeDf$Longitude[1] ||
       xlim[1] > rangeDf$Longitude[2] ||
       ylim[2] < rangeDf$Latitude[1] ||
       ylim[1] > rangeDf$Latitude[2]) {
        cat('\nPlot is entirely out of range: ',
            paste0(xlim, collapse=' -> '), ', ',
            paste0(ylim, collapse=' -> '), ' vs. (-135 -> -108, 20 -> 55)',
            sep='')
        return(NULL)
    }
    xlim[xlim < rangeDf$Longitude[1]] <- rangeDf$Longitude[1]
    xlim[xlim > rangeDf$Longitude[2]] <- rangeDf$Longitude[2]
    ylim[ylim < rangeDf$Latitude[1]] <- rangeDf$Latitude[1]
    ylim[ylim > rangeDf$Latitude[2]] <- rangeDf$Latitude[2]
    
    bathyData <- as.bathy(raster::raster(etopo))
    
    bathyData <- try(subsetBathy(bathyData, x=xlim, y=ylim, locator = FALSE), silent=TRUE)
    if(inherits(bathyData, 'try-error')) {
        cat('\nBathymetry subset failed for coordinates:',
            paste0(xlim, collapse=' -> '), ', ',
            paste0(ylim, collapse=' -> ')
        )
        return(NULL)
    }
    wid <- nrow(bathyData)
    xmar <- 1.24
    ht <- ncol(bathyData)
    ymar <- 1.84
    if(wid <= ht) {
        width <- size
        height <- size * ht / wid
    } else {
        height <- size
        width <- size * wid / ht
    }
    if(!is.null(filename)) {
        tryOpen <- suppressWarnings(try(
            png(filename, height = height + ymar, width = width + xmar, units='in',res=300)
        ))
        if(inherits(tryOpen, 'try-error')) {
            warning('Unable to create image "', filename, '", file appears to be open already')
            return(NULL)
        }
        on.exit(dev.off())
    }
    # Setting up bathy data and legend
    if(bathy) {
        depthPal <- list(c(0, max(bathyData), grey(.7), grey(.9), grey(.95)),
                         c(min(bathyData), 0, "darkblue", "lightblue"))
        bVals <- round(seq(from=0, to=abs(min(bathyData)), length.out=7), 0)
        bVals[c(2,4,6)] <- NA
        bCols <- colorRampPalette(c('lightblue', 'darkblue'))(7)
    } else {
        depthPal <- list(c(0, max(bathyData), grey(.7), grey(.9), grey(.95)),
                         c(min(bathyData), -200, "skyblue1", 'skyblue1'),
                         c(-200, 0,"lightblue", "lightblue"))
        bVals <- round(c(0, NA, 200, NA, abs(min(bathyData))), 0)
        bCols <- c(rep('lightblue', 3), rep('skyblue1', 2))
    }
    
    plot(bathyData, image = TRUE, land = TRUE, axes = T, lwd=0.3,
         bpal = depthPal, lty=1,
         shallowest.isobath=-100, deepest.isobath=-200, step=100, drawlabels=T)
    title(main=title)
    # SHipping lanes
    if(sl) {
        shipLanes <- readRDS(file.path(dataPath, 'ShippingLaneCA.RData'))
        # shipLanes <- readRDS('../Data/SPOTXML/ShippingLaneCA.RData')
        plot(shipLanes$geometry, add=TRUE, border='orange', lwd=2)
    }
    # Wind Call Areas
    if(wca) {
        windCall <- readRDS(file.path(dataPath, 'WindCallBoundary.RData'))
        # windCall <- readRDS('../Data/SPOTXML/WindCallBoundary.RData')
        plot(st_union(windCall), add=TRUE, border='purple', lwd=2)
    }
    # national marine sanctuaries
    if(nms) {
        nmsFiles <- list.files(dataPath, pattern='NMS', full.names=TRUE)
        nmsData <- lapply(nmsFiles, readRDS)
        nmsData <- do.call('c', lapply(nmsData, function(n) n$geometry))
        plot(nmsData, add=TRUE, border='blue', lwd=2)
    }
    # this looks weird but converting numeric to logical with !!
    if(sum(!!(c(current, wind, swell, wave))) > 1) {
        warning('Recommended to only plot one kind of arrow data at a time')
    }
    # Current data
    if(current) {
        switch(current,
               '1' = {
                   currentNc <- file.path(dataPath, 'RTOFSForecurrent.nc')
                   edi <- 'ncepRtofsG3DForeDaily_LonPM180'
                   curVar <- c(F, F, T, T)
                   radial <- FALSE
                   xyVars <- c('u', 'v')
               },
               '2' = {
                   currentNc <- file.path(dataPath, 'RTOFSNowcurrent.nc')
                   edi <- 'ncepRtofsG3DNowDaily_LonPM180'
                   curVar <- c(F, F, T, T)
                   xyVars <- c('u', 'v')
                   radial <- FALSE
               },
               '3' = {
                   currentNc <- file.path(dataPath, 'HFRADARcurrent.nc')
                   edi <- 'ucsdHfrW6'
                   curVar <- c(T, T, rep(F, 5))
                   xyVars <- c('water_u', 'water_v')
                   radial=FALSE
               },
               '4' = {
                   currentNc <- file.path(dataPath, 'HYCOMGLBycurrent.nc')
                   edi <- PAMmisc::getEdinfo()[['HYCOM']]
                   curVar <- c(F, F, F, T, T)
                   xyVars <- c('water_u', 'water_v')
                   radial <- FALSE
               },
               '5' = {
                   currentNc <- file.path(dataPath, 'RTOFSNowcurrent6hour.nc')
                   curVar <- c(F, F, T, T)
                   xyVars <- c('u', 'v')
                   radial <- FALSE
                   edi <- 'ncepRtofsG3DNow6hrlyR2'
               },
               '6' = {
                   currentNc <- file.path(dataPath, 'WCOFS_ROMS.rds')
                   curVar <- T
                   xyVars <- c('u_eastward', 'v_northward')
                   radial <- FALSE
               }
        )
        
        updateNc(currentNc, edi, vars=curVar)
        curLeg <- plotArrowGrid(xLim=xlim, yLim=ylim, diff=NULL, nc=currentNc,
                                xyVars = xyVars, depth=depth, time=time,
                                width=width, radial=radial)
        
        curLeg$vals <- round(curLeg$vals * 1.94384, 2)
        title(sub = paste0('Current data as of ', 
                           format(curLeg$time, '%Y-%m-%d %H:%M:%S'), ' Pacific'))
        # ucsdHfrW1
        # water_u/v
        
    }
    # Wind data
    if(wind) {
        windNc <- file.path(dataPath, 'Metopwind.nc')
        updateNc(windNc, 'erdQMwind1day_LonPM180', vars=c(T, T))
        wndLeg <- plotArrowGrid(xLim = xlim, yLim=ylim, diff=NULL, nc=windNc,
                                xyVars = c('x_wind', 'y_wind'), width=width, radial=FALSE)
    }
    # Swell data
    if(swell) {
        swellNc <- file.path(dataPath, 'WW3wave.nc')
        updateNc(swellNc, 'ww3_global', vars=c(T, F, T, T, F, T, F, F, F))
        swLeg <- plotArrowGrid(xLim = xlim, yLim=ylim, diff=NULL, nc=swellNc,
                               xyVars = c('shgt', 'sdir'), width=width,
                               radial=TRUE, cart=FALSE, dirFrom=TRUE)
    }
    # Wave heigh data
    if(wave) {
        waveNc <- file.path(dataPath, 'WW3wave.nc')
        updateNc(waveNc, 'ww3_global', vars=c(T, F, T, T, F, T, F, F, F))
        wvLeg <- plotArrowGrid(xLim = xlim, yLim=ylim, diff=NULL, nc=waveNc,
                               xyVars = c('Thgt', 'Tdir'), width=width,
                               radial=TRUE, cart=FALSE, dirFrom=TRUE)
    }
    myScaleBathy(bathyData, deg=diff(xlim) * .2, x= 'bottomleft', inset=5, col='white')
    
    
    # Plotting drift and start/end points
    symbs <- c(48:57, 35:38, 61:64, 131, 132, 163, 165, 167, 169, 171, 174, 97:122)
    mapLabs <- symbs[seq_along(unique(drift[[labelBy]]))]
    for(d in seq_along(unique(drift[[labelBy]]))) {
        thisDrift <- drift[drift[[labelBy]] == unique(drift[[labelBy]])[d], ]
        if(nrow(thisDrift) == 1) next
        start <- which.min(thisDrift$UTC)
        thisDrift <- arrange(thisDrift, UTC)
        lines(x=thisDrift$Longitude, y=thisDrift$Latitude, col='black', lwd=2)
        lines(x=thisDrift$Longitude, y=thisDrift$Latitude, col='grey', lwd=1, lty=5)
        if(isFALSE(simple)) {
            # start/end markers
            points(x=thisDrift$Longitude[start], y=thisDrift$Latitude[start], pch=15, cex=1.2)
            end <- which.max(thisDrift$UTC)
            points(x=thisDrift$Longitude[end], thisDrift$Latitude[end], pch=17, col='grey', cex=1.2)
            # labels
            points(x=thisDrift$Longitude[start], y=thisDrift$Latitude[start], pch=mapLabs[d], col='white', cex=.6)
            points(x=thisDrift$Longitude[end], y=thisDrift$Latitude[end], pch=mapLabs[d], col='black', cex=.6)
        }
    }
    if(isTRUE(simple)) {
        if(is.null(filename)) {
            return(invisible(drift))
        }
        return(invisible(filename))
    }
    
    # Plot port cities and other POI
    text(x=poiDf$Longitude, y=poiDf$Latitude, labels=poiDf$Name, cex=.6, srt=30, adj=c(-.1, .5))
    points(x=poiDf$Longitude, y=poiDf$Latitude, cex=.5, pch=16)
    
    # Legendary
    lastLegend <- legend(x='topright', legend=c('Start', 'End', unique(drift[[labelBy]])),
                         col=c('black', 'grey', rep('black', length(mapLabs))), pch=c(15, 17, mapLabs), merge=FALSE,
                         seg.len = 1, cex=1, plot=FALSE)
    useCex <- min(.2 * diff(xlim) / lastLegend$rect$w, 1)
    lastLegend <- legend(x='topright', legend=c('Start', 'End', unique(drift[[labelBy]])),
                         col=c('black', 'grey', rep('black', length(mapLabs))), pch=c(15, 17, mapLabs), merge=FALSE,
                         seg.len = 1, cex=useCex)
    if(current) {
        lastLegend  <- myGradLegend(lastLeg=lastLegend, vals=curLeg$vals, cols=curLeg$colors,
                                    title='Current (Knots)', cex=useCex, tCex=.67)
    }
    if(wind) {
        lastLegend  <- myGradLegend(lastLeg=lastLegend, vals=wndLeg$vals, cols=wndLeg$colors,
                                    title='Wind Speed (m/s)', cex=useCex, tCex=.67)
    }
    if(swell) {
        lastLegend  <- myGradLegend(lastLeg=lastLegend, vals=swLeg$vals, cols=swLeg$colors,
                                    title='Swell Size (m)', cex=useCex, tCex=.67)
    }
    if(wave) {
        lastLegend  <- myGradLegend(lastLeg=lastLegend, vals=wvLeg$vals, cols=wvLeg$colors,
                                    title='Wave Height (m)', cex=useCex, tCex=.67)
    }
    
    if(bathy) {
        lastLegend <- myGradLegend(lastLeg=lastLegend, vals=bVals, cols=bCols,
                                   title='      Depth (m)', cex=useCex, tCex=.67)
    }
    
    # myScaleBathy(bathyData, deg=diff(xlim) * .2, x= 'bottomleft', inset=5, col='white')
    if(is.null(filename)) {
        return(invisible(drift))
    }
    invisible(filename)
}

myGradLegend <- function(lastLeg=NULL, vals, cols, title, cex, tCex=.67) {
    if(is.null(lastLeg)) {
        legX <- 'topright'
        legY <- NULL
    } else {
        legX <- lastLeg$rect$left
        legY <- lastLeg$rect$top - lastLeg$rect$h
    }
    thisLeg <- legend(x=legX, y=legY,
                      legend=c(NA, vals), fill=c(NA, cols), cex=cex,
                      y.intersp=.5, adj=c(.2, .5), plot=FALSE)
    if(!is.null(lastLeg)) {
        legX <- legX + lastLeg$rect$w - thisLeg$rect$w
    }
    thisLeg <- legend(x=legX, y=legY,
                      legend=c(NA, vals), fill=c(NA, cols), border=c(NA, rep(1, length(cols))),
                      y.intersp=.5, adj=c(.2, .5), cex=cex)
    legend(x=legX, y=legY, title=title, cex=cex * tCex, bty='n', legend=NA)
    thisLeg
}
pgDateToPosix <- function(x) {
    # as.POSIXct(as.character(x), format='%Y-%m-%d %H:%M:%OS', tz='UTC')
    parse_date_time(stringr::str_trim(x), 
                    orders=c('%Y-%m-%d %H:%M:%OS', '%Y-%m-%d', '%Y-%m-%d  %H:%M:%OS'), tz='UTC', exact=TRUE)
}

# cart - cartesian or cardinal dir, dirFrom - angle pointing toward directin
# from or direction of travel
plotArrowGrid <- function(xLim, yLim, diff=NULL, nc, xyVars, depth=0, time=nowUTC(), width, scale=2,
                          radial=FALSE, cart=TRUE, dirFrom=FALSE) {
    if(is.null(diff)) {
        diff <- 30
    }
    if(length(diff) == 1) {
        if(diff > 1) {
            diff <- min(diff(xLim), diff(yLim)) / diff
        }
        diff <- rep(diff, 2)
    }
    # janky fix for janky hfradar. Often most recent hour is NA, so set up
    # to do average of last 2 hours
    buffer <- c(0,0,0)
    if(grepl('HFRADAR', nc)) {
        tmp <- nc_open(nc)
        tDim <- tmp$dim$time$vals
        nc_close(tmp)
        tDim <- as.POSIXct(tDim, origin='1970-01-01 00:00:00', tz='UTC')
        time <- max(tDim)
        buffer <- c(0,0, 3600)
    }
    llg <- makeLatLongGrid(xLim, yLim, diff[1], diff[2], time=time)
    diff <- diff * scale
    if(grepl('WCOFS_ROMS', nc)) {
        # llg <- matchWcofsData(llg, wcofs=nc, depth=depth)
        llg <- matchWcofsData(llg, wcofs=nc, depth=30:100)
        varNames <- xyVars
    } else {
        llg <- ncToData(llg, nc, FUN=mean,verbose = FALSE, progress=FALSE, buffer=buffer, depth=depth)
        varNames <- paste0(xyVars, '_mean')
    }
    llg <- PAMmisc:::to180(llg)
    # If given in R, theta
    if(radial) {
        angle <- llg[[varNames[2]]]
        if(!cart) {
            angle <- 90 - angle
        }
        if(dirFrom) {
            angle <- 180 + angle
        }
        angle <- angle * pi / 180
        llg$xComp <- llg[[varNames[1]]] * cos(angle)
        llg$yComp <- llg[[varNames[1]]] * sin(angle)
        varNames <- c('xComp', 'yComp')
    }
    aCols <- viridis::viridis(32, option='viridis', begin=.6)
    if(all(is.na(llg[[varNames[1]]])) ||
       all(is.na(llg[[varNames[2]]]))) {
        return(list(colors=aCols[1], vals=0, time=llg$matchTime_mean[1]))
    }
    llg$x_scale <- llg[[varNames[1]]] / max(abs(llg[[varNames[1]]]), na.rm=TRUE) * diff[1]
    llg$y_scale <- llg[[varNames[2]]] / max(abs(llg[[varNames[2]]]), na.rm=TRUE) * diff[2]
    llg$mag_scale <- sqrt(llg$x_scale^2 + llg$y_scale^2)
    llg$mag_scale <- llg$mag_scale / max(llg$mag_scale, na.rm=TRUE)
    llg$mag <- sqrt(llg[[varNames[1]]]^2 + llg[[varNames[2]]]^2)
    aLen <- width / diff(xLim) * diff[1] * .3
    # aCols <- colorRampPalette(c('green', 'yellow'))(32)
    llg$magColor <- llg$mag / max(llg$mag, na.rm=TRUE) * length(aCols)
    for(i in 1:nrow(llg)) {
        if(is.na(llg$mag_scale[i])) next
        arrows(x0=llg$Longitude[i], x1=llg$Longitude[i] + llg$x_scale[i],
               y0=llg$Latitude[i], y1=llg$Latitude[i] + llg$y_scale[i],
               length = aLen * llg$mag_scale[i], col=aCols[llg$magColor[i]], angle=20)
    }
    # break early if no data for arrows
    legIx <- seq(from=1, to=length(aCols), length.out=7)
    legCols <- aCols[legIx]
    legVals <- round(seq(from=0, to=max(llg$mag, na.rm=TRUE), length.out=7), 2)
    legVals[c(2,4,6)] <- NA
    list(colors=legCols, vals=legVals, time=with_tz(llg$matchTime_mean[1], 'America/Los_Angeles'))
}

makeLatLongGrid <- function(longRange, latRange, longDiff, latDiff, depth=0, time=nowUTC()) {
    lats <- seq(from=latRange[1], to=latRange[2], by=latDiff)
    lons <- seq(from=longRange[1], to=longRange[2], by=longDiff)
    data.frame(Latitude = rep(lats, length(lons)),
               Longitude = rep(lons, each=length(lats)),
               UTC = time,
               Depth = depth)
}

updateNc <- function(file='RTOFScurrent.nc', id, vars, rerun=TRUE) {
    if(grepl('WCOFS_ROMS.rds', file)) {
        if(file.exists(file)) {
            fInfo <- file.info(file)
            hourDiff <- as.numeric(difftime(Sys.time(), fInfo$mtime, units='hours'))
            if(hourDiff < 3) {
                return(TRUE)
            }
        }
        tryDl <- wcofsToList(file=file, depth=0:200)
        if(is.null(tryDl)) {
            cat('WCOFS download failed.')
            return(FALSE)
        }
        return(TRUE)
    }
    rangeDf <- makeRangeDf()
    if(file.exists(file)) {
        fInfo <- file.info(file)
        ncTry <- nc_open(file, return_on_error=TRUE)
        if(isFALSE(ncTry$error)) {
            nc_close(ncTry)
        }
        if(isFALSE(ncTry$error)) {
            dayDiff <- as.numeric(difftime(Sys.time(), fInfo$mtime, units='days'))
            if(grepl('HFRADAR', file)) {
                if(dayDiff < .125) {
                    return(TRUE)
                }
            } else if(dayDiff < .5) {
                return(TRUE)
            }
        }
        if(isTRUE(ncTry$error)) {
            msg <- suppressWarnings(try(read.table(file, sep='\n')))
            if(inherits(msg, 'try-error')) {
                msg <- 'No message content'
            } else {
                msg <- msg[1,1]
            }
            cat('Download failed with message "', msg, '"\n')
            if(grepl('does not intersect actual time', msg)) {
                splitMsg <- strsplit(msg, ' ')[[1]]
                endTime <- splitMsg[length(splitMsg)]
                endTime <- as.POSIXct(endTime, format='%Y-%m-%dT%H:%M:%SZ', tz='UTC')
                if(as.numeric(difftime(rangeDf$UTC[2], endTime, units='days')) < 1) {
                    cat('Changing end time to', as.character(endTime), '\n')
                    rangeDf$UTC <- endTime
                }
            } else {
                cat('Unexpected error message, copying file to ERROR.nc\n')
                file.copy(from=file, to='ERROR.nc', overwrite = TRUE)
            }
        }
    }
    if(inherits(id, 'edinfo')) {
        edi <- id
    } else {
        edi <- erddapToEdinfo(id, chooseVars = FALSE)
    }
    if(inherits(edi, 'hycomList')) {
        whichHy <- PAMmisc:::whichHycom(rangeDf$UTC[2], edi)
        edi <- edi$list[[whichHy]]
        timeLim <- hycomToEdinfo(edi$dataset, chooseVars = FALSE)
        rangeDf$UTC <- timeLim$limits$UTC[2]
    }
    edi <- varSelect(edi, vars)
    tryCatch({
        cat('Downloading file', file, '...\n')
        downloadEnv(rangeDf, edi, fileName=file, buffer=c(.1, .1, 0), progress = FALSE, timeout=240)
        TRUE
    },
    error = function(e) {
        if(isTRUE(rerun)) {
            cat('First download of', file, 'failed, trying again...\n')
            return(updateNc(file, id, vars, rerun=FALSE))
        }
        cat('Download of file', file, 'failed twice with error message', e$message, '\n')
        FALSE
    },
    TRUE
    )
}


checkWcUrl <- function(date=nowUTC()) {
    ymdh <- getWcofsTime(date)
    url <- buildWcUrl(ymdh[1], ymdh[2], ymdh[3], ymdh[4])
    nc <- try(open.nc(url))
    if(inherits(nc, 'try-error')) {
        ymdh <- getWcofsTime(date - 24 * 3600)
        url <- buildWcUrl(ymdh[1], ymdh[2], ymdh[3], ymdh[4]+24)
        nc <- try(open.nc(url))
        if(inherits(nc, 'try-error')) {
            warning(url, ' is not a valid dataset')
            return(NULL)
        }
    } 
    close.nc(nc)
    url
}

matchWcofsData <- function(data, wcofs, depth=0) {
    if(is.character(wcofs)) {
        wcofs <- readRDS(wcofs)
    }
    data$u_eastward <- NA
    data$v_northward <- NA
    xIx <- sapply(data$Longitude, function(x) {
        which.min(abs(wcofs$Longitude - x))[1]
    })
    
    yIx <- sapply(data$Latitude, function(x) {
        which.min(abs(wcofs$Latitude - x))[1]
    })
    zMin <- which.min(abs(wcofs$Depth - min(depth)))
    zMax <- which.min(abs(wcofs$Depth - max(depth)))
    zIx <- zMin:zMax
    for(i in 1:nrow(data)) {
        data$u_eastward[i] <- mean(wcofs$u_eastward[xIx[i], yIx[i], zIx, 1], na.rm=TRUE)
        data$v_northward[i] <- mean(wcofs$v_northward[xIx[i], yIx[i], zIx, 1], na.rm=TRUE)
    }
    data$matchTime_mean <- wcofs$UTC
    data
}

wcofsToList <- function(time=nowUTC(), depth=0, file='WCOFS_ROMS.rds') {
    url <- checkWcUrl(time)
    if(is.null(url)) {
        warning('Could not download WCOFS')
        return(NULL)
    }
    nc <- open.nc(url)
    on.exit(close.nc(nc))
    lat <- var.get.nc(nc, 'Latitude', start=c(1,1), count=c(1,NA))
    lon <- var.get.nc(nc, 'Longitude', start=c(1,1), count=c(NA, 1))
    d <- var.get.nc(nc, 'Depth', start=1, count=NA)
    data <- makeRangeDf()
    allXix <- sapply(data$Longitude, function(x) {
        which.min(abs(lon - x))[1]
    })
    startX <- min(allXix)
    countX <- diff(range(allXix)) + 1
    allYix <- sapply(data$Latitude, function(x) {
        which.min(abs(lat - x))[1]
    })
    startY <- min(allYix)
    countY <- diff(range(allYix)) + 1
    startZ <- which.min(abs(d - min(depth)))
    endZ <- which.min(abs(d - max(depth)))
    countZ <- length(startZ:endZ)
    allX <- var.get.nc(nc, 'u_eastward', start=c(startX, startY, startZ, 1),
                       count = c(countX, countY, countZ, 1),
                       collapse=FALSE)
    allY <- var.get.nc(nc, 'v_northward', start=c(startX, startY, startZ, 1),
                       count = c(countX, countY, countZ, 1),
                       collapse=FALSE)
    # this used to be "time" but now is "ocean_time" allow checking btwn two
    tryTime <- try(dim.inq.nc(nc, 'time'), silent=TRUE)
    if(inherits(tryTime, 'try-error')) {
        timeVar <- 'ocean_time'
    } else {
        timeVar <- 'time'
    }
    ncTime <- var.get.nc(nc, timeVar, start=1, count=1)
    ncTime <- as.POSIXct(ncTime, origin='2016-01-01 00:00:00', tz='UTC')
    result <- list(Latitude=lat[startY:(startY+countY-1)], 
                   Longitude=lon[startX:(startX+countX-1)], 
                   Depth = d[startZ:endZ],
                   u_eastward = allX,
                   v_northward = allY,
                   UTC = ncTime)
    saveRDS(result, file=file)
    result
}

getWcofsTime <- function(time=nowUTC()) {
    now <- round_date(time, unit='3hour')
    year <- year(now)
    mon <- month(now)
    day <- day(now)
    hour <- hour(now)
    webDay <- day(now())
    # problems if UTC day is greater than local day bc timezone
    hourDiff <- as.numeric(abs(difftime(time, now(), units='hours')))
    if(hourDiff < 12 &&
       day > webDay) {
        hour <- hour + 24 * (day - webDay)
        day <- webDay
    }
    # c(year, mon, day, hour)
    c(year, mon, day, hour -3)
}

buildWcUrl <- function(y, m, d, h) {
    base <- 'https://opendap.co-ops.nos.noaa.gov/thredds/dodsC/NOAA/WCOFS/MODELS/'
    m <- sprintf('%02d', m)
    d <- sprintf('%02d', d)
    h <- sprintf('%03d', h)
    ymd <- paste0(y, '/', m, '/', d, '/')
    dset <- 'nos.wcofs.regulargrid.f'
    dsetTime <- paste0(h, '.', y, m, d, '.t03z.nc')
    paste0(base, ymd, dset, dsetTime)
}

makeRangeDf <- function() {
    data.frame(Longitude=c(-135, -108), Latitude=c(20, 55),
               UTC=c(nowUTC(), nowUTC()), Depth=c(0, 200))
}

myScaleBathy <- function (mat, deg = 1, x = "bottomleft", y = NULL, inset = 10,
                          angle = 90, ...) {
    usr = par("usr")
    if (is.numeric(x) == TRUE & is.null(y) == FALSE) {
        X <- x
        Y <- y
        lat <- abs(Y)
    }
    if (is.numeric(x) == FALSE & is.null(y) == TRUE) {
        insetx = abs((usr[2] - usr[1]) * inset/100)
        insety = abs((usr[4] - usr[3]) * inset/100)
        X <- switch(x, bottomright = (usr[2] - insetx - deg),
                    topright = (usr[2] - insetx - deg), bottomleft = (usr[1] +
                                                                          insetx), topleft = (usr[1] + insetx))
        Y <- switch(x, bottomright = (usr[3] + insety), topright = (usr[4] -
                                                                        insety), bottomleft = (usr[3] + insety), topleft = (usr[4] -
                                                                                                                                insety))
        lat <- switch(x, bottomright = abs(min(as.numeric(colnames(mat)))),
                      topright = abs(max(as.numeric(colnames(mat)))),
                      bottomleft = abs(min(as.numeric(colnames(mat)))),
                      topleft = abs(max(as.numeric(colnames(mat)))))
    }
    cos.lat <- cos((2 * pi * lat)/360)
    perdeg <- (2 * pi * (6372.798 + 21.38 * cos.lat) * cos.lat)/360
    arrows(X, Y, X + (deg), Y, code = 3, length = 0.05, angle = angle, lwd=2, ...)
    text((X + X + (deg))/2, Y, adj = c(0.5, -0.5), labels = paste(round(perdeg *
                                                                            deg, 0), "km"), ...)
}

poiDf <- tibble::tribble(
    ~Name, ~Latitude, ~Longitude,
    "San Diego", 32.71068967391705, -117.17147162885448,
    "Santa Barbara",34.407205041229595, -119.69269808900013,
    'Ventura',34.250263795418434, -119.26720606042934,
    'Morro Bay',35.36841835524968, -120.86325292077103,
    'Monterey Bay',36.604218252060306, -121.89240128825472,
    'Santa Cruz',36.96225980624226, -122.00212520928149,
    'Half Moon Bay',37.50293801397416, -122.48765584637566,
    'San Francisco',37.813095434735914, -122.50037485469521,
    'Bodega Bay',38.30982199529412, -123.05611099859385,
    'Fort Bragg',39.42849849826603, -123.81309923266699,
    'Shelter Cove',40.02403071506129, -124.06607534915634,
    'Eureka',40.806299478998056, -124.1807826182425,
    'Crescent City',41.74692081633374, -124.19223894744171,
    'Point Conception',34.4483491615287, -120.47193766943991,
    'Point Arena',38.91093890217707, -123.71170879559632,
    'Cape Mendocino', 40.438268949326925, -124.40971460611878
)

recentBuoySummary <- function(db, skip=NULL, file=NULL, window=6, dataPath='.', etopo='etopo180.nc',shoreNc='Dist2Coast.nc') {
    con <- dbConnect(db, drv=SQLite())
    on.exit(dbDisconnect(con))
    deps <- dbReadTable(con, 'deploymentData')
    inProgress <- deps$DriftName[is.na(deps$End)]
    if(!is.null(skip)) {
        inProgress <- inProgress[!(inProgress %in% skip)]
    }
    outs <- bind_rows(lapply(inProgress, function(x) {
        # last 6 hours to do speed
        gps <- getDbDeployment(db, drift=x, days = window/24)
        if(nrow(gps) < 2) {
            gps <- getDbDeployment(db, drift=x)
            gps <- arrange(gps, UTC)
            gps <- tail(gps, 2)
        }
        gps <- arrange(gps, UTC)
        if(nrow(gps) > 1) {
            gps <- gps[c(1, nrow(gps)), ]
            timeDiff <- as.numeric(difftime(gps$UTC[2], gps$UTC[1], units='secs'))
            distance <- geosphere::distGeo(c(gps$Longitude[1], gps$Latitude[1]),
                                           c(gps$Longitude[2], gps$Latitude[2]))
            bearing <- geosphere::bearing(c(gps$Longitude[1], gps$Latitude[1]),
                                          c(gps$Longitude[2], gps$Latitude[2]))
            res <- tail(gps, 1)
            res$avSpeed <- distance / timeDiff * 1.94384
            res$bearing <- bearing %% 360
            res$hours <- round(timeDiff / 3600, 2)
        } else if(nrow(gps) == 1 ) {
            res <- gps
            res$avSpeed <- NA
            res$bearing <- NA
            res$hours <- NA
        } else {
            return(NULL)
        }
        res$depTime <- deps$Start[deps$DriftName == x]
        res
    }))
    outs <- ncToData(outs, nc=file.path(dataPath, shoreNc),  FUN=mean, verbose=FALSE, progress=FALSE)
    outs <- ncToData(outs, nc=etopo, FUN=mean, verbose=FALSE, progress=FALSE)
    outs$altitude_mean <- abs(outs$altitude_mean)
    outs$DaysSinceDeployment <- round(as.numeric(difftime(outs$UTC, outs$depTime, units='days')), 1)
    outs <- dplyr::select(outs, c('Latitude', 'Longitude', 'UTC', 'DriftName', 'avSpeed', 'bearing', 'hours','dist_mean', 'altitude_mean', 'DaysSinceDeployment'))
    outs <- rename(outs, AvgSpeedKnots=avSpeed, AvgBearing=bearing, AvgLastXHours=hours, Dist2ShoreKm=dist_mean, Depth_m=altitude_mean)
    if(!is.null(file)) {
        write.csv(outs, file=file, row.names = FALSE)
    }
    outs
}

plotTestDeployments <- function(sheet='1Gwjaoq0l7UavwF_xuajvMMRzn_5YL-l25bpJNcUP4CY',
                                file = 'TestDeployments.csv',
                                dataPath='.',
                                etopo='etopo180.nc',
                                current=4, 
                                driftData=NULL,
                                outDir='.') {
    with_drive_quiet({
        drive_download(as_id(sheet), path=file, overwrite = TRUE)
    })
    deps <- readr::read_csv(file,
                            col_types=list(col_double(), col_double(),
                                           col_character(), col_character(),
                                           col_character(), col_character(), 
                                           col_character(), col_character()))
    colnames(deps)[3] <- 'DriftName'
    naCols <- is.na(deps$Latitude) | is.na(deps$Longitude)
    if(!is.null(driftData)) {
        isDrift <- sapply(strsplit(deps$DriftName, ','), function(x) {
            x <- str_trim(x)
            any(x %in% driftData$DriftName)
        })
        naCols <- naCols & !isDrift
        # naCols <- naCols & !(deps$DriftName %in% driftData$DriftName)
    }
    deps <- deps[!naCols, ]
    deps$UTC <- nowUTC()
    nDeps <- nrow(deps)
    deps <- rbind(deps, deps)
    deps$UTC[1:nDeps] <- deps$UTC[1:nDeps] - 1
    deps$LatRange[is.na(deps$LatRange)] <- '.5'
    deps$LonRange[is.na(deps$LonRange)] <- '1'
    deps$PlotEndDate <- as.POSIXct(deps$PlotEndDate, format='%m/%d/%Y', tz='UTC')
    outFiles <- character(0)
    for(i in unique(deps$DriftName)) {
        if(any(!is.na(deps$PlotEndDate[deps$DriftName == i])) &&
           any(deps$PlotEndDate[deps$DriftName == i] < nowUTC())) {
            next
        }
        if(!is.null(driftData) &&
           # i %in% driftData$DriftName) {
           any(str_trim(strsplit(i, ',')[[1]]) %in% driftData$DriftName)) {
            thisDrift <- driftData[driftData$DriftName %in% 
                                       str_trim(strsplit(i, ',')[[1]]), ]
            thisDrift$LatRange <- deps$LatRange[deps$DriftName == i][1]
            thisDrift$LonRange <- deps$LonRange[deps$DriftName == i][1]
        } else {
            thisDrift <- deps %>% filter(DriftName == i)
            if(nrow(thisDrift) > 2) {
                thisDrift$DriftName <- paste0(thisDrift$DriftName, '_', rep(1:(nrow(thisDrift)/2), 2))
            }
        }
        curName <- switch(as.character(current),
                          '4' = 'HYCOM',
                          '3' = 'HFRADAR',
                          '6' = 'WCOFS'
        )
        if(is.null(thisDrift$DeploymentSite) ||
           is.na(unique(thisDrift$DeploymentSite))) {
            thisFile <- paste0(i, '_', curName, '.png')
        } else {
            thisFile <- paste0(i, '_', curName, '_',
                               unique(thisDrift$DeploymentSite), '.png')
        }
        thisFile <- file.path(outDir, thisFile)
        # depTimeLocal <- min(thisDrift$UTC) - 7 * 3600
        # depTimeLocal <- as.character(format(depTimeLocal, format='%m-%d-%Y'))
        # thisFile <- paste0(depTimeLocal, '_',thisFile)
        
        latr <- thisDrift$LatRange[1]
        if(grepl(',', latr)) {
            latr <- strsplit(latr, ',')[[1]]
        }
        lonr <- thisDrift$LonRange[1]
        if(grepl(',', lonr)) {
            lonr <- strsplit(lonr, ',')[[1]]
        }
        latr <- as.numeric(latr)
        lonr <- as.numeric(lonr)
        
        outFiles <- c(outFiles,
                      plotAPIDrift(thisDrift, etopo=etopo, labelBy = 'DriftName',
                                   filename=thisFile, current=current, dataPath = dataPath,
                                   xlim=lonr, ylim=latr, title=curName)
        )
    }
    outFiles
}

# addToGps <- function(x, maxSpeed = 4 / 3.6) {
#     if(is.null(x) ||
#        nrow(x) == 0) {
#         return(x)
#     }
#     if(nrow(x) == 1) {
#         x$Distance <- NA
#         x$Speed <- NA
#         x$Bearing <- NA
#         return(x)
#     }
#     x <- arrange(x, UTC)
#     timeDiff <- as.numeric(difftime(x$UTC[2:(nrow(x))], x$UTC[1:(nrow(x)-1)], units='secs'))
#     distance <- distGeo(cbind(x$Longitude[1:(nrow(x)-1)], x$Latitude[1:(nrow(x)-1)]),
#                         cbind(x$Longitude[2:(nrow(x))], x$Latitude[2:(nrow(x))]))
#     x$Distance <- c(distance, 0)
#     x$Speed <- c(distance / timeDiff, 0)
#     tooFast <- x$Speed > maxSpeed
#     if(any(tooFast)) {
#         return(addToGps(x[!tooFast,], maxSpeed))
#     }
#     bearing <- bearing(cbind(x$Longitude[1:(nrow(x)-1)], x$Latitude[1:(nrow(x)-1)]),
#                        cbind(x$Longitude[2:(nrow(x))], x$Latitude[2:(nrow(x))]))
#     x$Bearing <- c(bearing %% 360, NA)
#     x
# }

# plotAnneDrift <- function(drift, etopo = 'etopo180.nc', filename=NULL, bathy=TRUE, sl=TRUE, wca=TRUE,
#                           current=FALSE, wind=FALSE, swell=FALSE, wave=FALSE, depth=0, time=nowUTC(),
#                           size = 4, xlim=1, ylim=.5, labelBy='DriftName', title=NULL,
#                           dataPath='../Data/SPOTXML',
#                           pal = 'Set2') {
#     if(is.null(etopo)) {
#         etopo <- tempfile()
#     } else {
#         etopo <- file.path(dataPath, etopo)
#     }
#     if(!file.exists(etopo)) {
#         edi <- erddapToEdinfo('etopo180', chooseVars = FALSE)
#         edi <- varSelect(edi, TRUE)
#         etopo <- downloadEnv(drift, edinfo=edi, fileName = etopo, buffer=c(1, 1, 0))
#     }
#     rangeDf <- makeRangeDf()
#     if(is.null(xlim)) {
#         xlim <- range(drift$Longitude)
#     }
#     if(is.null(ylim)) {
#         ylim <- range(drift$Latitude)
#     }
#     if(length(xlim) == 1) {
#         xlim <- range(drift$Longitude) + c(-1, 1) * xlim
#     }
#     if(length(ylim) == 1) {
#         ylim <- range(drift$Latitude) + c(-1, 1) * ylim
#     }
#     xlim[1] <- max(c(xlim[1], rangeDf$Longitude[1]))
#     xlim[2] <- min(c(xlim[2], rangeDf$Longitude[2]))
#     ylim[1] <- max(c(ylim[1], rangeDf$Latitude[1]))
#     ylim[2] <- min(c(ylim[2], rangeDf$Latitude[2]))
#     
#     bathyData <- as.bathy(raster(etopo))
#     # browser()
#     bathyData <- try(subsetBathy(bathyData, x=xlim, y=ylim, locator = FALSE), silent=TRUE)
#     if(inherits(bathyData, 'try-error')) {
#         return(plotAnneDrift(drift, etopo=NULL, filename, bathy, sl, wca, current,
#                              wind, swell, wave, depth,time, size, xlim, ylim, labelBy, title, dataPath, pal))
#     }
#     wid <- nrow(bathyData)
#     xmar <- 1.24
#     ht <- ncol(bathyData)
#     ymar <- 1.84
#     if(wid <= ht) {
#         width <- size
#         height <- size * ht / wid
#     } else {
#         height <- size
#         width <- size * wid / ht
#     }
#     if(!is.null(filename)) {
#         tryOpen <- suppressWarnings(try(
#             png(filename, height = height + ymar, width = width + xmar, units='in',res=300)
#         ))
#         if(inherits(tryOpen, 'try-error')) {
#             warning('Unable to create image "', filename, '", file appears to be open already')
#             return(NULL)
#         }
#         on.exit(dev.off())
#     }
#     # Setting up bathy data and legend
#     if(bathy) {
#         depthPal <- list(c(0, max(bathyData), grey(.7), grey(.9), grey(.95)),
#                          c(min(bathyData), 0, "darkblue", "lightblue"))
#         bVals <- round(seq(from=0, to=abs(min(bathyData)), length.out=7), 0)
#         bVals[c(2,4,6)] <- NA
#         bCols <- colorRampPalette(c('lightblue', 'darkblue'))(7)
#     } else {
#         depthPal <- list(c(0, max(bathyData), grey(.7), grey(.9), grey(.95)),
#                          c(min(bathyData), -200, "skyblue1", 'skyblue1'),
#                          c(-200, 0,"lightblue", "lightblue"))
#         bVals <- round(c(0, NA, 200, NA, abs(min(bathyData))), 0)
#         bCols <- c(rep('lightblue', 3), rep('skyblue1', 2))
#     }
#     
#     plot(bathyData, image = TRUE, land = TRUE, axes = T, lwd=0.3,
#          bpal = depthPal, lty=1,
#          shallowest.isobath=-100, deepest.isobath=-200, step=100, drawlabels=T)
#     title(main=title)
#     # SHipping lanes
#     if(sl) {
#         shipLanes <- readRDS(file.path(dataPath, 'ShippingLaneCA.RData'))
#         # shipLanes <- readRDS('../Data/SPOTXML/ShippingLaneCA.RData')
#         plot(shipLanes$geometry, add=TRUE, border='orange', lwd=2)
#     }
#     # Wind Call Areas
#     if(wca) {
#         windCall <- readRDS(file.path(dataPath, 'WindCallBoundary.RData'))
#         # windCall <- readRDS('../Data/SPOTXML/WindCallBoundary.RData')
#         plot(windCall$geometry, add=TRUE, border='purple', lwd=2)
#     }
#     mbnms <- readRDS(file.path(dataPath, 'MBNMS_Bounds.rda'))
#     plot(mbnms$geometry, add=TRUE, border='blue', lwd=1)
#     gfnms <- readRDS(file.path(dataPath, 'GFNMS_Bounds.rda'))
#     plot(gfnms$geometry, add=TRUE, border='blue', lwd=1)
#     # cinms <- readRDS(file.path(dataPath, 'CINMS_Bounds.rda'))
#     # plot(cinms$geometry, add=TRUE, border='blue', lwd=1)
#     cbnms <- readRDS(file.path(dataPath, 'CBNMS_Bounds.rda'))
#     plot(cbnms$geometry, add=TRUE, border='blue', lwd=1)
#     # this looks weird but converting numeric to logical with !!
#     if(sum(!!(c(current, wind, swell, wave))) > 1) {
#         warning('Recommended to only plot one kind of arrow data at a time')
#     }
#     # Current data
#     if(current) {
#         switch(current,
#                '1' = {
#                    currentNc <- file.path(dataPath, 'RTOFSForecurrent.nc')
#                    edi <- 'ncepRtofsG3DForeDaily_LonPM180'
#                    curVar <- c(F, F, T, T)
#                    radial <- FALSE
#                    xyVars <- c('u', 'v')
#                },
#                '2' = {
#                    currentNc <- file.path(dataPath, 'RTOFSNowcurrent.nc')
#                    edi <- 'ncepRtofsG3DNowDaily_LonPM180'
#                    curVar <- c(F, F, T, T)
#                    xyVars <- c('u', 'v')
#                    radial <- FALSE
#                },
#                '3' = {
#                    currentNc <- file.path(dataPath, 'HFRADARcurrent.nc')
#                    edi <- 'ucsdHfrW6'
#                    curVar <- c(T, T, rep(F, 5))
#                    xyVars <- c('water_u', 'water_v')
#                    radial=FALSE
#                },
#                '4' = {
#                    currentNc <- file.path(dataPath, 'HYCOMGLBycurrent.nc')
#                    edi <- PAMmisc::getEdinfo()[[2]]
#                    curVar <- c(F, F, F, T, T)
#                    xyVars <- c('water_u', 'water_v')
#                    radial <- FALSE
#                },
#                '5' = {
#                    currentNc <- file.path(dataPath, 'RTOFSNowcurrent6hour.nc')
#                    curVar <- c(F, F, T, T)
#                    xyVars <- c('u', 'v')
#                    radial <- FALSE
#                    edi <- 'ncepRtofsG3DNow6hrlyR2'
#                }
#         )
#         
#         updateNc(currentNc, edi, vars=curVar)
#         curLeg <- plotArrowGrid(xLim=xlim, yLim=ylim, diff=NULL, nc=currentNc,
#                                 xyVars = xyVars, depth=depth, time=time,
#                                 width=width, radial=radial)
#         
#         curLeg$vals <- round(curLeg$vals * 1.94384, 2)
#         title(sub = paste0('Current data as of ', curLeg$time, ' UTC'))
#         # ucsdHfrW1
#         # water_u/v
#         
#     }
#     # Wind data
#     if(wind) {
#         windNc <- file.path(dataPath, 'Metopwind.nc')
#         updateNc(windNc, 'erdQMwind1day_LonPM180', vars=c(T, T))
#         wndLeg <- plotArrowGrid(xLim = xlim, yLim=ylim, diff=NULL, nc=windNc,
#                                 xyVars = c('x_wind', 'y_wind'), width=width, radial=FALSE)
#     }
#     # Swell data
#     if(swell) {
#         swellNc <- file.path(dataPath, 'WW3wave.nc')
#         updateNc(swellNc, 'ww3_global', vars=c(T, F, T, T, F, T, F, F, F))
#         swLeg <- plotArrowGrid(xLim = xlim, yLim=ylim, diff=NULL, nc=swellNc,
#                                xyVars = c('shgt', 'sdir'), width=width,
#                                radial=TRUE, cart=FALSE, dirFrom=TRUE)
#     }
#     # Wave heigh data
#     if(wave) {
#         waveNc <- file.path(dataPath, 'WW3wave.nc')
#         updateNc(waveNc, 'ww3_global', vars=c(T, F, T, T, F, T, F, F, F))
#         wvLeg <- plotArrowGrid(xLim = xlim, yLim=ylim, diff=NULL, nc=waveNc,
#                                xyVars = c('Thgt', 'Tdir'), width=width,
#                                radial=TRUE, cart=FALSE, dirFrom=TRUE)
#     }
#     
#     # Plotting drift and start/end points
#     symbs <- c(48:57, 35:38, 61:64, 131, 132, 163, 165, 167, 169, 171, 174, 97:122)
#     mapLabs <- (49:57)[as.numeric(gsub('ADRIFT_00', '', unique(drift[[labelBy]])))]
#     nColors <- length(unique(drift[[labelBy]]))
#     colPal <- brewer.pal(nColors, pal)
#     for(d in seq_along(unique(drift[[labelBy]]))) {
#         thisDrift <- drift[drift[[labelBy]] == unique(drift[[labelBy]])[d], ]
#         if(nrow(thisDrift) == 1) next
#         start <- which.min(thisDrift$UTC)
#         thisDrift <- arrange(thisDrift, UTC)
#         lines(x=thisDrift$Longitude, y=thisDrift$Latitude, col='black', lwd=2)
#         lines(x=thisDrift$Longitude, y=thisDrift$Latitude, col=colPal[d], lwd=1, lty=1)
#         # start/end markers
#         points(x=thisDrift$Longitude[start], y=thisDrift$Latitude[start], pch=15, cex=1.3, col='black')
#         points(x=thisDrift$Longitude[start], y=thisDrift$Latitude[start], pch=15, cex=1.2, col=colPal[d])
#         end <- which.max(thisDrift$UTC)
#         points(x=thisDrift$Longitude[end], thisDrift$Latitude[end], pch=17, col='black', cex=1.3)
#         points(x=thisDrift$Longitude[end], thisDrift$Latitude[end], pch=17, col=colPal[d], cex=1.2)
#         # labels
#         points(x=thisDrift$Longitude[start], y=thisDrift$Latitude[start], pch=mapLabs[d], col='black', cex=.6)
#         points(x=thisDrift$Longitude[end], y=thisDrift$Latitude[end], pch=mapLabs[d], col='black', cex=.6)
#     }
#     
#     # Plot port cities and other POI
#     text(x=poiDf$Longitude, y=poiDf$Latitude, labels=poiDf$Name, cex=.6, srt=30, adj=c(-.1, .5))
#     points(x=poiDf$Longitude, y=poiDf$Latitude, cex=.5, pch=16)
#     
#     # Legendary
#     # browser()
#     lastLegend <- legend(x='topright', legend=c('Deployment', 'Recovery'),
#                          col=c('black', 'black'), pch=c(15, 17), merge=FALSE,
#                          seg.len = 1, cex=1, plot=FALSE)
#     useCex <- min(.2 * diff(xlim) / lastLegend$rect$w, 1)
#     lastLegend <- legend(x='topright', legend=c('Deployment', 'Recovery'),
#                          col=c('black', 'black'), pch=c(15, 17), merge=FALSE,
#                          seg.len = 1, cex=useCex)
#     if(current) {
#         lastLegend  <- myGradLegend(lastLeg=lastLegend, vals=curLeg$vals, cols=curLeg$colors,
#                                     title='Current (Knots)', cex=useCex, tCex=.67)
#     }
#     if(wind) {
#         lastLegend  <- myGradLegend(lastLeg=lastLegend, vals=wndLeg$vals, cols=wndLeg$colors,
#                                     title='Wind Speed (m/s)', cex=useCex, tCex=.67)
#     }
#     if(swell) {
#         lastLegend  <- myGradLegend(lastLeg=lastLegend, vals=swLeg$vals, cols=swLeg$colors,
#                                     title='Swell Size (m)', cex=useCex, tCex=.67)
#     }
#     if(wave) {
#         lastLegend  <- myGradLegend(lastLeg=lastLegend, vals=wvLeg$vals, cols=wvLeg$colors,
#                                     title='Wave Height (m)', cex=useCex, tCex=.67)
#     }
#     
#     if(bathy) {
#         # lastLegend <- myGradLegend(lastLeg=lastLegend, vals=bVals, cols=bCols,
#         #                            title='      Depth (m)', cex=useCex, tCex=.67)
#     }
#     
#     myScaleBathy(bathyData, deg=diff(xlim) * .2, x= 'bottomleft', inset=5, col='white')
# }

doDriftPlots <- function(depGps, dataPath='.', current=4, verbose=FALSE, outDir='.') {
    if(is.character(depGps)) {
        depGps <- getDbDeployment(depGps)
    }
    outFiles <- character(0)
    # browser()
    
    for(d in unique(depGps$DriftName)) {
        thisDep <- depGps[depGps$DriftName == d, ]
        if(nrow(thisDep) == 0) {
            next
        }
        fname <- paste0(format(min(thisDep$UTC), format='%m-%d-%Y'))
        if(!is.na(thisDep$DeploymentSite[1])) {
            fname <- paste0(fname, '_', thisDep$DeploymentSite[1])
        }
        curName <- switch(as.character(current),
                          '4' = 'HYCOM',
                          '3' = 'HFRADAR',
                          '6' = 'WCOFS',
                          ''
        )
        fname <- paste0(curName, '_', fname)
        fname <- file.path(outDir, paste0(thisDep$DriftName[1], '_', fname, '.png'))
        if(file.exists(fname)) {
            modtime <- file.info(fname)$mtime
            attr(modtime, 'tzone') <- 'UTC'
            if(modtime > max(thisDep$UTC)) next
        }
        if(verbose) {
            cat('\nPlotting drift', thisDep$DriftName[1])
        }
        distEst <- round(sum(thisDep$distance, na.rm=TRUE) / 1e3, 1)
        title <- paste0(curName, ', Est. dist. ', distEst, 'km') 
        thisPlot <- tryCatch(plotAPIDrift(thisDep, filename=fname, current=current, dataPath=dataPath, 
                                          title=title),
                             error = function(e) {
                                 message(e)
                                 NULL
                             })
        outFiles <- c(outFiles, thisPlot)
    }
    outFiles
}

sheetToDbDepFormat <- function(x, new=FALSE) {
    if(new) {
        selCols <- c('Deployment_Date_UTC',
                     'Data_Start_UTC (defined once data is recovered by scanning LTSA)',
                     'Recovery_Date_UTC',
                     'Data_End_UTC (defined once data is recovered by scanning LTSA)',
                     "GPS ID (if appropriate - top / bottom)",
                     'Project',
                     'DeploymentID',
                     'Site',
                     'Deployment_Latitude',
                     'Deployment_Longitude',
                     'Recovery_Latitude', 
                     'Recovery_Longitude'
        )
    } else {
        # first 8 rows have bad formatting for dates, are MB/POSIT survey
        # x <- x[-(1:8), ]
        selCols <- c('Deployment_Date',#
                     'Data_Start',
                     'Recovery_Date',#
                     'Data_End',
                     'GPS_ID',#
                     'Project',#
                     'DeploymentID',#
                     'Site',#
                     'Deployment_Latitude',#
                     'Deployment_Longitude',#
                     'Recovery_Latitude', #
                     'Recovery_Longitude'#
        )
    }
    dbNames <- c('Start', 'DataStart','End','DataEnd', 'DeviceName', 'DriftName', 'DeploymentID', 'DeploymentSite',
                 'Deployment_Latitude',
                 'Deployment_Longitude',
                 'Recovery_Latitude', 
                 'Recovery_Longitude')
    x <- dplyr::select(x, all_of(selCols))
    colnames(x) <- dbNames
    for(i in c('DeviceName', 'DriftName', 'DeploymentID', 'DeploymentSite')) {
        x[[i]] <- as.character(unlist(x[[i]]))
    }
    for(i in c('Start', 'DataStart', 'End', 'DataEnd')) {
        x[[i]] <- suppressWarnings(as.POSIXct(as.numeric(unlist(x[[i]])), origin='1970-01-01 00:00:00', tz='UTC'))
    }
    if(new) {
        x <- x[-1, ]
    } else {
        x <- x[!is.na(x$DriftName), ]
    }
    x$DeploymentID <- ifelse(is.na(x$DeploymentID), '', x$DeploymentID)
    for(i in 1:nrow(x)) {
        x$DeploymentID[i] <- paste0(paste0(rep(0, 3-nchar(x$DeploymentID[i])), collapse=''), x$DeploymentID[i])
        #### Uncomment these to update to DataStart/End ####
        # if(!is.na(x$DataStart[i])) {
        #     x$Start[i] <- x$DataStart[i]
        # }
        # if(!is.na(x$DataEnd[i])) {
        #     x$End[i] <- x$DataEnd[i]
        # }
    }
    
    x$DriftName <- paste0(x$DriftName, '_', x$DeploymentID)
    x$DeploymentID <- NULL
    # x$DataStart <- NULL
    # x$DataEnd <- NULL
    x$DeviceName <- gsub('\\/', ', ', x$DeviceName)
    isOpp <- grepl('OPPS', x$DriftName)
    for(i in which(isOpp)) {
        oppsName <- paste0(x$DriftName[i], c('_D', '_R'), collapse=', ')
        if(x$DeviceName[i] == 'NA') {
            x$DeviceName[i] <- oppsName
        } else {
            x$DeviceName[i] <- paste0(x$DeviceName[i], ', ', oppsName)
        }
    }
    drops <- is.na(x$Start) | x$DeviceName == 'NA'
    x[!drops, ]
}

doGdriveUpload <- function(files, destination, modHours=1) {
    backoffs <- c(1, 2, 4, 8, 16)
    backIx <- 1
    with_drive_quiet({
        for(i in seq_along(files)) {
            modtime <- file.info(files[i])$mtime
            diff <- as.numeric(difftime(Sys.time(), modtime, units='hours'))
            if(diff > modHours) next
            # Trying exp backoff for 500 errors
            tryUp <- try(drive_upload(files[i], path=destination, overwrite=TRUE))
            if(inherits(tryUp, 'try-error') &&
               grepl('Server error: (500)', tryUp)) {
                while(backIx <= length(backoffs)) {
                    cat('\n500 error - sleeping for', backoffs[backIx])
                    randTime <- runif(1) / 10
                    Sys.sleep(backoffs[backIx] + randTime)
                    tryUp <- try(drive_upload(files[i], path=destination, overwrite=TRUE))
                    # still bad, wait more
                    if(inherits(tryUp, 'try-error')) {
                        backIx <- backIx + 1
                    }
                    # if worked, reset backoff and break
                    backIx <- 1
                    break
                }
            }
        }
    })
}

# Check for new deployments from worksheet
checkDeploymentUpdates <- function(sheetId = '10bxlwfVOe1LFfj69B_YddxcA0V14m7codYwgD2YncFk', 
                                   db='SPOTGPS_Logger.sqlite3') {
    sheetId <- as_id(sheetId)
    with_drive_quiet({
        drive_download(sheetId, path='deployDetails', overwrite = TRUE)
    })
    # cat('\nReading XL sheets')
    sheetNames <- readxl::excel_sheets('deployDetails.xlsx')
    allDep <- vector('list', length=length(sheetNames))
    for(i in seq_along(allDep)) {
        if(sheetNames[i] == 'deployDetails') {
            allDep[[i]] <- read_xlsx('deployDetails.xlsx', sheet='deployDetails', col_types = 'list')
            allDep[[i]] <- sheetToDbDepFormat(allDep[[i]], new=FALSE)
        }
        if(sheetNames[i] == 'NEW DEPLOYMENT TO SAVE') {
            allDep[[i]] <- read_xlsx('deployDetails.xlsx', sheet='NEW DEPLOYMENT TO SAVE', col_types ='list')
            allDep[[i]] <- sheetToDbDepFormat(allDep[[i]], new=TRUE)
        }
    }
    allDep <- bind_rows(allDep)
    # depDet <- read_xlsx('deployDetails.xlsx', sheet='deployDetails', col_types = 'list')
    # newDep <- read_xlsx('deployDetails.xlsx', sheet='NEW DEPLOYMENT TO SAVE', col_types ='list')
    
    # GPS archive if needed
    # newDep <- sheetToDbDepFormat(newDep, new=TRUE)
    # depDet <- sheetToDbDepFormat(depDet, new=FALSE)
    # allDep <- rbind(newDep, depDet)
    oppData <- createOppData(db, allDep)
    # allDep$Start <- as.character(allDep$Start)
    # allDep$End <- as.character(allDep$End)
    allDep$Start <- format(allDep$Start, format='%Y-%m-%d %H:%M:%S')
    allDep$End <- format(allDep$End, format='%Y-%m-%d %H:%M:%S')
    allDep$DataStart <- format(allDep$DataStart, format='%Y-%m-%d %H:%M:%S')
    allDep$DataEnd <- format(allDep$DataEnd, format='%Y-%m-%d %H:%M:%S')
    # janky fix for SO-001 == SO-296 mismatch
    for(i in 1:nrow(allDep)) {
        has001 <- grepl('SO-001', allDep$DeviceName[i])
        has296 <- grepl('SO-296', allDep$DeviceName[i])
        if(has001 & !has296) {
            allDep$DeviceName[i] <- gsub('SO-001', 'SO-001, SO-296', allDep$DeviceName[i])
        } else if(has296 & !has001) {
            allDep$DeviceName[i] <- gsub('SO-296', 'SO-296, SO-001', allDep$DeviceName[i])
        }
    }
    # cat('\nReading DB')
    con <- dbConnect(db, drv=SQLite())
    on.exit(dbDisconnect(con))
    dbDep <- dbReadTable(con, 'deploymentData')
    # dbDep$
    ########
    if(nrow(oppData) > 0) {
        dbLog <- dbReadTable(con, 'gpsLogger')
        gpsData <- dbReadTable(con, 'gpsData')
        oppData$Id <- 1:nrow(oppData) + max(gpsData$Id)
        oppData$UTC <- format(oppData$UTC, format='%Y-%m-%d %H:%M:%S')
        logId <- max(dbLog$Id) + 1
        now <- nowUTC()
        
        logAppend <- data.frame(Id = logId, UTC = format(now, format='%Y-%m-%d %H:%M:%S'),
                                RowsAdded = nrow(oppData),
                                Source = 'Fake Opportunistic')
        dbAppendTable(con, 'gpsLogger', logAppend)
        dbAppendTable(con, 'gpsData', oppData)
    }
    ###########
    # dbDep$Start <- as.POSIXct(dbDep$Start, format='%Y-%m-%d %H:%M:%S', tz='UTC')
    # dbDep$End <- as.POSIXct(dbDep$End, format='%Y-%m-%d %H:%M:%S', tz='UTC')
    # 
    toAdd <- allDep[!allDep$DriftName %in% dbDep$DriftName, ]
    dbNames <- c('Id', 'Start', 'End', 'DeviceName', 'DriftName', 'DeploymentSite', 'DataStart', 'DataEnd')
    if(nrow(toAdd) > 0) {
        toAdd$Id <- max(dbDep$Id) + 1:nrow(toAdd)
        dbAppendTable(con, 'deploymentData', toAdd[dbNames])
        cat('\nAdded', nrow(toAdd), 'rows to database')
    }
    toCheck <- allDep[allDep$DriftName %in% dbDep$DriftName, ]
    updated <- updateNaVals(db, toCheck = toCheck)
    if(length(updated) > 0) {
        cat('\nUpdated', length(updated), 'values in deploymentData')
    }
    # invisible(TRUE)
    updated
}

createOppData <- function(db, depDet) {
    isOpp <- grepl('OPPS', depDet$DriftName)
    depDet <- depDet[isOpp, ]
    oppNames <- unique(depDet$DriftName)
    oppData <- getDbDeployment(db, drift=oppNames)
    for(c in c('Recovery_Longitude', 'Recovery_Latitude',
               'Deployment_Longitude', 'Deployment_Latitude')) {
        if(is.list(depDet[[c]])) {
            depDet[[c]] <- unlist(depDet[[c]])
        }
    }
    # for(i in 1:nrow(depDet)) {
    addOpp <- bind_rows(lapply(split(depDet, depDet$DriftName), function(x) {
        thisOpp <- x$DriftName
        result <- list()
        if(!is.na(x$Start) &&
           !(paste0(thisOpp, '_D') %in% oppData$DeviceName) &&
           !is.na(x$Deployment_Latitude) &&
           !is.na(x$Deployment_Longitude) &&
           x$Deployment_Latitude != 'NA' &&
           x$Deployment_Longitude != 'NA') {
            result$Id <- NA
            result$Latitude <- x$Deployment_Latitude
            result$Longitude <- x$Deployment_Longitude
            result$UTC <- x$Start + 1
            result$DeviceName <- paste0(thisOpp, '_D')
        }
        if(!is.na(x$End) &&
           !(paste0(thisOpp, '_R') %in% oppData$DeviceName) &&
           !is.na(x$Recovery_Latitude) &&
           !is.na(x$Recovery_Longitude) &&
           x$Recovery_Longitude != 'NA' &&
           x$Recovery_Latitude != 'NA') {
            result$Id <- c(result$Id, NA)
            result$Latitude <- c(result$Latitude, x$Recovery_Latitude)
            result$Longitude <- c(result$Longitude, x$Recovery_Longitude)
            result$UTC <- c(result$UTC, x$End - 1)
            result$DeviceName <- c(result$DeviceName, paste0(thisOpp, '_R'))
        }
        if(length(result) == 0) {
            return(NULL)
        }
        result
    }))
    
}

# updating existing data in DB but only if existing is NA
# Does not replace any existing data bc if we manually change
# would keep overriding
# add drift names to this if we want db to differ from deployDetails sheet
# the PASCALs currently here are problems because they have multiple rows per drift,
# so they keep trying to update even though haven't changed
DONTCHANGEDRIFT <- c(paste0('ADRIFT_00', 1:9),
                     paste0('ADRIFT_0', 10:11),
                     paste0('PASCAL_', c('009', '022', '023', '024', '026', '029'))
)

updateNaVals <- function(db, table = 'deploymentData', toCheck) {
    if(nrow(toCheck) == 0) {
        return(0)
    }
    con <- dbConnect(db, drv=SQLite())
    on.exit(dbDisconnect(con))
    
    dbDep <- dbReadTable(con, 'deploymentData')
    dbDep$Start <- pgDateToPosix(dbDep$Start)
    dbDep$End <- pgDateToPosix(dbDep$End)
    toCheck$Start <- pgDateToPosix(toCheck$Start)
    toCheck$End <- pgDateToPosix(toCheck$End)
    # nUpd <- 0
    updated <- character(0)
    # for(j in c('Start', 'End', 'DeviceName', 'DriftName', 'DeploymentSite', 'DataStart', 'DataEnd')) {
    for(j in c('Start', 'End', 'DataStart', 'DataEnd', 'DeploymentSite')) {
        thisCheck <- rep(FALSE, nrow(toCheck))
        for(i in 1:nrow(toCheck)) {
            if(toCheck$DriftName[i] %in% DONTCHANGEDRIFT) {
                next
            }
            # # setting any date only (00:00:00) to NA to force recheck
            whichDrift <- which(dbDep$DriftName == toCheck$DriftName[i])
            # if(j %in% c('Start', 'End') &&
            #    allZero(dbDep[[j]][whichDrift]) &&
            #    !allZero(toCheck[[j]][i])) {
            #     dbDep[[j]][whichDrift] <- NA
            # }
            # thisCheck[i] <- is.na(dbDep[[j]][whichDrift]) & !is.na(toCheck[[j]][i])
            # Changing behavior to always update if diffrent
            # thisCheck[i] <- dbDep[[j]][whichDrift] != toCheck[[j]][i]
            thisCheck[i] <- !identical(dbDep[[j]][whichDrift], toCheck[[j]][i])
        }
        if(!any(thisCheck)) {
            next
        }
        for(i in which(thisCheck)) {
            # updateNa <- dbSendQuery(
            #     con,
            q <-  paste0('UPDATE ', table, ' SET ', j, " = '", toCheck[[j]][i],
                         "' WHERE DriftName = '", toCheck$DriftName[i], "'")
            addVal <- dbSendQuery(con, q)
            dbClearResult(addVal)
            # nUpd <- nUpd + 1
            updated <- c(updated, toCheck$DriftName[i])
        }
    }
    updated
}

doTextUpdates <- function(db) {
    with_drive_quiet({
        # drive_auth(cache='.secrets', email=TRUE)
        gs4_auth(token = drive_token())
        SHEETID <- as_id('1qJQ2f7b8qCo90ewtYlPCmlX2_iiav-qxaLaWErhlmlE')
        sched <- read_sheet(ss=SHEETID, sheet='Schedule')
        tz(sched$Start_Local) <- 'America/Los_Angeles'
        tz(sched$Stop_Local) <- 'America/Los_Angeles'
        contact <- read_sheet(ss=SHEETID, sheet='Contacts')
    })
    dayMax <- 2
    for(i in 1:nrow(sched)) {
        if(with_tz(sched$Start_Local[i], 'UTC') > nowUTC()) next
        if(with_tz(sched$Stop_Local[i], 'UTC') < nowUTC()) next
        gps <- getDbDeployment(db, drift=sched$Deployment_Name[i], days=dayMax)
        if(is.null(gps) || 
           nrow(gps) == 0) {
            message <- paste0('No update in last ', dayMax, ' day(s)')
        } else {
            info <- getLastUpdate(gps, gpsFmt = sched$Coord_Style[i])
            message <- with(info,
                            paste0('Drift ', drift, ' Last Update ',
                                   date, ': ',
                                   position[1], ', ', position[2], ' - ',
                                   'Bearing ', direction, ' Degrees ', speed, ' Knots')
            )
            
        }
        toEmail <- contact$Email[contact$Id == sched$Recipient_Id[i]]
        cat('\nTrying text schedule row', i, 'sent to:', toEmail)
        cat('\nMessage:', message)
        if(toEmail == 'SWFSCinReach') {
            emOut <- sendInreachText(message = message)
            cat('\nResponse:', emOut)
        } else {
            emOut <- sendTurboEmail(to=toEmail, message=message)
            cat('\nResponse:', emOut$message)
        }
    }
}

sendTurboEmail <- function(to, message) {
    authEmail <- secrets$turbo_email
    pw <- secrets$turbo_pw  
    
    url <- 'https://api.turbo-smtp.com/api/v2/mail/send'
    headList <- list(authuser = authEmail,
                     authpass = pw)
    
    bodList <- list(from=authEmail,
                    to = paste0(to, collapse=','),
                    content=message)
    tryEmail <- POST(url=url,
                     body = c(headList, bodList))
    response <- fromJSON(rawToChar(tryEmail$content))
    response
}

sendInreachText <- function(message, imei='301434030691620') {
    user <- secrets$inreach_user
    pw <- secrets$inreach_pw
    url <- 'https://explore.garmin.com/ipcinbound/V1/Messaging.svc/Message'
    newMessage <- inrTemplate
    newMessage$Messages[[1]]$Timestamp <- paste0('/Date(', round(as.numeric(nowUTC()), 0)*1e3, ')/')
    newMessage$Messages[[1]]$Message <- message
    newMessage$Messages[[1]]$Recipients <- list(as.character(imei))
    tryInreach <- POST(url=url,
                       body = toJSON(newMessage),
                       config = authenticate(user, pw),
                       content_type('application/json'))
    response <- rawToChar(tryInreach$content)
    response
    # tryInreach
}

inrTemplate <- 
    list(
        Messages = list(
            list(
                Sender = 'taiki.sakai@noaa.gov',
                Recipients = list('301434030691620'),
                Timestamp = "/Date(1647338836000)/",
                Message = 'sent at 109pm'
                # ReferencePoint = list(
                #     LocationType = 'GPSLocation',
                #     Altitude = 0,
                #     Speed = 0,
                #     Course = 0,
                #     Coordinate = list(
                #         Latitude = 32,
                #         Longitude = -116
                #     ),
                #     Label = 'Test'
                # )
            )
        )
    )

fmtCoord <- function(x, mode=c('decidegree', 'dms', 'deciminute')) {
    switch(match.arg(mode),
           'decidegree' = as.character(round(x, 3)),
           'dms' =  {
               # browser()
               isNeg <- x < 0
               x <- abs(x)
               d <- as.integer(x)
               m <- as.integer((x - d)*60)
               s <- round((x - d - m/60)*3600, 0)
               m <- as.integer(m)
               m <- m + (s %/% 60)
               s <- s %% 60
               d <- d + (m %/% 60)*(ifelse(d < 0, -1, 1))
               m <- m %% 60
               if(m < 10) m <- paste0('0', m)
               if(s < 10) s <- paste0('0', s)
               if(isNeg) d <- d * -1
               paste0(d, '\u00B0', m, "'", s, '"')
           },
           'deciminute' = {
               isNeg <- x < 0
               x <- abs(x)
               d <- as.integer(x)
               m <- (x - d)*60
               m <- round(m, 3)
               d <- d + (m %/% 60)*(ifelse(d < 0, -1, 1))
               m <- m %% 60
               if(m < 10) m <- paste0('0', m)
               if(isNeg) d <- d * -1
               paste0(d, '\u00B0', m, "'")
           }
    )
}

getLastUpdate <- function(x, last=2, gpsFmt='dms', toLocal = TRUE) {
    x <- arrange(x, desc(UTC))
    lastDev <- x$DeviceName[1]
    x <- x[x$DeviceName == lastDev,]
    # make sure points are at least 15 mins apart, otherwise bump up last value
    if(last < nrow(x)) {
        checkTime <- as.numeric(difftime(x$UTC[1], x$UTC[last], units='mins'))
        if(checkTime < 15) {
            return(getLastUpdate(x, last=last+1, toLocal=toLocal))
        }
    }
    last <- min(last, nrow(x))
    dist <- distGeo(c(x$Longitude[last], x$Latitude[last]),
                    c(x$Longitude[1], x$Latitude[1])) / 1e3
    time <- as.numeric(difftime(x$UTC[1], x$UTC[last], units='hours'))
    direction <- bearing(c(x$Longitude[last], x$Latitude[last]),
                         c(x$Longitude[1], x$Latitude[1]))
    position <- c(fmtCoord(x$Latitude[1], gpsFmt), 
                  fmtCoord(x$Longitude[1], gpsFmt))
    date <- x$UTC[1]
    if(toLocal) {
        attr(date, 'tzone') <- 'America/Los_Angeles'
    }
    date <- format(date, usetz=TRUE)
    list(drift=x$DriftName[1], position=position, direction=round(direction %% 360, 0), speed=round(dist / time / 1.852, 2), date=date)
}

updateGpsCsv <- function(db, csvDir='GPS_CSV', id='1xiayEHbx30tFumMagMHJ1uhfmOishMn2', dataPath='.',
                         force=NULL) {
    id <- as_id(id)
    csvs <- drive_ls(id)$name
    # csvs <- list.files(csvDir, pattern='csv$', full.names=TRUE)
    doneDrifts <- gsub('_GPS\\.csv$', '', basename(csvs))
    con <- dbConnect(db, drv=SQLite())
    on.exit(dbDisconnect(con))
    # dep <- dbReadTable(con, 'deploymentData')
    dep <- getDeploymentData(db)
    dep <- dep[!is.na(dep$End) & !is.na(dep$Start), ]
    if(!is.null(force)) {
        toDo <- unique(force)
    } else {
        toDo <- dep$DriftName[!(dep$DriftName %in% doneDrifts)]
    }
    for(d in toDo) {
        thisGps <- getDbDeployment(db, drift=d, verbose=FALSE)
        if(nrow(thisGps) == 0) next
        if(nrow(thisGps) == 1) {
            thisGps <- bind_rows(thisGps, thisGps)
            thisGps$UTC[2] <- thisGps$UTC[2] + 1
        }
        thisFile <- file.path(csvDir, paste0(d, '_GPS'))
        qaqc <- plotGpsQAQC(mixExtra(thisGps), ylim=4, 
                            xlim=c(dep$Start[dep$DriftName == d],
                                   dep$End[dep$DriftName == d]),
                            filename = paste0(thisFile, '_QAQC.png'),
                            title=paste0(d, ' QAQC'))
        thisGps <- ncToData(thisGps, nc=file.path(dataPath, 'etopo180.nc'),
                            keepMatch = FALSE, progress=FALSE)
        thisGps <- rename(thisGps, seadepth = altitude_mean)
        thisGps$seadepth <- thisGps$seadepth * -1
        cat('\nWriting GPS CSV for drift', d, '...')
        plotAPIDrift(thisGps, current=FALSE, wca=FALSE, bathy=TRUE, sl=FALSE, nms=TRUE, xlim=.3, ylim=.3,
                     filename=paste0(thisFile, '.png'), dataPath=dataPath)
        write.csv(thisGps, file = paste0(thisFile, '.csv'), row.names = FALSE)
        drive_upload(paste0(thisFile, '.csv'), path=id, overwrite = TRUE)
        drive_upload(paste0(thisFile, '.png'), path=id, overwrite = TRUE)
        drive_upload(paste0(thisFile,'_QAQC.png'), path=id, overwrite=TRUE)
    }
}

plotGpsQAQC <- function(x, ylim=4, xlim=NULL, title=NULL, filename=NULL) {
    if(is.null(xlim)) {
        xlim <- range(x$UTC)
    }
    tPlot <- x %>% 
        mutate(tLabel = ifelse(tDiff > ylim, round(tDiff,0), NA),
               tDiff = ifelse(tDiff > ylim, ylim, tDiff)) %>% 
        ggplot(aes(x=UTC,y=tDiff, col=Device)) + 
        geom_line(show.legend=FALSE) +
        ylim(0, ylim) + 
        xlim(xlim[1], xlim[2]) +
        geom_vline(xintercept = xlim) +
        geom_text(aes(x=UTC, y=tDiff, label=tLabel), show.legend=FALSE) +
        ylab('Hours Apart') +
        facet_wrap('Device', ncol=1)
    kPlot <- x %>% 
        ggplot(aes(x=UTC,y=knots, col=Device)) + 
        geom_line() +
        ylim(0, 3) +
        xlim(xlim[1], xlim[2]) +
        geom_vline(xintercept = xlim) +
        # geom_text(aes(x=UTC, y=tDiff, label=tLabel), show.legend=FALSE) +
        ylab('Avg Knots') + 
        facet_wrap('Device', ncol=1)
    out <- tPlot + kPlot
    out <- out + 
        plot_annotation(title=title,
                        theme= theme(plot.title = element_text(hjust = 0.45)))
    if(is.null(filename)) {
        return(out)
    }
    ggsave(filename, plot=out, width=10, height=10, units='in', dpi=200)
    filename
}
# making combined extra mesaurements
mixExtra <- function(x) {
    result <- bind_rows(lapply(split(x, x$DeviceName), function(d) {
        tmp <- addGpsExtra(d)
        tmp$Device <- d$DeviceName[1]
        tmp
    }))
    comb <- addGpsExtra(x)
    comb$Device <- 'Combined'
    comb$knots <- calcKnots(comb)
    out <- bind_rows(result, comb)
    out$Device <- factor(out$Device, levels=c(unique(result$Device), 'Combined'))
    out
}
# adding time diff and diff based groups
addGpsExtra <- function(x, hours=3) {
    if(nrow(x) == 1) {
        x$tDiff <- 0
        x$tGroup <- 1
        return(x)
    }
    x$tDiff <- NA
    x$tDiff[2:nrow(x)] <- as.numeric(difftime(x$UTC[2:nrow(x)], x$UTC[1:(nrow(x)-1)], units='hours'))
    x$tDiff[1] <- 0
    x$tGroup <- NA
    gIx <- 1
    for(i in 1:nrow(x)) {
        if(x$tDiff[i] > hours) {
            gIx <- gIx + 1
        }
        x$tGroup[i] <- gIx
    }
    x
}

checkNMS <- function(gps, nmsData) {
    # nmsFiles <- list.files(nmsFolder, pattern='NMS', full.names=TRUE)
    # nmsData <- lapply(nmsFiles, readRDS)
    # names(nmsData) <- gsub('([A-z]NMS).*', '\\1', basename(nmsFiles))
    if(is.null(gps) || nrow(gps) == 0) {
        return('No NMS entered')
    }
    gps <- st_sfc(st_multipoint(matrix(c(gps$Longitude, gps$Latitude), ncol=2)))
    st_crs(gps) <- 4326
    nmsInt <- unlist(lapply(nmsData, function(x) {
        ints <- unlist(st_intersects(x, gps))
        length(unlist(ints)) > 0
    }))
    if(!any(nmsInt)) {
        return('No NMS entered')
    }
    names(nmsData)[nmsInt]
}

createSanctSummary <- function(db, nmsFolder) {
    nmsFiles <- list.files(nmsFolder, pattern='NMS', full.names=TRUE)
    nmsData <- lapply(nmsFiles, readRDS)
    names(nmsData) <- gsub('([A-z]NMS).*', '\\1', basename(nmsFiles))
    depData <- getDeploymentData(db)
    depData <- depData[c('Start', 'End', 'DriftName')]
    colnames(depData) <- c('DriftStart', 'DriftEnd', 'DriftName')
    depData$Sanctuaries <- 'Start or End times not updated'
    for(i in 1:nrow(depData)) {
        if(allZero(depData$DriftStart[i]) || allZero(depData$DriftEnd[i])) {
            next
        }
        depData$Sanctuaries[i] <- paste0(
            checkNMS(getDbDeployment(db, drift=depData$DriftName[i]), nmsData),
            collapse = ', ')
    }
    depData
}

createCombPlot <- function(images, filename, wide=TRUE) {
    grobs <- lapply(images, function(x) {
        rasterGrob(readPNG(x))
    })
    if(isTRUE(wide)) {
        combined <- arrangeGrob(grobs=grobs, nrow=1)
        ggsave(filename, combined, width=ncol(grobs[[1]]$raster) * length(combined$grobs),
               height = nrow(grobs[[1]]$raster), units='px')
    } 
    if(isFALSE(wide)) {
        combined <- arrangeGrob(grobs=grobs, ncol=1)
        ggsave(filename, combined, width=ncol(grobs[[1]]$raster),
               height = nrow(grobs[[1]]$raster) * length(combined$grobs), units='px')
    }
    filename
}

combineCurrents <- function(plots, outDir='.') {
    pat <- '(.*)_(HFRADAR|HYCOM|WCOFS)_(.*png$)'
    basenames <- gsub(pat, '\\1_\\3', plots)
    outfiles <- character(0)
    for(base in unique(basenames)) {
        thisPlots <- plots[basenames %in% base]
        outfiles <- c(outfiles,
                      createCombPlot(thisPlots, filename=file.path(outDir, basename(base)), wide=TRUE))
    }
    outfiles
}

# writes KML file for windy upload
# gps can be DB path or output from getDbDeployment
gpsToKml <- function(gps, drift=NULL, filename=NULL, extraLocs=NULL, 
                     contour=NULL, outDir='.', knotsLimit=4) {
    if(is.character(gps)) {
        gps <- getDbDeployment(gps, drift=drift, verbose=FALSE, knotsLimit=knotsLimit)
    }
    gps <- split(gps, gps$DriftName)
    tracks <- vector('list', length=length(gps))
    for(i in seq_along(gps)) {
        thisGps <- gps[[i]]
        thisGps <- arrange(thisGps, .data$UTC)
        thisFile <- paste0(filename, '_', thisGps$DriftName[1], '.kml')
        sfTrack <- st_as_sf(thisGps[c('Latitude', 'Longitude', 'DriftName')], 
                            coords=c('Longitude', 'Latitude'), crs=4326) %>% 
            st_combine() %>% 
            st_cast('LINESTRING') %>% 
            st_as_sf()
        sfTrack$DriftName <- thisGps$DriftName[1]
        names(sfTrack) <- c('geometry', 'DriftName')
        st_geometry(sfTrack) <- 'geometry'
        sfPoint <- st_as_sf(thisGps[nrow(thisGps), c('Latitude', 'Longitude', 'DriftName')],
                            coords=c('Longitude', 'Latitude'), crs=4326)
        sfCombined <- rbind(sfTrack, sfPoint)
        tracks[[i]] <- sfCombined
    }
    if(!is.null(extraLocs)) {
        names(extraLocs)[names(extraLocs) == 'name'] <- 'DriftName'
        sfExtra <- st_as_sf(extraLocs[c('Latitude', 'Longitude', 'DriftName')],
                            coords=c('Longitude', 'Latitude'), crs=4326)
        tracks[[length(tracks) + 1]] <- sfExtra
    }
    tracks <- do.call(rbind, tracks)
    if(!is.null(contour)) {
        contour <- readRDS(contour)
        tracks <- rbind(tracks, contour)
    }
    if(!is.null(filename)) {
        fileEnd <- substr(filename, nchar(filename) - 3, nchar(filename))
        if(fileEnd != '.kml') {
            filename <- paste0(filename , '.kml')
        }
        st_write(tracks, file.path(outDir, filename), append=FALSE, dataset_options=c('NameField=DriftName'))
        if(!is.null(contour)) {
            textKml <- read_file(file.path(outDir, filename))
            textKml <- gsub('<name>Contour_200</name>\n\t<Style><LineStyle><color>ff0000ff', 
                            '<name>Contour_200</name>\n\t<Style><LineStyle><color>ffffaf7d',
                            textKml)
            textKml <- gsub('<name>Contour_2000</name>\n\t<Style><LineStyle><color>ff0000ff', 
                            '<name>Contour_2000</name>\n\t<Style><LineStyle><color>fff58742',
                            textKml)
            textKml <- gsub('<name>Contour_4000</name>\n\t<Style><LineStyle><color>ff0000ff', 
                            '<name>Contour_4000</name>\n\t<Style><LineStyle><color>ffff0000',
                            textKml)
            
            write_file(textKml, file=file.path(outDir, filename))
        }
    }
    tracks
}

plotSpeedSummary <- function(db, days=7, units=c('knots', 'kmh'), message=TRUE,
                             tz='UTC', knotsLimit=4) {
    gps <- getDbDeployment(db, days=days, verbose=FALSE, knotsLimit=knotsLimit)
    gps$UTC <- with_tz(gps$UTC, tzone=tz)
    if(nrow(gps) == 0) {
        warning('No GPS in the last ', days, ' days')
        return(NULL)
    }
    lastPoint <- gps %>% 
        group_by(DriftName) %>% 
        arrange(desc(UTC)) %>% 
        slice(1) %>% 
        ungroup()
    switch(match.arg(units),
           'knots' = {
               ylab <- 'Knots'
               plotCol <- 'knots'
           },
           'kmh' = {
               ylab <- 'Km/Hour'
               plotCol <- 'kmh'
               gps$kmh <- gps$knots * 1.852
               lastPoint$kmh <- lastPoint$knots * 1.852
           }
    )
    g <- ggplot() +
        geom_line(data=gps, aes(x=UTC, y=.data[[plotCol]], col=DriftName)) +
        geom_label(data=lastPoint, aes(x=UTC, y=.data[[plotCol]], label=round(.data[[plotCol]], 2), col=DriftName),
                   fontface='bold', show.legend=FALSE) +
        geom_vline(xintercept=nowUTC()) +
        xlim(c(min(gps$UTC), nowUTC())) +
        ylab(ylab) +
        xlab(tz)
    if(message) {
        for(i in 1:nrow(lastPoint)) {
            thisMessage <- paste0(
                lastPoint$DriftName[i],
                ' last update ', 
                format(lastPoint$UTC[i], format='%Y-%m-%d %H:%M:%S', tz=tz),
                ' (', tz, '), ',
                round(lastPoint$Latitude[i], 6), ', ',
                round(lastPoint$Longitude[i], 6),'\n'
            )
            cat(thisMessage)
        }
    }
    g
}
