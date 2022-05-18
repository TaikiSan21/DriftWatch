suppressPackageStartupMessages({
    library(dplyr)
    library(RSQLite)
    library(plotKML)
    library(xml2)
    # library(raster)
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
})
# Updated 1-19-22 post collapse v 1.0
# Updated 2-7-2022 fkin etopos
# Updated 5-2-2022 adding Lonestar
thisVersion <- function() {
    '1.2'
}

getSpotAPIData <- function(id='09m8vfKzAyrx3j1sSqVMCDamuAJKln1ys', db, start=1) {
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


getLonestarAPIData <- function(key='f22e32e6ba5953978f0875c86f07c5ffae24b2bc', db, start=NULL, days=30) {
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
    lsDf
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

getAPIData <- function(x, db, source=c('spot', 'lonestar'), progress=FALSE) {
    source <- match.arg(source)
    switch(source,
                 'spot' = {
                     start <- 1
                     df <- getSpotAPIData(x, db, start=start)
                     nFours <- 60
                     tryMax <- 15
                 },
                 'lonestar' = {
                     days <- 29
                     start <- nowUTC() - days * 24 * 3600
                     df <- getLonestarAPIData(x, db, start=start, days=days)
                     nFours <- 15
                     tryMax <- 12
                 }
    )
    isDupe <- checkDupeCoord(df, db)
    if(all(isDupe)) {
        return(NULL)
    }
    toAdd <- df[!isDupe, ]
    toAdd$UTC <- as.character(toAdd$UTC)
    # go again if everything was new - means more to get
    # if(start == 1) {
    tryMax <- 10
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
                   newData <- getSpotAPIData(x, db, start=start)
               },
               'lonestar' = {
                   start <- start - days * 24 * 3600
                   newData <- getLonestarAPIData(x, db, start=start, days=days)
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
        newData$UTC <- as.character(newData$UTC)
        toAdd <- distinct(bind_rows(toAdd, newData))
        if(any(isDupe)) {
            break
        }
        nTry <- nTry + 1
    }
    badCoords <- toAdd$Latitude < -90 | toAdd$Longitude < -180
    toAdd <- toAdd[!badCoords, ]
    toAdd <- arrange(toAdd, UTC)
    toAdd
}

# x is key or id for API
addAPIToDb <- function(x='09m8vfKzAyrx3j1sSqVMCDamuAJKln1ys', db, source=c('spot', 'lonestar')) {
    con <- dbConnect(db, drv=SQLite())
    on.exit(dbDisconnect(con))
    dbDf <- dbReadTable(con, 'gpsData')
    dbDf$UTC <- pgDateToPosix(dbDf$UTC)
    apiData <- getAPIData(x, dbDf, source)

    if(!is.null(apiData) &&
       nrow(apiData) > 0) {
        dbAppendTable(con, 'gpsData', apiData)
    }
    dbLog <- dbReadTable(con, 'gpsLogger')
    logId <- ifelse(nrow(dbLog) == 0, 1, max(dbLog$Id) + 1)
    now <- nowUTC()
    sourceName <- switch(match.arg(source),
                         'spot' = 'SPOT API',
                         'lonestar' = 'Lonestar API'
    )
    logAppend <- data.frame(Id = logId, UTC = as.character(now),
                            RowsAdded = ifelse(is.null(apiData), 0, nrow(apiData)),
                            Source = sourceName)
    dbAppendTable(con, 'gpsLogger', logAppend)
    apiData
}

psxToSpot <- function(x) {
    x <- as.character(x)
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
    # browser()
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

gpxToDf <- function(x) {
    gpx <- plotKML::readGPX(x)
    format <- '%Y-%m-%dT%H:%M:%SZ'
    result <- do.call(rbind, lapply(gpx$tracks, function(x) {
        tmp <- vector('list', length = length(x))
        for(i in seq_along(x)) {
            # browser()
            df <- x[[i]][, c('lon', 'lat', 'time')]
            df$DeviceName <- names(x)[i]
            tmp[[i]] <- df
        }
        bind_rows(tmp)
    }))
    colnames(result) <- c('Longitude', 'Latitude', 'UTC', 'DeviceName')
    result$UTC <- as.POSIXct(result$UTC, tz='UTC', format=format)
    badCoords <- result$Latitude < -90 | result$Longitude < -180
    result[!badCoords, ]
}

addGpxToDb <- function(gpx, db) {
    gpxDf <- gpxToDf(gpx)
    con <- dbConnect(db, drv=SQLite())
    on.exit(dbDisconnect(con))
    dbDf <- dbReadTable(con, 'gpsData')
    dbDf$UTC <- pgDateToPosix(dbDf$UTC)
    isDupe <- checkDupeCoord(gpxDf, dbDf)
    if(!all(isDupe)) {
        toAdd <- gpxDf[!isDupe, ]
        gpsAppend <- dbDf[FALSE, ]
        gpsAppend[1:nrow(toAdd), ] <- NA
        gpsAppend$Latitude <- toAdd$Latitude
        gpsAppend$Longitude <- toAdd$Longitude
        gpsAppend$DeviceName <- toAdd$DeviceName
        gpsAppend <- arrange(gpsAppend, UTC)
        gpsAppend$UTC <- as.character(toAdd$UTC)
        ixPoss <- 1:(nrow(dbDf) + nrow(toAdd))
        ixPoss <- ixPoss[!(ixPoss %in% dbDf$Id)]
        gpsAppend$Id <- ixPoss[1:nrow(toAdd)]
        dbAppendTable(con, 'gpsData', gpsAppend)
    } else {
        gpsAppend <- NULL
    }
    dbLog <- dbReadTable(con, 'gpsLogger')
    logId <- ifelse(nrow(dbLog) == 0, 1, max(dbLog$Id) + 1)
    now <- nowUTC()
    logAppend <- data.frame(Id = logId, UTC = as.character(now),
                            RowsAdded = ifelse(all(isDupe), 0, nrow(gpsAppend)),
                            Source = 'GPX')
    dbAppendTable(con, 'gpsLogger', logAppend)
    gpsAppend
}

nowUTC <- function() {
    now <- Sys.time()
    attr(now, 'tzone') <- 'UTC'
    now
}

# returns which rows are duplicated in the first df
checkDupeCoord <- function(x, y, coords=c('UTC', 'Longitude', 'Latitude')) {
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

getDbDeployment <- function(db, drift=NULL, days=NULL) {
    con <- dbConnect(db, drv=SQLite())
    on.exit(dbDisconnect(con))
    gps <- dbReadTable(con, 'gpsData')
    gps$UTC <- pgDateToPosix(gps$UTC)
    deployment <- dbReadTable(con, 'deploymentData')
    deployment$Start <- pgDateToPosix(deployment$Start)
    deployment$End <- pgDateToPosix(deployment$End)
    if(!is.null(drift)) {
        deployment <- deployment[deployment$DriftName %in% drift, ]
    }
    if(is.null(deployment) ||
       nrow(deployment) == 0) {
        return(NULL)
    }
    if(any(is.na(deployment$End))) {
        deployment$End[is.na(deployment$End)] <- nowUTC()
    }
    allGps <- bind_rows(lapply(deployment$DriftName, function(x) {
        thisDep <- deployment[deployment$DriftName == x, ]
        thisGps <- gps[gps$DeviceName %in% gsub(' ', '', strsplit(thisDep$DeviceName, ',')[[1]]), ]
        thisGps <- thisGps[thisGps$UTC >= thisDep$Start &
                               thisGps$UTC <= thisDep$End, ]
        thisGps$DriftName <- x
        thisGps$DeploymentSite <- unique(thisDep$DeploymentSite)
        thisGps
    }))
    if(is.null(days)) {
        return(allGps)
    }
    filter(allGps, UTC >= (nowUTC() - days * 24 * 3600))
}

plotAPIDrift <- function(drift, etopo = 'etopo180.nc', filename=NULL, bathy=TRUE, sl=TRUE, wca=TRUE,
                         current=TRUE, wind=FALSE, swell=FALSE, wave=FALSE, depth=0, time=nowUTC(),
                         size = 4, xlim=1, ylim=.5, labelBy='DriftName', title=NULL,
                         dataPath='.') {
    if(is.null(etopo)) {
        etopo <- file.path(dataPath, 'etopo180.nc')
    } else {
        etopo <- file.path(dataPath, etopo)
    }
    rangeDf <- makeRangeDf()
    # browser()
    if(!file.exists(etopo)) {
        # edi <- erddapToEdinfo('etopo180', chooseVars = FALSE)
        # edi <- varSelect(edi, TRUE)
        # etopo <- downloadEnv(drift, edinfo=edi, fileName = etopo, buffer=c(1, 1, 0))
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
       ylim[1] < rangeDf$Latitude[1] ||
       ylim[2] > rangeDf$Latitude[2]) {
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
    # xlim[1] <- max(c(xlim[1], rangeDf$Longitude[1]))
    # xlim[2] <- min(c(xlim[2], rangeDf$Longitude[2]))
    # ylim[1] <- max(c(ylim[1], rangeDf$Latitude[1]))
    # ylim[2] <- min(c(ylim[2], rangeDf$Latitude[2]))


    bathyData <- as.bathy(raster::raster(etopo))
    # browser()
    bathyData <- try(subsetBathy(bathyData, x=xlim, y=ylim, locator = FALSE), silent=TRUE)
    if(inherits(bathyData, 'try-error')) {
        # return(plotAPIDrift(drift, etopo=NULL, filename, bathy, sl, wca, current,
        #                     wind, swell, wave, depth,time, size, xlim, ylim, labelBy, title, dataPath))
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
        plot(windCall$geometry, add=TRUE, border='purple', lwd=2)
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
               }
        )
        # browser()
        updateNc(currentNc, edi, vars=curVar)
        curLeg <- plotArrowGrid(xLim=xlim, yLim=ylim, diff=NULL, nc=currentNc,
                                xyVars = xyVars, depth=depth, time=time,
                                width=width, radial=radial)

        curLeg$vals <- round(curLeg$vals * 1.94384, 2)
        title(sub = paste0('Current data as of ', curLeg$time, ' UTC'))
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
        # start/end markers
        points(x=thisDrift$Longitude[start], y=thisDrift$Latitude[start], pch=15, cex=1.2)
        end <- which.max(thisDrift$UTC)
        points(x=thisDrift$Longitude[end], thisDrift$Latitude[end], pch=17, col='grey', cex=1.2)
        # labels
        points(x=thisDrift$Longitude[start], y=thisDrift$Latitude[start], pch=mapLabs[d], col='white', cex=.6)
        points(x=thisDrift$Longitude[end], y=thisDrift$Latitude[end], pch=mapLabs[d], col='black', cex=.6)
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

    myScaleBathy(bathyData, deg=diff(xlim) * .2, x= 'bottomleft', inset=5, col='white')
    if(is.null(filename)) {
        return(drift)
    }
    filename
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
    as.POSIXct(as.character(x), format='%Y-%m-%d %H:%M:%OS', tz='UTC')
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
    # browser()
    llg <- makeLatLongGrid(xLim, yLim, diff[1], diff[2], depth, time=time)
    diff <- diff * scale
    llg <- ncToData(llg, nc, FUN=mean,verbose = FALSE, progress=FALSE)
    llg <- PAMmisc:::to180(llg)
    varNames <- paste0(xyVars, '_mean')
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
    llg$x_scale <- llg[[varNames[1]]] / max(abs(llg[[varNames[1]]]), na.rm=TRUE) * diff[1]
    llg$y_scale <- llg[[varNames[2]]] / max(abs(llg[[varNames[2]]]), na.rm=TRUE) * diff[2]
    llg$mag_scale <- sqrt(llg$x_scale^2 + llg$y_scale^2)
    llg$mag_scale <- llg$mag_scale / max(llg$mag_scale, na.rm=TRUE)
    llg$mag <- sqrt(llg[[varNames[1]]]^2 + llg[[varNames[2]]]^2)
    aLen <- width / diff(xLim) * diff[1] * .3
    # aCols <- colorRampPalette(c('green', 'yellow'))(32)
    aCols <- viridis::viridis(32, option='viridis', begin=.6)
    llg$magColor <- llg$mag / max(llg$mag, na.rm=TRUE) * length(aCols)
    for(i in 1:nrow(llg)) {
        if(is.na(llg$mag_scale[i])) next
        arrows(x0=llg$Longitude[i], x1=llg$Longitude[i] + llg$x_scale[i],
               y0=llg$Latitude[i], y1=llg$Latitude[i] + llg$y_scale[i],
               length = aLen * llg$mag_scale[i], col=aCols[llg$magColor[i]], angle=20)
    }
    legIx <- seq(from=1, to=length(aCols), length.out=7)
    legCols <- aCols[legIx]
    legVals <- round(seq(from=0, to=max(llg$mag, na.rm=TRUE), length.out=7), 2)
    legVals[c(2,4,6)] <- NA
    list(colors=legCols, vals=legVals, time=llg$matchTime_mean[1])
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
    rangeDf <- makeRangeDf()
    if(file.exists(file)) {
        fInfo <- file.info(file)
        ncTry <- nc_open(file, return_on_error=TRUE)
        if(isFALSE(ncTry$error) &&
           as.numeric(difftime(Sys.time(), fInfo$mtime, units='days')) < .5) {
            return(TRUE)
        }
        if(isTRUE(ncTry$error)) {
            msg <- suppressWarnings(read.table(file, sep='\n'))
            msg <- msg[1,1]
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
    # browser()
    if(inherits(edi, 'hycomList')) {
        whichHy <- PAMmisc:::whichHycom(rangeDf$UTC[2], edi)
        edi <- edi$list[[whichHy]]
        timeLim <- hycomToEdinfo(edi$dataset, chooseVars = FALSE)
        rangeDf$UTC <- timeLim$limits$UTC[2]
    }
    edi <- varSelect(edi, vars)
    tryCatch({
        downloadEnv(rangeDf, edi, fileName=file, buffer=c(.1, .1, 0), progress = FALSE)
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

plotTestDeployments <- function(sheet='~/DriftWatch/TestDeployments',
                                file = 'TestDeployments.csv',
                                dataPath='.',
                                etopo='etopo180.nc',
                                current=2) {
    with_drive_quiet({
        drive_download(sheet, path=file, overwrite = TRUE)
    })
    deps <- readr::read_csv(file,
                            col_types=list(col_double(), col_double(),
                                           col_character(), col_character(),
                                           col_character(), col_character(), col_character()))
    naCols <- is.na(deps$Latitude) | is.na(deps$Longitude)
    deps <- deps[!naCols, ]
    colnames(deps)[3] <- 'DriftName'
    deps$UTC <- nowUTC()
    nDeps <- nrow(deps)
    deps <- rbind(deps, deps)
    deps$UTC[1:nDeps] <- deps$UTC[1:nDeps] - 1
    deps$LatRange[is.na(deps$LatRange)] <- '.5'
    deps$LonRange[is.na(deps$LonRange)] <- '1'

    outFiles <- character(0)
    for(i in unique(deps$DriftName)) {
        thisDrift <- deps %>% filter(DriftName == i)
        if(is.null(thisDrift$DeploymentSite) ||
           is.na(unique(thisDrift$DeploymentSite))) {
            thisFile <- paste0(i, '.png')
        } else {
            thisFile <- paste0(unique(thisDrift$DeploymentSite),'_',
                               i, '.png')
        }
        # depTimeLocal <- min(thisDrift$UTC) - 7 * 3600
        # depTimeLocal <- as.character(format(depTimeLocal, format='%m-%d-%Y'))
        # thisFile <- paste0(depTimeLocal, '_',thisFile)
        if(nrow(thisDrift) > 2) {
            thisDrift$DriftName <- paste0(thisDrift$DriftName, '_', rep(1:(nrow(thisDrift)/2), 2))
        }
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
                                   xlim=lonr, ylim=latr)
        )
    }
    outFiles
}

addToGps <- function(x, maxSpeed = 4 / 3.6) {
    if(is.null(x) ||
       nrow(x) == 0) {
        return(x)
    }
    if(nrow(x) == 1) {
        x$Distance <- NA
        x$Speed <- NA
        x$Bearing <- NA
        return(x)
    }
    x <- arrange(x, UTC)
    timeDiff <- as.numeric(difftime(x$UTC[2:(nrow(x))], x$UTC[1:(nrow(x)-1)], units='secs'))
    distance <- distGeo(cbind(x$Longitude[1:(nrow(x)-1)], x$Latitude[1:(nrow(x)-1)]),
                        cbind(x$Longitude[2:(nrow(x))], x$Latitude[2:(nrow(x))]))
    x$Distance <- c(distance, 0)
    x$Speed <- c(distance / timeDiff, 0)
    tooFast <- x$Speed > maxSpeed
    if(any(tooFast)) {
        return(addToGps(x[!tooFast,], maxSpeed))
    }
    bearing <- bearing(cbind(x$Longitude[1:(nrow(x)-1)], x$Latitude[1:(nrow(x)-1)]),
                       cbind(x$Longitude[2:(nrow(x))], x$Latitude[2:(nrow(x))]))
    x$Bearing <- c(bearing %% 360, NA)
    x
}

plotAnneDrift <- function(drift, etopo = 'etopo180.nc', filename=NULL, bathy=TRUE, sl=TRUE, wca=TRUE,
                          current=FALSE, wind=FALSE, swell=FALSE, wave=FALSE, depth=0, time=nowUTC(),
                          size = 4, xlim=1, ylim=.5, labelBy='DriftName', title=NULL,
                          dataPath='../Data/SPOTXML',
                          pal = 'Set2') {
    if(is.null(etopo)) {
        etopo <- tempfile()
    } else {
        etopo <- file.path(dataPath, etopo)
    }
    if(!file.exists(etopo)) {
        edi <- erddapToEdinfo('etopo180', chooseVars = FALSE)
        edi <- varSelect(edi, TRUE)
        etopo <- downloadEnv(drift, edinfo=edi, fileName = etopo, buffer=c(1, 1, 0))
    }
    rangeDf <- makeRangeDf()
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
    xlim[1] <- max(c(xlim[1], rangeDf$Longitude[1]))
    xlim[2] <- min(c(xlim[2], rangeDf$Longitude[2]))
    ylim[1] <- max(c(ylim[1], rangeDf$Latitude[1]))
    ylim[2] <- min(c(ylim[2], rangeDf$Latitude[2]))

    bathyData <- as.bathy(raster(etopo))
    # browser()
    bathyData <- try(subsetBathy(bathyData, x=xlim, y=ylim, locator = FALSE), silent=TRUE)
    if(inherits(bathyData, 'try-error')) {
        return(plotAnneDrift(drift, etopo=NULL, filename, bathy, sl, wca, current,
                             wind, swell, wave, depth,time, size, xlim, ylim, labelBy, title, dataPath, pal))
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
        plot(windCall$geometry, add=TRUE, border='purple', lwd=2)
    }
    mbnms <- readRDS(file.path(dataPath, 'MBNMS_Bounds.rda'))
    plot(mbnms$geometry, add=TRUE, border='blue', lwd=1)
    gfnms <- readRDS(file.path(dataPath, 'GFNMS_Bounds.rda'))
    plot(gfnms$geometry, add=TRUE, border='blue', lwd=1)
    # cinms <- readRDS(file.path(dataPath, 'CINMS_Bounds.rda'))
    # plot(cinms$geometry, add=TRUE, border='blue', lwd=1)
    cbnms <- readRDS(file.path(dataPath, 'CBNMS_Bounds.rda'))
    plot(cbnms$geometry, add=TRUE, border='blue', lwd=1)
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
                   edi <- PAMmisc::getEdinfo()[[2]]
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
               }
        )

        updateNc(currentNc, edi, vars=curVar)
        curLeg <- plotArrowGrid(xLim=xlim, yLim=ylim, diff=NULL, nc=currentNc,
                                xyVars = xyVars, depth=depth, time=time,
                                width=width, radial=radial)

        curLeg$vals <- round(curLeg$vals * 1.94384, 2)
        title(sub = paste0('Current data as of ', curLeg$time, ' UTC'))
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

    # Plotting drift and start/end points
    symbs <- c(48:57, 35:38, 61:64, 131, 132, 163, 165, 167, 169, 171, 174, 97:122)
    mapLabs <- (49:57)[as.numeric(gsub('ADRIFT_00', '', unique(drift[[labelBy]])))]
    nColors <- length(unique(drift[[labelBy]]))
    colPal <- brewer.pal(nColors, pal)
    for(d in seq_along(unique(drift[[labelBy]]))) {
        thisDrift <- drift[drift[[labelBy]] == unique(drift[[labelBy]])[d], ]
        if(nrow(thisDrift) == 1) next
        start <- which.min(thisDrift$UTC)
        thisDrift <- arrange(thisDrift, UTC)
        lines(x=thisDrift$Longitude, y=thisDrift$Latitude, col='black', lwd=2)
        lines(x=thisDrift$Longitude, y=thisDrift$Latitude, col=colPal[d], lwd=1, lty=1)
        # start/end markers
        points(x=thisDrift$Longitude[start], y=thisDrift$Latitude[start], pch=15, cex=1.3, col='black')
        points(x=thisDrift$Longitude[start], y=thisDrift$Latitude[start], pch=15, cex=1.2, col=colPal[d])
        end <- which.max(thisDrift$UTC)
        points(x=thisDrift$Longitude[end], thisDrift$Latitude[end], pch=17, col='black', cex=1.3)
        points(x=thisDrift$Longitude[end], thisDrift$Latitude[end], pch=17, col=colPal[d], cex=1.2)
        # labels
        points(x=thisDrift$Longitude[start], y=thisDrift$Latitude[start], pch=mapLabs[d], col='black', cex=.6)
        points(x=thisDrift$Longitude[end], y=thisDrift$Latitude[end], pch=mapLabs[d], col='black', cex=.6)
    }

    # Plot port cities and other POI
    text(x=poiDf$Longitude, y=poiDf$Latitude, labels=poiDf$Name, cex=.6, srt=30, adj=c(-.1, .5))
    points(x=poiDf$Longitude, y=poiDf$Latitude, cex=.5, pch=16)

    # Legendary
    # browser()
    lastLegend <- legend(x='topright', legend=c('Deployment', 'Recovery'),
                         col=c('black', 'black'), pch=c(15, 17), merge=FALSE,
                         seg.len = 1, cex=1, plot=FALSE)
    useCex <- min(.2 * diff(xlim) / lastLegend$rect$w, 1)
    lastLegend <- legend(x='topright', legend=c('Deployment', 'Recovery'),
                         col=c('black', 'black'), pch=c(15, 17), merge=FALSE,
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
        # lastLegend <- myGradLegend(lastLeg=lastLegend, vals=bVals, cols=bCols,
        #                            title='      Depth (m)', cex=useCex, tCex=.67)
    }

    myScaleBathy(bathyData, deg=diff(xlim) * .2, x= 'bottomleft', inset=5, col='white')
}

# Spot: jaybarlow33 // 3Spot99-1
# SpotAPI : 09m8vfKzAyrx3j1sSqVMCDamuAJKln1ys

doDriftPlots <- function(depGps, dataPath='.', current=4, verbose=FALSE) {
    if(is.character(depGps)) {
        depGps <- getDbDeployment(depGps)
    }
    outFiles <- character(0)
    for(d in unique(depGps$DriftName)) {
        thisDep <- depGps[depGps$DriftName == d, ]
        fname <- paste0(format(min(thisDep$UTC), format='%m-%d-%Y'))
        if(!is.na(thisDep$DeploymentSite[1])) {
            fname <- paste0(fname, '_', thisDep$DeploymentSite[1])
        }
        fname <- file.path(dataPath, paste0(fname, '_', thisDep$DriftName[1], '.png'))
        if(file.exists(fname)) {
            modtime <- file.info(fname)$mtime
            attr(modtime, 'tzone') <- 'UTC'
            if(modtime > max(thisDep$UTC)) next
        }
        if(verbose) {
            cat('Plotting drift', thisDep$DriftName[1], '\n')
        }
        outFiles <- c(outFiles,
                  plotAPIDrift(thisDep, filename=fname, current=current, dataPath=dataPath))
    }
    outFiles
}

sheetToDbDepFormat <- function(x, new=FALSE) {
    if(new) {
        selCols <- c('Deployment_Date_UTC',
                     'Data_Start_UTC',
                     'Recovery_Date_UTC',
                     'Data_End_UTC',
                     "GPS ID (if appropriate - top / bottom)",
                     'Project',
                     'DeploymentID',
                     'Site'
        )
    } else {
        selCols <- c('Deployment_Date',
                     'Data_Start',
                     'Recovery_Date',
                     'Data_End',
                     'GPS_ID',
                     'Project',
                     'DeploymentID',
                     'Site'
        )
    }
    dbNames <- c('Start', 'DataStart','End','DataEnd', 'DeviceName', 'DriftName', 'DeploymentID', 'DeploymentSite')
    x <- dplyr::select(x, all_of(selCols))
    colnames(x) <- dbNames
    if(new) {
        x <- x[-1, ]
    } else {
        x <- x[!is.na(x$DriftName), ]
    }
    x$DeploymentID <- ifelse(is.na(x$DeploymentID), '', x$DeploymentID)
    for(i in 1:nrow(x)) {
        x$DeploymentID[i] <- paste0(paste0(rep(0, 3-nchar(x$DeploymentID[i])), collapse=''), x$DeploymentID[i])
        for(d in c('DataStart', 'Start', 'End', 'DataEnd')) {
            val <- x[[d]][i]
            if(is.null(val) ||
               is.na(val) ||
               val == 'NA') {
                x[[d]][i] <- NA
            }
        }
        if(!is.na(x$DataStart[i])) {
            x$Start[i] <- x$DataStart[i]
        }
        # if(!is.na(x$DataEnd[i])) {
        #     x$End[i] <- x$DataEnd[i]
        # }
    }
    for(j in c('Start', 'End')) {
        x[[j]] <- gsub('\\s{0,1}UTC$', '', x[[j]])
        x[[j]] <- gsub('\\s{0,1}AM$', '', x[[j]])
        x[[j]] <- gsub('\\s{0,1}PM$', '', x[[j]])
        x[[j]] <- PAMmisc:::parseToUTC(x[[j]],
                                       format=c('%m/%d/%Y %H:%M:%S', '%m/%d/%Y'),
                                       tz='UTC')
    }
    x$DriftName <- paste0(x$DriftName, '_', x$DeploymentID)
    x$DeploymentID <- NULL
    x$DataStart <- NULL
    x$DataEnd <- NULL
    x$DeviceName <- gsub('\\/', ', ', x$DeviceName)
    drops <- is.na(x$Start) | x$DeviceName == 'NA'
    x[!drops, ]
}

# with_drive_quiet({
#     allPngs <- list.files(pattern='png$', full.names = TRUE, recursive = FALSE)
#     for(i in seq_along(allPngs)) {
#         modtime <- file.info(allPngs[i])$mtime
#         diff <- as.numeric(difftime(Sys.time(), modtime, units='hours'))
#         if(diff > 1) next
#         drive_upload(allPngs[i], path=gdriveDest, overwrite = TRUE)
#     }
# })

doGdriveUpload <- function(files, destination, modHours=1) {
    with_drive_quiet({
        for(i in seq_along(files)) {
            modtime <- file.info(files[i])$mtime
            diff <- as.numeric(difftime(Sys.time(), modtime, units='hours'))
            if(diff > modHours) next
            drive_upload(files[i], path=destination, overwrite=TRUE)
        }
    })
}
