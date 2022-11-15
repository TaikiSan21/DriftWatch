# drifts in nms boundaries
ad12 <- getDbDeployment(db, 'ADRIFT_012')
nmsFiles <- list.files('PlottingData/', pattern='NMS', full.names = T)
nmsData <- lapply(nmsFiles, function(x) {
    # st_transform(readRDS(x), 4326)
    readRDS(x)
})
names(nmsData) <- gsub('([A-z]*NMS).*', '\\1', basename(nmsFiles))
library(sf)

sfGps <- st_sfc(st_multipoint(matrix(c(ad12$Longitude, ad12$Latitude), ncol=2)))
crs <- st_crs(nmsData[[1]])
st_crs(sfGps) <- crs
plot(sfGps)
st_intersection(sfGps, nmsData[[1]])
plot(sfGps)
plot(nmsData[[1]]$geometry, add=T)
plot(nmsData[[2]]$geometry)
plot(nmsData[[3]]$geometry)
plot(nmsData[[4]]$geometry)
plot(st_sfc(nmsData[[1]]))
allNms <- c(nmsData[[1]]$geometry, nmsData[[2]]$geometry, nmsData[[3]]$geometry, nmsData[[4]]$geometry)
allDrift <- getDbDeployment(db)
allDriftSf <- lapply(split(allDrift, allDrift$DriftName), function(x) {
    x <- arrange(x, UTC)
    if(nrow(x) <= 1)  return(NULL)
    tmp <- st_sfc(st_multipoint(matrix(c(x$Longitude, x$Latitude), ncol=2)))
    st_crs(tmp) <- 4326
    tmp
})
allDriftSf <- allDriftSf[sapply(allDriftSf, function(x) !is.null(x))]
allDriftSf <- do.call(c, allDriftSf)
plot(c(allDriftSf[[3]], allNms))

ints <- lapply(allDriftSf, function(x) {
    st_intersection(x, allNms)
})
hm <- st_intersection(allNms[1], allDriftSf[[3]])
plot(hm)
plot(allDriftSf[[3]])
plot(allNms[1])
plot(allDriftSf[[3]], add=T)

st_intersects(nmsData[[2]], allDriftSf[[3]])
hm <- unlist(lapply(nmsData, function(x) {
    ints <- unlist(st_intersects(x, allDriftSf[[3]]))
    length(ints) > 0
}))
    


checkNMS(getDbDeployment(db, 'ADRIFT_001'), 'PlottingData')

depData <- getDeploymentData(db)
depData <- depData[c('Start', 'End', 'DriftName')]
colnames(depData) <- c('DriftStart', 'DriftEnd', 'DriftName')
depData$Sanctuaries <- 'Start or End times not updated'
for(i in 1:nrow(depData)) {
    if(allZero(depData$DriftStart[i]) || allZero(depData$DriftEnd[i])) {
        next
    }
    depData$Sanctuaries[i] <- paste0(
        checkNMS(getDbDeployment(db, drift=depData$DriftName[i]), 'PlottingData'),
        collapse = ', ')
}
ints <- lapply(split(allDrift, allDrift$DriftName), function(x) {
    checkNMS(x, 'PlottingData')
})


ss <- createSanctSummary(db, 'PlottingData')
