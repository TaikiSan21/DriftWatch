# DriftWatch Daily
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

cat('------------Script run on', as.character(Sys.time()), '---------------\n')
# recent buoy summary
cat('Creating recent buoy summary...\n')
recentBuoySummary(db, file='RecentBuoySummary.csv')
with_drive_quiet(drive_upload('RecentBuoySummary.csv', path=gdriveDest, type='spreadsheet', overwrite = TRUE))

# Archive older plots

# Check for new deployments

# GPS archive if needed
