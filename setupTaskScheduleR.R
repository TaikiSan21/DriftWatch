library(taskscheduleR)
taskscheduler_create(taskname = "testDW", rscript = normalizePath('DriftWatch_Hourly.R'), 
                     schedule = "ONCE", starttime = format(Sys.time() + 62, "%H:%M"),
                     startdate = format(Sys.Date(), '%m-%d-%Y'))
