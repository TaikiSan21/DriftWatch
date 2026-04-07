# CAN ONLY GET 1 VARIABLE AT A TIME

# national data buoy center API access
# CSV specific time all stations in BBOX
url <- 'https://sdf.ndbc.noaa.gov/sos/server.php?request=GetObservation&service=SOS&version=1.0.0&offering=urn:ioos:network:noaa.nws.ndbc:all&featureofinterest=BBOX:-90,25,-85,30&observedproperty=air_pressure_at_sea_level&responseformat=text/csv&eventtime=2011-03-01T00:50Z'
# same above but SWL xml
url <- 'https://sdf.ndbc.noaa.gov/sos/server.php?
request=GetObservation
&service=SOS
&version=1.0.0
&offering=urn:ioos:network:noaa.nws.ndbc:all
&featureofinterest=BBOX:-90,25,-85,30
&observedproperty=air_pressure_at_sea_level
&responseformat=text/xml;subtype=%22om/1.0.0%22
&eventtime=2011-03-01T00:50Z'
# same above KML
url <- 'https://sdf.ndbc.noaa.gov/sos/server.php?
request=GetObservation
&service=SOS
&version=1.0.0
&offering=urn:ioos:network:noaa.nws.ndbc:all
&featureofinterest=BBOX:-90,25,-85,30
&observedproperty=air_pressure_at_sea_level
&responseformat=application/vnd.google-earth.kml%2bxml
&eventtime=2011-03-01T00:50Z'

library(httr)

tryGet <- GET(url=url, write_disk('Test.csv', overwrite=TRUE))
GET('https://sdf.ndbc.noaa.gov/sos/server.php?request=GetCapabilities&service=SOS')
GET("https://sdf.ndbc.noaa.gov/sos/server.php?\nrequest=GetObservation\n&service=SOS\n&version=1.0.0\n&offering=urn:ioos:network:noaa.nws.ndbc:all\n&featureofinterest=BBOX:-90,25,-85,30\n&observedproperty=air_pressure_at_sea_level\n&responseformat=text/xml;subtype=%22om/1.0.0%22\n&eventtime=2011-03-01T00:50Z")
