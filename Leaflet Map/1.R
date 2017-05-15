# Bring data in R
library(leaflet)
library(raster)
library(data.table)
setwd("/Users/Jimbook/Google Drive/Documents/BCCRS/Sample Report")
getwd()
fire <- fread("/Users/Jimbook/Google Drive/Documents/BCCRS/Sample Report/before.csv")
#---------------------------------------------
# Below codes assign one name to each variable I want to examine. Doing this makes my life easier.
CI <- fire$'Civilian Injuries'
CF <- fire$'Civilian Fatalities'
FI <- fire$'Fire Injuries'
FF <- fire$'Fire Fatalities'
SU <- fire$'Suppression Units'
SP <- fire$'Suppression Personnel'
EU <- fire$'EMS Units'
EP <- fire$'EMS Personnel'
OU <- fire$'Other Units'
OP <- fire$'Other Personnel'
SS <- 389180
AT <- fire$'Action Taken Primary'


# EMT but no injury map
fire.1 <- subset.data.frame(fire, ((FF==0)&(EP>=1)) |
                                   ((FI==0)&(EP>=1)) | 
                                   ((CF==0)&(EP>=1)) | 
                                   ((CI==0)&(EP>=1)))
Lat <- fire$Lat[((FF==0)&(EP>=1)) |
                       ((FI==0)&(EP>=1)) |
                       ((CF==0)&(EP>=1)) |
                       ((CI==0)&(EP>=1))]
Long <- fire$Long[((FF==0)&(EP>=1)) |
                         ((FI==0)&(EP>=1)) |
                         ((CF==0)&(EP>=1)) |
                         ((CI==0)&(EP>=1))]
sub1.map <- ggmap(mapImage) + geom_point( aes ( x= sub1.Long, y = sub1.Lat), color="dark blue", size = 0.1, data = sub1.deploy, alpha = .4) 
sub1.map





# Eyedrop Markers
leaflet(data = fire.1) %>% addTiles() %>% setView(lng = -122.43, lat = 37.75, zoom = 13) %>%
  addMarkers(~Long, ~Lat)
#------------------------------

# Clustered Count Markers
leaflet(fire.1) %>% addTiles()  %>% setView(lng = -122.43, lat = 37.77, zoom = 12) %>% 
  addMarkers(
    clusterOptions = markerClusterOptions()
  )
#------------------------------

# Circle Markers
leaflet(fire.1) %>% addTiles() %>% setView(lng = -122.43, lat = 37.77, zoom = 12) %>%
  addCircleMarkers(
    radius = 1,
    stroke = FALSE,
    fillOpacity = 0.5
  )
#------------------------------

textConnection("
                                City,Lat,Long,Pop
                                Boston,42.3601,-71.0589,645966
                                Hartford,41.7627,-72.6743,125017
                                New York City,40.7127,-74.0059,8406000
                                Philadelphia,39.9500,-75.1667,1553000
                                Pittsburgh,40.4397,-79.9764,305841
                                Providence,41.8236,-71.4222,177994
                                "))



(m <- leaflet() %>% addTiles())

m %>% setView(lng = -122.43, lat = 37.75, zoom = 13) # set centre and extent of map

(m2 <- m %>%
  setView(-122.43, 37.75, 12) %>% # map location
  addMarkers(-77.02, 38.9) %>% # add a marker
  addPopups(-122.43, 37.76, popup = "San Francisco") %>% # popup
  # add som circles:
  addCircles(color = "black", runif(90, -2, -1), runif(90, 53, 54), runif(90, 10, 500)))





# Bring data in R
library(data.table)
setwd("/Users/Jimbook/Google Drive/Documents/BCCRS/Sample Report")
getwd()
fire <- fread("before.csv")
fire.2016 <- fread("after.csv")


system.time(dt <- fread("before.csv"))
# user  system elapsed 
# 1.435   0.108   1.562 






#---------------------------------------------
# I am mapping because I am a nerd
# All deploy
library(ggmap)
library(mapproj)
sub0.Lat <- fire$Lat
sub0.Long <- fire$Long
mapImage <- get_map(location = c(lon = -122.43, lat = 37.76),
                    color = "color",
                    source = "google",
                    maptype = "terrain",
                    zoom = 12)
all.deploy.map <- ggmap(mapImage) + geom_point( aes ( x= sub0.Long, y = sub0.Lat), color="red", size = 0.01, data = fire, alpha = .4)
all.deploy.map



# EMT only but no injury map
sub2.deploy <- subset.data.frame(fire, ((FF==0)&(EP>=1)&(SP==0)&(SU==0)) |
                                   ((FI==0)&(EP>=1)&(SP==0)&(SU==0)) | 
                                   ((CF==0)&(EP>=1)&(SP==0)&(SU==0)) | 
                                   ((CI==0)&(EP>=1)&(SP==0)&(SU==0)))
sub2.Lat <- fire$Lat[((FF==0)&(EP>=1)&(SP==0)&(SU==0)) |
                       ((FI==0)&(EP>=1)&(SP==0)&(SU==0)) |
                       ((CF==0)&(EP>=1)&(SP==0)&(SU==0)) |
                       ((CI==0)&(EP>=1&(SP==0)&(SU==0)))]
sub2.Long <- fire$Long[((FF==0)&(EP>=1)&(SP==0)&(SU==0)) |
                         ((FI==0)&(EP>=1)&(SP==0)&(SU==0)) |
                         ((CF==0)&(EP>=1)&(SP==0)&(SU==0)) | 
                         ((CI==0)&(EP>=1)&(SP==0)&(SU==0))]
sub2.map <- ggmap(mapImage) + geom_point( aes ( x= sub2.Long, y = sub2.Lat), color="dark blue", size = 0.1, data = sub2.deploy, alpha = .4)
sub2.map