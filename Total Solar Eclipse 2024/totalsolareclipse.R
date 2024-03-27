#------------------------------------#
# Total Solar Eclipse 2024
# Data Exploration
# Aura Walmer
#------------------------------------#

setwd("/Volumes/AuraByte3/GT Sonification Lab/Total Solar Eclipse")
library(dplyr)

eclipse_path <- read.table("Data/center.txt", header = FALSE, sep = ",", skip = 1)
#scan(file("Data/center.txt"), what = "", nlines = 1, sep=",", quote = "\"",) 
colnames(eclipse_path) <- c("UTC","W lon","N lat","H","H+N","dur","alt","az",
                            "L","B","C","dist")

# Eclipse over U.S. territory seems to start around row 2061:
# 28.84455, -100.5763
# and ends around row 5899:
# 46.29391, -67.78109

write.csv(eclipse_path, "Data/eclipse_path_totality.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")

eclipse_path$longitude <- eclipse_path$`W lon`*(-1)
eclipse_path$latitude <- eclipse_path$`N lat`

## uploaded to https://geojson.io/#map=3.13/35.67/-78.59 to get the GeoJSON file

write.csv(eclipse_path[c("latitude","longitude")], "Data/eclipse_coordinates.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")

# subset within the U.S.:
eclipse_path_us <- eclipse_path[2062:5898,]
rownames(eclipse_path_us) <- NULL

## Preparing Lat/Lon coordinates for highcharts map:
hc_eclipse_coords <- paste0("[",
       eclipse_path_us$longitude[1:nrow(eclipse_path_us)],
       ",",
       eclipse_path_us$latitude[1:nrow(eclipse_path_us)],
       "]",
       collapse=",")

#3838 rows... get down to 20 coords? 3838/x=20 --> sample every 191th row? haha

hc_eclipse_coords_sub21 <- eclipse_path_us[c(seq(1,3837,191)),c("longitude","latitude")]
# get list of arrays:
paste0("[",
       hc_eclipse_coords_sub21$longitude[1:nrow(hc_eclipse_coords_sub21)],
       ",",
       hc_eclipse_coords_sub21$latitude[1:nrow(hc_eclipse_coords_sub21)],
       "]",
       collapse=",")

# new table for mapping
path_coords_30 <- eclipse_path_us[c(seq(1,3837,length.out=30)),c("longitude","latitude")]
seq(42, 86, length.out=30) # sonic pi
seq(25.29, 48.38, length.out = 30) # extreme south-north of US
# make the data mapping and then in loop assign which value in lat/lon table is closest to other mappping, 
# and grab the row val?

data_mapping_length30 <- data.frame("sonic_pi"=seq(42, 86, length.out=30),
                                    "coord_extremes"=seq(25.29, 48.38, length.out = 30)
                                    )

data_mapping_length30$mapping_id <- seq.int(nrow(data_mapping_length30))

write.csv(data_mapping_length30, "Data/data_mapping_length30.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")

which(abs(data_mapping_length30$coord_extremes-31.34141)==min(abs(data_mapping_length30$coord_extremes-31.34141)))

closest<-function(vector,value) {
  which(abs(vector-value)==min(abs(vector-value)))
}

path_coords_30$id_nearest <- NA
path_coords_30$corresponding_note <- NA
for (x in 1:nrow(path_coords_30)) {
  index <- closest(data_mapping_length30$coord_extremes, path_coords_30$latitude[x])
  path_coords_30$id_nearest[x] <- index
  path_coords_30$corresponding_note[x] <- data_mapping_length30$sonic_pi[index]
}

write.csv(path_coords_30, "Data/path_coords_30.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")

paste(round(path_coords_30$corresponding_note, digits=2), collapse=",")


hc_eclipse_coords_sub42 <- eclipse_path_us[c(seq(1,3837,90)),c("longitude","latitude")]

## create new data set where longitude coordinates are spaces by 1
# so rowr for longitude = -100, -99, -98... to -68
## (and see how slope changes based on latitude)
# then you can make audio changes based on how high the jump is per each lateral movement


## -- 3/28/2024 -- ##
# inspecting coordinates:
eclipse_path_us$lon2 <- round(eclipse_path_us$longitude, 2)
eclipse_path_us$lon1 <- round(eclipse_path_us$longitude, 1)
eclipse_path_us$lat1 <- round(eclipse_path_us$latitude, 1)

## Rounded coordinates + new data frame:
eclipse_path_us_sbst_1dec <- eclipse_path_us[!duplicated(eclipse_path_us$lon1), ]
eclipse_path_us_sbst_1dec$lon_int <- round(eclipse_path_us_sbst_1dec$lon1, 0)
eclipse_path_us_sbst_1dec$lat_int <- round(eclipse_path_us_sbst_1dec$lat1, 0)

## NOTE
# 18 notes in latitude just the right amount to be able to do G scale
# arpeggios, one note for each integer
# number of rows with that integer could dictate timing of arpeggios
# 

# timing of arpeggios mapped to number of rows of lat value
eclipse_path_us_sbst_1dec$lat_val_prcnt <- eclipse_path_us_sbst_1dec$lat_int/329 # 329 = nrow


# data mapping
eclipse_data_mapping <- eclipse_path_us_sbst_1dec[!duplicated(eclipse_path_us_sbst_1dec$lat_int), ]
eclipse_data_mapping <- eclipse_data_mapping[c("UTC","longitude","latitude",
                                               "lon1","lat1","lon_int","lat_int",
                                               "lat_val_prcnt")]
eclipse_data_mapping$lat_val_prcntx15 <- eclipse_data_mapping$lat_val_prcnt*15
# if the 15x variable is equal to seconds, can have start time
# cumulative sum starting at second row... (from previous row)
eclipse_data_mapping$start_time <- 0
eclipse_data_mapping$start_time[2:18] <- cumsum(eclipse_data_mapping$lat_val_prcntx15)[1:17]
# can change this scale potentially

## Prep data of appended rows for flourish
eclipse_path_us_sbst_clean <- eclipse_path_us_sbst_1dec[c("UTC","longitude","latitude",
                                                         "lon1","lat1","lon_int","lat_int")]

# starting data frame:
eclipse_path_us_sbst_clean$group <- "trajetory"
# now make separate data frame to rbind after
# horizontal line, vertical line, extreme coordinates, and major cities

# sides of triangle
horizontal <- data.frame("lon_int"=unique(eclipse_path_us_sbst_clean$lon_int),
                         "lat_int"=rep(29,34),
                         "group"="horizontal line")
vertical <- data.frame("lon_int"=rep(-68,18),
                         "lat_int"=unique(eclipse_path_us_sbst_clean$lat_int),
                         "group"="vertical line")
# major cities
# texas: san antonio [29.502, -98.537], austin [30.267, -97.743], dallas [32.777, -96.798]
# oklahmoa: 
# arkansas: little rock [34.748, -92.302]
# missouri: 
# kentucky: louisville [38.257, -85.765]
# indiana:
# illinois: Indianapolis, IN [39.7696, -86.155]
# ohio: cleveland [41.496, -81.669]
# new york: buffalo [42.886, -78.875], rochester [43.157, -77.606]
major_cities <- data.frame(
  "city_name" = c("San Antonio, TX","Austin, TX","Dallas, TX",
                  "Little Rock, AR","Louisville, KY","Indianapolis, IN",
                  "Cleveland, OH","Buffalo, NY","Rochester, NY"), # 9 cities
  "latitude"=c(29.502,30.267,32.777,34.748,38.257,39.7696,41.496,42.886,43.157),
  "longitude"=c(-98.537,-97.743,-96.798,-92.302,-85.765,-86.155,-81.669,-78.875,-77.606),
  "group"="major city")

extreme_coordinates <- data.frame(
  "city_name"=c("Seattle, WA","San Diego, CA"),
  # Seattle [47.647, -122.302], San Diego [32.714, -117.159]
  "label"=c(),
  "latitude"=c(),
  "longitude"=c(),
  "group"="extreme coordinate"
)

start_end <- data.frame(
  "label"=c("Trajectory Start","Trajectory End"),
  "latitude"=c(28.849,46.278),
  "longitude"=c(-100.571,-67.849),
  "group"=c("start","end")
)



