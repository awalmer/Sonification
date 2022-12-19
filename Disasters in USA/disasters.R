## Federally Declared Disasters in the USA since 1953 ##
## FEMA: Federal Emergency Management Agency (US D of Homeland Sec) ##
## August, 2022 | Auralee Walmer ##
# LINKS: 
#   [FEMA Data: Disaster Declarations Summary] https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2
#   [FEMA Data FAQ] https://www.fema.gov/about/openfema/faq


# Set up #
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)   
library(openxlsx)
library(ggplot2)

setwd("/Volumes/AuraByte2/Data Projects/Disasters in USA/")

# Call FEMA API #
res = GET("https://www.fema.gov/api/open/v2/DisasterDeclarationsSummaries")
rawToChar(res$content)
content = fromJSON(rawToChar(res$content))
names(content)
data = content$DisasterDeclarationsSummaries
# Now extend limitation on records returned: (default 1,000)
res = GET("https://www.fema.gov/api/open/v2/DisasterDeclarationsSummaries?$top=100000")
content = fromJSON(rawToChar(res$content))
data = fromJSON(rawToChar(res$content))$DisasterDeclarationsSummaries
# 63,215 records
# Note: this is not the total number of disasters. There are potentially multiple rows for a given disaster affecting more than one county.

# Summary Tables
freq_by_year <- 
data %>%
  group_by(fyDeclared) %>%
  summarise(distinct_incidents = n_distinct(disasterNumber),
            types = n_distinct(incidentType))

type_count_per_year <- 
data %>%
  group_by(fyDeclared, incidentType) %>%
  summarise(distinct_incidents = n_distinct(disasterNumber))

incident_count_wide <- # pivoted table with incident type as columns
type_count_per_year %>%
  pivot_wider(names_from = incidentType, values_from = distinct_incidents)
incident_count_wide$Total <- rowSums(incident_count_wide[,c(2:ncol(incident_count_wide))], na.rm=TRUE)
incident_count_wide <- incident_count_wide[,c(1,25,2:24)]

# Export
# write.xlsx(data, 'fema_disaster_data.xlsx', sheetName = 'All Data', row.names=FALSE)
openxlsx::write.xlsx(freq_by_year, 'datasets/disaster_count_per_year.xlsx', sheetName = 'Disaster Count Per Year', rowNames=FALSE)
openxlsx::write.xlsx(incident_count_wide, 'datasets/disaster_count_by_type_wide.xlsx', sheetName = 'Number Incidents byType pYear', rowNames=FALSE)


# Exploration
ggplot(incident_count_wide, aes(fyDeclared, Fire)) + geom_point()
ggplot(incident_count_wide, aes(fyDeclared, Flood)) + geom_point()
ggplot(incident_count_wide, aes(fyDeclared, Tornado)) + geom_point()
ggplot(incident_count_wide, aes(fyDeclared, Hurricane)) + geom_point()

incident_count_wide[!is.na(incident_count_wide$Fire),]

# Simplified Data / Data of interest:
icw_subset <- incident_count_wide[
  incident_count_wide$fyDeclared>=1981 & incident_count_wide$fyDeclared<=2021,
  c("fyDeclared","Fire","Flood","Tornado","Hurricane")
  ]
icw_subset$Total = 
  rowSums(icw_subset[,c(2:ncol(icw_subset))], 
          na.rm=TRUE)

icw_subset$sonicpi_numeric = 50 + icw_subset$Total*.25 # range from 50 to 100
# audible in sonic pi; normalize range
write.table(matrix(icw_subset$sonicpi_numeric,nrow=1), sep=",",
            row.names=FALSE, col.names=FALSE) # for "play_pattern_timed" in sonic pi
# Useful Sonic Pi docs:
# https://github.com/sonic-pi-net/sonic-pi/tree/dev/etc/doc/tutorial





# Ideas
# map of state colored by how many disasters per state
# what about just fires in california, merged with max temp over time?
# (sonification: tone pitch by temp, and instances of fire)
# data on temperature in usa: https://www.ncei.noaa.gov/cag/statewide/time-series/4/tmin/ann/12/1950-2022?base_prd=true&begbaseyear=1950&endbaseyear=2022

# what about low pitch = fewer instances total, higher pitch = more instances?
# amplitude of sound sample correlated with number of specific disaster instances?
# could have loopable sound that gets increased in amplitude the more it happens, 
# so that way it's more of a continuous growth
