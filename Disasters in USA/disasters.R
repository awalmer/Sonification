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
# most prevalent types: fire, severe storm, hurricane, flood, drought, snowstorm...

incident_count_wide <- # pivoted table with incident type as columns
  type_count_per_year %>%
    pivot_wider(names_from = incidentType, values_from = distinct_incidents)
  incident_count_wide$Total <- rowSums(incident_count_wide[,c(2:ncol(incident_count_wide))], na.rm=TRUE)
  incident_count_wide <- incident_count_wide[,c(1,25,2:24)]

# Export to Excel
openxlsx::write.xlsx(freq_by_year, 'datasets/disaster_count_per_year.xlsx', sheetName = 'Disaster Count Per Year', rowNames=FALSE)
openxlsx::write.xlsx(incident_count_wide, 'datasets/disaster_count_by_type_wide.xlsx', sheetName = 'Number Incidents byType pYear', rowNames=FALSE)


# Exploration
ggplot(incident_count_wide, aes(fyDeclared, Fire)) + geom_point()
ggplot(incident_count_wide, aes(fyDeclared, Flood)) + geom_point()
ggplot(incident_count_wide, aes(fyDeclared, incident_count_wide$`Severe Storm`)) + geom_point()
ggplot(incident_count_wide, aes(fyDeclared, Hurricane)) + geom_point()
incident_count_wide[!is.na(incident_count_wide$Fire),]


# Simplified Data / Data of interest (most prevalent disasters):
data_subset <- incident_count_wide[
  incident_count_wide$fyDeclared>=1970 & incident_count_wide$fyDeclared<=2021,
  c("fyDeclared","Fire","Severe Storm","Hurricane","Flood","Drought","Snowstorm")
  ]
data_subset$Total = 
  rowSums(data_subset[,c(2:ncol(data_subset))], 
          na.rm=TRUE)
# Playing with Sonic Pi tool, get numeric array:
# Normalize Total range to 50 range:
min_disasters <- min(data_subset$Total)
max_disasters <- max(data_subset$Total)
normalizing_factor <- (max_disasters-min_disasters)/50
starting_place <- round(50-min_disasters/normalizing_factor,2)
# 50 to 100 sonic neighborhood for sonic pi:
data_subset$sonicpi_numeric = starting_place + round(data_subset$Total/normalizing_factor,2)
write.table(matrix(data_subset$sonicpi_numeric,nrow=1), sep=",",
            row.names=FALSE, col.names=FALSE) # for "play_pattern_timed" in sonic pi
# Useful Sonic Pi docs: https://github.com/sonic-pi-net/sonic-pi/tree/dev/etc/doc/tutorial

# Convert values so that tones will be playable by MIDI piano in Logic Pro X
data_subset$sonicpi_semitones <- round(data_subset$sonicpi_numeric)
# array for Sonic Pi script:
write.table(matrix(data_subset$sonicpi_semitones,nrow=1), sep=",",
            row.names=FALSE, col.names=FALSE) # for "play_pattern_timed" in sonic pi








## OLD code:
## Convert Sonic Pi numeric values into groups so that values can be translating to semitones in Logic Pro X.
# (Problem: in-between tones in logic when converting to MIDI)
# Okay, instead we can do numeric grouping here in R, then use Sonic Pi.
# Transform to check what group Total value falls within. 1 to 4 = group 1, 5 to 8 = group 2, etc...
data_subset$group = transform(data_subset, group = cut(Total, c(seq(min_disasters,max_disasters,4)), labels = FALSE))$group
data_subset$sonicpi_tonegroup = data_subset$group + 50 # bumping into audible range for Sonic Pi
# array for Sonic Pi script:
write.table(matrix(data_subset$sonicpi_tonegroup,nrow=1), sep=",",
            row.names=FALSE, col.names=FALSE) # for "play_pattern_timed" in sonic pi

# Excel output:
openxlsx::write.xlsx(data_subset, 'datasets/disaster_count_by_type_wide_subset.xlsx', sheetName = 'Disaster Data Subset', rowNames=FALSE)



## Extra / Notes

# Would be interesting to also give listener narrated samples of average tones per decade
# or just tonal reference for number of disasters to pitch


# Two Tone Attempt:
twotonedata <- data_subset[c("fyDeclared", "Total")]
openxlsx::write.xlsx(twotonedata, 'datasets/twotonedata.xlsx', sheetName = 'For Two Tone', rowNames=FALSE)
# two tone gives too many repeat tone values, not precise enough! :-(






# Ideas
# map of state colored by how many disasters per state
# what about just fires in california, merged with max temp over time?
# (sonification: tone pitch by temp, and instances of fire)
# data on temperature in usa: https://www.ncei.noaa.gov/cag/statewide/time-series/4/tmin/ann/12/1950-2022?base_prd=true&begbaseyear=1950&endbaseyear=2022

# what about low pitch = fewer instances total, higher pitch = more instances?
# amplitude of sound sample correlated with number of specific disaster instances?
# could have loopable sound that gets increased in amplitude the more it happens, 
# so that way it's more of a continuous growth
