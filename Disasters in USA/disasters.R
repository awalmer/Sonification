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

setwd("/Volumes/AuraByte/Data Projects/Disasters in USA/")

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



# Ideas
# map of state colored by how many disasters per state
