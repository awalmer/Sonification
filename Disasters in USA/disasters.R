## Federally Declared Disasters in the USA since 1953 ##
## FEMA: Federal Emergency Management Agency (US D of Homeland Sec) ##
## August, 2022 | Auralee Walmer ##
# LINKS: 
#   [FEMA Data: Disaster Declarations Summary] https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2
#   [FEMA Data FAQ] https://www.fema.gov/about/openfema/faq


# Set up #
library(httr)
library(jsonlite)

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
# Note: this is not the total numner of disasters. There are potentially multiple rows for a given disaster affecting more than one county.

# Ideas
# map of state colored by how many disasters 
