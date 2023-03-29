## Decibels Sonification Discord Group March Challenge ##
## Personal Data: My health data from iPhone ##
## March, 2023 | Auralee Walmer ##

# Set Up #
library(xml2)
library(tidyverse)
library(tibble)
library(XML)


setwd("/Volumes/AuraByte2/Data Projects/Decibels March Challenge | Personal Data/")

# Import Data #
xml_file <- "apple_health_export/export.xml"
xml_list <- as_list(read_xml("apple_health_export/export.xml"))
xml_df = tibble::as_tibble(xml_list) %>%
  unnest_longer(ActivitySummary)
rec_wider = xml_df %>%
  #dplyr::filter(HealthData_id == "MetadataEntry") %>%
  unnest_wider(HealthData)

xml_file <- read_xml("apple_health_export/export.xml")
str(xml_file)
xml_name(xml_file) # "HealthData"
xml_structure(xml_file)
xml_text(xml_file)
xml_find_all(xml_file, xpath = "//Record")
record <- list(xml_find_all(xml_file, xpath = "//Record"))

class(record) # list
record[[1]][[1]]
record[[1]][[2]]
record[[1]][[1]]$node
class(record[[1]][[1]]) # xml node
as.character(record[[1]][[1]])

hd <- xml_find_all(xml_file, xpath = "//HealthData")
hd[[1]]
xml_find_all(hd, xpath = "//Record")
xml_find_all(hd, xpath = "//Record")[1][[1]]
# Capture all records:
records <- xml_find_all(hd, xpath = "//Record")
# Capture all attributes:
type <- xml_attr(records, "type")
sourceName <- xml_attr(records, "sourceName")
sourceVersion <- xml_attr(records, "sourceVersion")
device <- xml_attr(records, "device")
unit <- xml_attr(records, "unit")
creationDate <- xml_attr(records, "creationDate")
startDate <- xml_attr(records, "startDate")
endDate <- xml_attr(records, "endDate")
value <- xml_attr(records, "value")
  
df <- tibble(type = type, sourceName = sourceName, 
             sourceVersion = sourceVersion, device = device, unit = unit,
             creationDate = creationDate, startDate = startDate,
             endDate = endDate, value = value)
# Looks like I will need to sum by day for total daily step count!
# format dates as well and filter to data of interest
