## Degrees Conferred in the US 1970-2019 ##
## NCES: National Center for Educational Statistics (US D of Education) ##
## January, 2023 | Auralee Walmer ##
  # https://nces.ed.gov/programs/digest/d20/tables/dt20_318.10.asp
  # https://nces.ed.gov/programs/digest/d20/tables/dt20_318.20.asp

# Set Up #
library(readxl)
library(plyr)
library(dplyr)
library(resample)
library(ggplot2)

setwd("/Volumes/AuraByte2/Data Projects/Degrees Conferred/")

# Import Data #
degree_gender <- read_excel("data import/degree_gender.xls", skip=1)
degree_field <- read_excel("data import/degree_fieldofstudy.xls")

## Clean Data ##

# Degree + Field
colnames(degree_field) <- as.list(degree_field[2,])
field_B_raw <- degree_field[c(5:17),c(1:12)]
field_B_percent <- degree_field[c(5:17),c(1,13:23)]
field_col_names <- c("year","All Degrees","Humanities","Psychology",
                     "Social Sciences and History","Natural Sciences and Mathematics",
                     "Computer Sciences","Engineering","Education","Business",
                     "Health Professions and Related Programs","Other Fields")
field_col_names_percent <- c("year", paste("percent ",field_col_names[2:12]))
colnames(field_B_raw) <- field_col_names
colnames(field_B_percent) <- field_col_names_percent
bachelors_by_field <- join(field_B_raw, field_B_percent, by = "year", 
                           type = "left", match = "all")
bachelors_by_field[c(2:ncol(bachelors_by_field))] <- 
  sapply(bachelors_by_field[c(2:ncol(bachelors_by_field))], as.numeric)


field_M_raw <- degree_field[c(19:31),c(1:12)]
field_M_percent <- degree_field[c(19:31),c(1,13:23)]
colnames(field_M_raw) <- field_col_names
colnames(field_M_percent) <- field_col_names_percent
masters_by_field <- join(field_M_raw, field_M_percent, by = "year", 
                         type = "left", match = "all")

field_D_raw <- degree_field[c(33:45),c(1:12)]
field_D_percent <- degree_field[c(33:45),c(1,13:23)]
colnames(field_D_raw) <- field_col_names
colnames(field_D_percent) <- field_col_names_percent
doctorates_by_field <- join(field_D_raw, field_D_percent, by = "year", 
                            type = "left", match = "all")

# Degree + Gender
gender_B <- degree_gender[c(3:64),c(1,6,8,10,12)]
colnames(gender_B) <- c("year","bachelors_total","bachelors_males","bachelors_females","bachelors_percent_female")
gender_M <- degree_gender[c(3:64),c(1,13:16)]
colnames(gender_M) <- c("year","masters_total","masters_males","masters_females","masters_percent_female")
gender_D <- degree_gender[c(3:64),c(1,17:20)]
colnames(gender_D) <- c("year","doctorates_total","doctorates_males","doctorates_females","doctorates_percent_female")

# We don't have data over time combining field of study + gender
# But we can look at gender + degree type over time
# It would be so nice to have this over time: (but only have 2018-2019)
  # https://nces.ed.gov/programs/digest/d20/tables/dt20_318.30.asp

gender_degree_merge <- join(gender_B, gender_M, by = "year", type = "left", match = "all")
gender_degree_merge <- join(gender_degree_merge, gender_D, by = "year", type = "left", match = "all")

# Export Data
openxlsx::write.xlsx(gender_degree_merge, 'data export/Degree Type by Gender Over Time.xlsx', sheetName = 'Degrees Conferred by Gender', rowNames=FALSE)
openxlsx::write.xlsx(bachelors_by_field, 'data export/Bachelors by Field Over Time.xlsx', sheetName = 'Bachelors by Field of Study', rowNames=FALSE)
openxlsx::write.xlsx(masters_by_field, 'data export/Masters by Field Over Time.xlsx', sheetName = 'Masters by Field of Study', rowNames=FALSE)
openxlsx::write.xlsx(doctorates_by_field, 'data export/Doctorates by Field Over Time.xlsx', sheetName = 'Doctorates by Field of Study', rowNames=FALSE)


## Data Exploration ##

# Sonification Prep for Degrees Conferred

# Associate percent of degrees with pitch, one field type at a time

piano_note_levels <- c("C2","E2","G2","C3","E3","G3","C4","E4","G4","C5","E5","G5","C6")
max_percent <- max(
  bachelors_by_field[c((ncol(bachelors_by_field)-9):ncol(bachelors_by_field))] %>% 
  summarise_if(is.numeric, max)
) # 23.96178 ~= 24
# Associate 0-24 range with piano note levels
piano_note_factor <- c("C2"=0,"E2"=2,"G2"=4,"C3"=6,"E3"=8,"G3"=10,
                       "C4"=12,"E4"=14,"G4"=16,"C5"=18,"E5"=20,"G5"=22,"C6"=24)
bachelors_by_field$humanities_value_group <- round(bachelors_by_field$`percent  Humanities`/2)*2
bachelors_by_field$humanities_pitch <- 
  mapvalues(
  x=bachelors_by_field$humanities_value_group, 
  from=seq(0,24,2), 
  to=names(piano_note_factor), 
  warn_missing = TRUE
  )
# Create Functions:
convert_percent_to_group <- function(percent_column) {
  round(percent_column/2)*2
}
convert_group_to_pitch <- function(group_column, note_levels) {
  mapvalues(
    x=group_column, 
    from=seq(0,24,2), 
    to=names(note_levels), 
    warn_missing = FALSE
  )
}
# Apply:
bachelors_by_field$psychology_value_group <- 
  convert_percent_to_group(bachelors_by_field$`percent  Psychology`)
bachelors_by_field$psychology_pitch <- 
  convert_group_to_pitch(
    bachelors_by_field$psychology_value_group, piano_note_factor
    )

bachelors_by_field$socialscience_history_value_group <- 
  convert_percent_to_group(bachelors_by_field$`percent  Social Sciences and History`)
bachelors_by_field$socialscience_history_pitch <- 
  convert_group_to_pitch(
    bachelors_by_field$socialscience_history_value_group, piano_note_factor
  )

bachelors_by_field$natscience_math_value_group <- 
  convert_percent_to_group(bachelors_by_field$`percent  Natural Sciences and Mathematics`)
bachelors_by_field$natscience_math_pitch <- 
  convert_group_to_pitch(
    bachelors_by_field$natscience_math_value_group, piano_note_factor
  )

bachelors_by_field$compsci_value_group <- 
  convert_percent_to_group(bachelors_by_field$`percent  Computer Sciences`)
bachelors_by_field$compsci_pitch <- 
  convert_group_to_pitch(
    bachelors_by_field$compsci_value_group, piano_note_factor
  )

bachelors_by_field$engineering_value_group <- 
  convert_percent_to_group(bachelors_by_field$`percent  Engineering`)
bachelors_by_field$engineering_pitch <- 
  convert_group_to_pitch(
    bachelors_by_field$engineering_value_group, piano_note_factor
  )

bachelors_by_field$education_value_group <- 
  convert_percent_to_group(bachelors_by_field$`percent  Education`)
bachelors_by_field$education_pitch <- 
  convert_group_to_pitch(
    bachelors_by_field$education_value_group, piano_note_factor
  )

bachelors_by_field$business_value_group <- 
  convert_percent_to_group(bachelors_by_field$`percent  Business`)
bachelors_by_field$business_pitch <- 
  convert_group_to_pitch(
    bachelors_by_field$business_value_group, piano_note_factor
  )

bachelors_by_field$health_value_group <- 
  convert_percent_to_group(bachelors_by_field$`percent  Health Professions and Related Programs`)
bachelors_by_field$health_pitch <- 
  convert_group_to_pitch(
    bachelors_by_field$health_value_group, piano_note_factor
  )

bachelors_by_field$other_value_group <- 
  convert_percent_to_group(bachelors_by_field$`percent  Other Fields`)
bachelors_by_field$other_pitch <- 
  convert_group_to_pitch(
    bachelors_by_field$other_value_group, piano_note_factor
  )

## Subset of interest for Logic Pro X

piano_note_subset <- data.frame(
  bachelors_by_field$year,
  bachelors_by_field %>% select(ends_with("_pitch"))
  )

## Recaps:
# Most variation
max(colVars(bachelors_by_field[c(14:23)])) # Education
# Least Variation
min(colVars(bachelors_by_field[c(14:23)])) # Psychology
# Biggest Change Over Time
cl <- c()
for (x in 14:23) {
  n <- bachelors_by_field[1,x]-bachelors_by_field[13,x]
  cl[length(cl)+1] <- n
}
max(cl) # Education
# Smallest Change Over Time - Natural Science and Math

# Overall Lowest Levels
ol <- c()
for (x in 14:23) {
  n <- sum(bachelors_by_field[1:13,x])
  ol[length(ol)+1] <- n
}
ol
min(ol) # Computer Science
# Overall Highest Levels
max(ol) # Business

# Biggest Decrease: Education
# Biggest Increase: Other

## after all separate scales played, narratively point out the fields 
# with biggest changes over time and play those notes in comparison

# Then play highest rates (as whole chord) e.g. all years of Education together
# and all years of comp sci - consistently lowest rates?

#--------------------

## Continued work March 2023 ##

## Table with 1970-71 to 2018-19 percentages:
percent_compare_table <- data.frame(bachelors_by_field$year, 
                                    bachelors_by_field %>% select(starts_with("percent"))
)
colnames(percent_compare_table) <- field_col_names_percent
## Table with only 1971 versus 2019:
pct <- percent_compare_table[c(1,nrow(percent_compare_table)),]

## Line graph output:
ggplot(data=percent_compare_table, aes(x=percent_compare_table$year,
                                       y=percent_compare_table$`percent  Humanities`,
                                       group=1)) +
  geom_path()+
  labs(title="1970-2019",x="Year", y = "Percent of Degrees Conferred")+
  scale_y_continuous(limits = c(0,25)) 

# Function:
makelinegraph <- function(column, title) {
  p <- ggplot(data=percent_compare_table, aes(x=`year`,
                                              y=column,
                                              group=1)) +
    geom_path()+
    labs(title=title, x="Year", y = "Percent of Degrees Conferred")+
    scale_y_continuous(limits = c(0,24)) 
  return(p)
}

colnames(percent_compare_table)
makelinegraph(percent_compare_table$`percent  Humanities`, "Humanities")
makelinegraph(percent_compare_table$`percent  Psychology`, "Psychology")
makelinegraph(percent_compare_table$`percent  Social Sciences and History`, "Social Sciences and History")
makelinegraph(percent_compare_table$`percent  Natural Sciences and Mathematics`, "Natural Sciences and Mathematics")
makelinegraph(percent_compare_table$`percent  Computer Sciences`, "Computer Sciences")
makelinegraph(percent_compare_table$`percent  Engineering`, "Engineering")
makelinegraph(percent_compare_table$`percent  Education`, "Education")
makelinegraph(percent_compare_table$`percent  Health Professions and Related Programs`, "Health Professions and Related Programs")
makelinegraph(percent_compare_table$`percent  Other Fields`, "Other Fields")

