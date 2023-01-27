## Degrees Conferred in the US 1970-2019 ##
## NCES: National Center for Educational Statistics (US D of Education) ##
## January, 2023 | Auralee Walmer ##
  # https://nces.ed.gov/programs/digest/d20/tables/dt20_318.10.asp
  # https://nces.ed.gov/programs/digest/d20/tables/dt20_318.20.asp

# Set Up #
library(readxl)
library(plyr)

setwd("/Volumes/AuraByte2/Data Projects/Degrees Conferred/")

# Import Data #
degree_gender <- read_excel("data import/degree_gender.xls", skip=1)
degree_field <- read_excel("data import/degree_fieldofstudy.xls")

## Clean Data ##

# Degree + Gender
gender_B <- degree_gender[c(3:64),c(1,6,8,10,12)]
colnames(gender_B) <- c("year","bachelors_total","bachelors_males","bachelors_females","bachelors_percent_female")
gender_M <- degree_gender[c(3:64),c(1,13:16)]
colnames(gender_M) <- c("year","masters_total","masters_males","masters_females","masters_percent_female")
gender_D <- degree_gender[c(3:64),c(1,17:20)]
colnames(gender_D) <- c("year","doctorates_total","doctorates_males","doctorates_females","doctorates_percent_female")

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


