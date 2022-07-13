## Lord of the Rings Data ##
## July 11, 2022 ##
# LINKS: 
#   [Kaggle Data] https://www.kaggle.com/datasets/paultimothymooney/lord-of-the-rings-data
#   [Scrape Guide]  https://rvest.tidyverse.org/articles/rvest.html
#   [LOTR FOTR Movie Script]  https://imsdb.com/scripts/Lord-of-the-Rings-Fellowship-of-the-Ring,-The.html
#   [Character Classification Data] https://juandes.github.io/lotr-names-classification/

# Set up #
library(tidyverse)
library(rvest)
library(stringi)

setwd("/Volumes/AuraByte/Data Projects/LOTR/")

lotr_scripts <- read_csv("Data/lotr_scripts.csv")[,2:4]
lotr_characters <- read_csv("Data/lotr_characters.csv")
lotr_char_race <- read_csv("Data/characters_no_surnames.csv")

# Data Cleaning #
name_check <- 
  lotr_scripts %>%
  group_by(char) %>%
  summarise(n_distinct(char))
# typos in names need to be corrected and duplicates need combining
# ALERT! lotr_characters data doesn't contain Eowyn, for example! Missing data.
# Also, data set not in order of lines spoken.

## ALTERNATE APPROACH ##
# Scrape Script from LOTR-FOTR
scrape <- read_html("https://imsdb.com/scripts/Lord-of-the-Rings-Fellowship-of-the-Ring,-The.html")

scrape %>%
  html_elements("b")

scrape_char_line <- 
  data.frame(
  character_lines_in_order = scrape %>% html_elements("b") %>% html_text2()
)
# This gives a data frame of the FIRST body element throughout the script (within each <b></b>),
# which contains the name of the character saying the line (because a spoken line will always be preceded by character name).

# If I merge this with a comprehensive list of characters from LOTR and keep only matches,
# I should be able to have a race mapping of character in the order of lines spoken.

# GOAL: Sonification of lines spoken by race
# different race = different sonic tones

# Data Cleaning #
# scrape_char_line$char_first_name <- gsub( " .*$", "", scrape_char_line$character_lines_in_order) # gets rid of e.g. "(V.O.)" etc.
scrape_char_line$char_first_name <- ""
for (row in 1:nrow(scrape_char_line)) {
  scrape_char_line$char_first_name[row] <- strsplit(scrape_char_line$character_lines_in_order[row], "[ ]")[[1]][1]
} # just in case two spaces anywhere
lotr_char_race$name_capitalized <- toupper(lotr_char_race$name) # upper case for matching
lotr_char_race$name_capitalized_nopunct = stri_trans_general(str = lotr_char_race$name_capitalized, id = "Latin-ASCII") # remove accents etc.
lotr_char_race <- lotr_char_race[!duplicated(lotr_char_race), ] # 827 to 737 rows, removing duplicates
# lotr_char_race does not contain GOLLUM!
lotr_char_race[738,] <- list("Gollum","Hobbit","GOLLUM","GOLLUM")
lotr_char_race[739,] <- list("Strider","Man","STRIDER","STRIDER")
lotr_char_race[740,] <- list("Mrs. Sackville Baggins","Hobbit","MRS. SACKVILLE BAGGINS","MRS. SACKVILLE BAGGINS")

unique(scrape_char_line$char_first_name)

## note! weird characters! "\r" -->
scrape_char_line$char_first_name[23]
# CREATING A PROBLEM WITH THE JOIN!
scrape_char_line$char_first_name_cleanup <- gsub( '/(\r\n)+|\r+|\n+/i', '', scrape_char_line$char_first_name)
# Removed carriage return
# (thank you https://kaspars.net/blog/regex-remove-all-line-breaks-from-text)
#scrape_char_line$char_first_name_cleanup <- gsub( ".", "", scrape_char_line$char_first_name_cleanup)
unique(scrape_char_line$char_first_name_cleanup)
# Add Mrs. Sackville Baggins (one line!)
scrape_char_line$char_first_name_cleanup[which(scrape_char_line$char_first_name_cleanup=="MRS.")] <- "MRS. SACKVILLE BAGGINS"

# Join Datasets #
lotr_join <- left_join(scrape_char_line, lotr_char_race, by = c("char_first_name_cleanup" = "name_capitalized_nopunct"))
# okay, now it's looking like a more accurate match.

# Drop non-matches:
lotr_join <- lotr_join[!is.na(lotr_join$race),c("char_first_name_cleanup","race")]
colnames(lotr_join) <- c("lotr_character_line_in_order","lotr_race")

# Write output:
write_csv(lotr_join, "Output/lotr_fotr_character_line_order.csv")

# New data set for sonification:
# Number by race
lotr_line_order_race <- data.frame(race = lotr_join$lotr_race, order = 1:nrow(lotr_join))
# Write output:
write_csv(lotr_line_order_race, "Output/lotr_line_order_race.csv")
write_csv(data.frame(lotr_line_order_race$race), "Output/lotr_line_order_race_onlyrace.csv")

## Looks like with Two Tone, you have to numericize the data of interest?
lotr_line_order_race_numericized 

