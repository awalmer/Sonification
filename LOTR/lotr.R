## Lord of the Rings Data | Sonification ##
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

# External Data Sources #
lotr_scripts <- read_csv("Data/lotr_scripts.csv")[,2:4]
lotr_characters <- read_csv("Data/lotr_characters.csv")
lotr_char_race <- read_csv("Data/characters_no_surnames.csv")

# Data Check #
name_check <- 
  lotr_scripts %>%
  group_by(char) %>%
  summarise(n_distinct(char))
# Some name text contain typos; duplicates exist; characters missing (e.g. Eowyn)
# Rows are not in order of lines spoken in script

## ALTERNATE APPROACH ##
# Scrape Script from The Fellowship of the Ring:
scrape <- read_html("https://imsdb.com/scripts/Lord-of-the-Rings-Fellowship-of-the-Ring,-The.html")

scrape_characters <- 
  data.frame(
    character_lines_in_order = 
      scrape %>%
      html_elements("b") %>%
      html_text2()
)
# This gives a data frame of the FIRST body element throughout the script (within each <b></b>),
# which contains the name of the character saying the line (because a spoken line will always be preceded by character name).

# If I merge this with a comprehensive list of characters from LOTR and keep only matches,
# I should be able to have a race mapping of character in the order of lines spoken.

# GOAL: Sonification of lines spoken, pitch by race
# (different race = different sonic tones)


## DATA CLEANING ##
# 1. FOTR Script Data:
scrape_characters$name <- ""
for (row in 1:nrow(scrape_characters)) {
  scrape_characters$name[row] <- strsplit(scrape_characters$character_lines_in_order[row], "[ ]")[[1]][1]
} # Capturing first value before space (name where applicable)
# Data check:
unique(scrape_characters$name) # values contain "\r" (carriage return)
# Remove carriage returns:
scrape_characters$name_clean <- gsub( '/(\r\n)+|\r+|\n+/i', '', scrape_characters$name)
# Removed carriage return (thank you https://kaspars.net/blog/regex-remove-all-line-breaks-from-text)
unique(scrape_characters$name_clean)
# Add Mrs. Sackville Baggins (one line!)
scrape_characters$name_clean[which(scrape_characters$name_clean=="MRS.")] <- "MRS. SACKVILLE BAGGINS"

# 2. Comprehensive Character Data:
lotr_char_race$name_clean <- toupper(lotr_char_race$name) # upper case for matching
lotr_char_race$name_clean = stri_trans_general(str = lotr_char_race$name_clean, id = "Latin-ASCII") # remove accents etc.
lotr_char_race <- lotr_char_race[!duplicated(lotr_char_race), ] # 827 to 737 rows, removing duplicates
# Fill in missing characters:
lotr_char_race[nrow(lotr_char_race)+1,] <- list("Gollum","Hobbit","GOLLUM")
lotr_char_race[nrow(lotr_char_race)+1,] <- list("Strider","Man","STRIDER")
lotr_char_race[nrow(lotr_char_race)+1,] <- list("Mrs. Sackville Baggins","Hobbit","MRS. SACKVILLE BAGGINS")


## JOIN DATA ##
lotr_join <- left_join(scrape_characters[c("character_lines_in_order","name_clean")], 
                       lotr_char_race[c("race","name_clean")],
                       by = "name_clean") # match looking more accurate

# Drop non-matches:
lotr_join <- lotr_join[!is.na(lotr_join$race),c("name_clean","race")]


# Additional modifications for data sonification purposes:
lotr_join$order <- 1:nrow(lotr_join)
# Numericize race (for Two Tone):
lotr_join$race_num <- as.factor(lotr_join$race)
levels(lotr_join$race_num) <- list("Ainur"=2,"Elf"=4,"Man"=6,"Dwarf"=8,"Hobbit"=10)
lotr_join$race_num <- as.numeric(lotr_join$race_num)*2
# Character ID:
lotr_join$character_id <- as.numeric(factor(lotr_join$name_clean))

# Write output:
write_csv(lotr_join, "Output/lotr_fotr_character_line_order.csv")


## How about a data frame of number of lines per character?
lotr_script_summary <- data.frame(character = lotr_join$name_clean, race = lotr_join$race, number_of_lines=0)
lotr_script_summary <- lotr_script_summary[!duplicated(lotr_script_summary),]
for (n in 1:nrow(lotr_script_summary)) {
  lotr_script_summary$number_of_lines[n] <- length(
    which(
      lotr_join$name_clean==lotr_script_summary$character[n]
      )
    )
}
  
write_csv(lotr_script_summary, "Output/lotr_lines_per_character.csv")


### NOTE: Need to clean up Haldir's elf vs man classification



