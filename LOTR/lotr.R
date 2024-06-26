## Lord of the Rings Data | Sonification ##
## July, 2022 | Auralee Walmer ##
# LINKS: 
#   [Kaggle Data] https://www.kaggle.com/datasets/paultimothymooney/lord-of-the-rings-data
#   [Scrape Guide]  https://rvest.tidyverse.org/articles/rvest.html
#   [LOTR FOTR Movie Script]  https://imsdb.com/scripts/Lord-of-the-Rings-Fellowship-of-the-Ring,-The.html
#   [Character Classification Data] https://juandes.github.io/lotr-names-classification/

# Set up #
library(tidyverse)
library(rvest)
library(stringi)
library(data.table)

setwd("/Volumes/AuraByte/Data Projects/LOTR/")

# External Data Sources #
lotr_scripts <- read_csv("Data Input/lotr_scripts.csv")[,2:4]
lotr_characters <- read_csv("Data Input/lotr_characters.csv")
lotr_char_race <- read_csv("Data Input/characters_no_surnames.csv")

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
#scrape_characters$name_clean <- gsub( '/(\r\n)+|\r+|\n+/i', '', scrape_characters$name)
scrape_characters$name_clean <- gsub("[\r\n]", "", scrape_characters$name)
# (references: https://kaspars.net/blog/regex-remove-all-line-breaks-from-text, 
# https://stackoverflow.com/questions/21781014/remove-all-line-breaks-enter-symbols-from-the-string-using-r)
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
lotr_char_race[nrow(lotr_char_race)+1,] <- list("Sam","Hobbit","SAM") # Samwise but Sam in script


## JOIN DATA ##
lotr_join <- left_join(scrape_characters[c("character_lines_in_order","name_clean")], 
                       lotr_char_race[c("race","name_clean")],
                       by = "name_clean") # match looking more accurate

# Drop non-matches:
lotr_join <- lotr_join[!is.na(lotr_join$race),c("name_clean","race")]

# Further data check & clean:
unique(lotr_join[c("name_clean","race")]) # Frodo typo; contradicting race for Haldir
# Fix "ODO" typo --> "FRODO"
lotr_join[which(lotr_join$name_clean=="ODO"),"name_clean"] <- "FRODO"
# Fix Haldir --> Elf
lotr_join[which(lotr_join$name_clean=="HALDIR" & lotr_join$race=="Man"),"race"] <- "Elf"
# Change Strider --> Aragorn
lotr_join[which(lotr_join$name_clean=="STRIDER"),"name_clean"] <- "ARAGORN"

# Additional modifications for data sonification purposes:
lotr_join$order <- 1:nrow(lotr_join)
# Numericize race (for Two Tone):
lotr_join$race_num <- as.factor(lotr_join$race)
levels(lotr_join$race_num) <- list("Ainur"=1,"Elf"=2,"Dwarf"=3,"Man"=4,"Hobbit"=5) # order of creation
lotr_join$race_num <- as.numeric(lotr_join$race_num)
# Character ID:
char_df <- data.frame(unique(lotr_join["name_clean"]))
char_df$character_id <- 1:nrow(char_df) # order of appearance
lotr_join <- left_join(lotr_join, char_df, by = "name_clean")

# Write output:
write_csv(lotr_join, "Data Output/lotr_script_data.csv")


## Dataframes: Script Summaries
lotr_script_summary1 <- data.frame(character = lotr_join$name_clean, race = lotr_join$race, 
                                   character_id = lotr_join$character_id, number_of_lines=0)
lotr_script_summary1 <- lotr_script_summary1[!duplicated(lotr_script_summary1),]
for (n in 1:nrow(lotr_script_summary1)) {
  lotr_script_summary1$number_of_lines[n] <- length(
    which(
      lotr_join$name_clean==lotr_script_summary1$character[n]
      )
    )
}

lotr_script_summary2 <- data.frame(race = lotr_join$race, number_of_lines=0)
lotr_script_summary2 <- lotr_script_summary2[!duplicated(lotr_script_summary2),]
for (n in 1:nrow(lotr_script_summary2)) {
  lotr_script_summary2$number_of_lines[n] <- length(
    which(
      lotr_join$race==lotr_script_summary2$race[n]
    )
  )
}

write_csv(lotr_script_summary1, "Data Output/lotr_lines_per_character.csv")
write_csv(lotr_script_summary2, "Data Output/lotr_lines_per_race.csv")


## Goal: tranfer data into Logic Pro X and implement Philharmonia sound samples
lotr_logicprox <- transform(lotr_join, counter = ave(name_clean, rleid(name_clean), FUN = seq_along))
lotr_logicprox$group_id <- rleid(lotr_logicprox$name_clean)
lotr_logicprox <- lotr_logicprox%>%group_by(group_id)%>%slice(which.max(counter))
lotr_logicprox$counter <- as.numeric(lotr_logicprox$counter)
lotr_logicprox$musical_measure <- lotr_logicprox$counter/4
lotr_logicprox$line_start <- 0 # Measure placemarkers for MIDI regions in Logic Pro X
lotr_logicprox$line_end <- 0
lotr_logicprox[1,"line_start"] <- 6
lotr_logicprox[1,"line_end"] <- 6+lotr_logicprox[1,"musical_measure"]
for (m in 2:nrow(lotr_logicprox)) {
  lotr_logicprox$line_start[m] <- lotr_logicprox$line_end[m-1]
  lotr_logicprox$line_end[m] <- lotr_logicprox$line_start[m]+lotr_logicprox$musical_measure[m]
}

# Write output:
write_csv(lotr_logicprox, "Data Output/lotr_logicprox.csv")


## DATA VISUALIZATIONS ##
library(plotly)
library(ggplot2)
library(treemapify)

lotr_colors <- c(Hobbit="#c29a11",Elf="#1c724d",Ainur="#5b31a1",Man="#25719c",Dwarf="#995f19")
race_order <- c("Hobbit","Ainur","Elf","Man","Dwarf")

legend_aes <- list(
  font = list(
    family = "sans-serif",
    size = 12,
    color = "#000"),
  bgcolor = "#E2E2E2",
  bordercolor = "#FFFFFF",
  borderwidth = 2)

barchart_ethnicgroup <- lotr_script_summary2 %>% 
  plot_ly(x = ~race, y = ~number_of_lines, 
          type="bar", color = ~race, colors=lotr_colors, text=~number_of_lines,textposition = 'auto') %>%
  layout(paper_bgcolor='#494949',
         plot_bgcolor='#292929',
         xaxis = list(title = "Group",
                      categoryorder = "array",
                      categoryarray = ~race_order,
                      color = '#ffffff'),
         yaxis = list(title = "Lines in Script",
           color = '#ffffff'),
         title="title",
         legend = legend_aes
         ) %>% 
  add_text(x = ~race, y = ~number_of_lines,text=~number_of_lines,textposition="top")
barchart_ethnicgroup


apple_pie <- lotr_script_summary2 %>%
              plot_ly(type='pie', labels=lotr_script_summary2$race, values=lotr_script_summary2$number_of_lines, 
                     textinfo='label+percent',
                     marker = list(colors = lotr_colors)
                     )
apple_pie

hobbits <- lotr_script_summary1[which(lotr_script_summary1$race=="Hobbit"),]
ainur <- lotr_script_summary1[which(lotr_script_summary1$race=="Ainur"),]
elves <- lotr_script_summary1[which(lotr_script_summary1$race=="Elf"),]
men <- lotr_script_summary1[which(lotr_script_summary1$race=="Man"),]
dwarves <- lotr_script_summary1[which(lotr_script_summary1$race=="Dwarf"),]

hobbit_pie <- hobbits %>%
  plot_ly(type='pie', labels=hobbits$character, 
          values=hobbits$number_of_lines, 
          textinfo='label+percent+value',
          marker = list(colors = lotr_colors)
  )
hobbit_pie
ainur_pie <- ainur %>%
  plot_ly(type='pie', labels=ainur$character, 
          values=ainur$number_of_lines, 
          textinfo='label+percent+value',
          marker = list(colors = lotr_colors)
  )
ainur_pie
elf_pie <- elves %>%
  plot_ly(type='pie', labels=elves$character, 
          values=elves$number_of_lines, 
          textinfo='label+percent+value',
          marker = list(colors = lotr_colors)
  )
elf_pie
men_pie <- men %>%
  plot_ly(type='pie', labels=men$character, 
          values=men$number_of_lines, 
          textinfo='label+percent+value',
          marker = list(colors = lotr_colors)
  )
men_pie
dwarf_pie <- dwarves %>%
  plot_ly(type='pie', labels=dwarves$character, 
          values=dwarves$number_of_lines, 
          textinfo='label+percent+value',
          marker = list(colors = lotr_colors)
  )
dwarf_pie

ggplot(lotr_script_summary1, aes(area = number_of_lines, fill = race, label = character)) +
  geom_treemap() +
  geom_treemap_text()

ggplot(lotr_script_summary1, aes(x = race, y = number_of_lines, fill = character)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = race_order)

