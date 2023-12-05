## Wikipedia Sonification ##
## Auralee Walmer ##
# Refactor Dec 2023 #

# Set Up #
setwd("/Volumes/AuraByte3/Data Projects/Jordan Collaboration/Wikipedia Sonification/")
library(dplyr)
library(tabr)
library(plyr)
library(data.table)

#-------------#
# Import Data #
#-------------#

# Wiki Revision History of Sonification Page
# Edited Python Script
wiki_raw <- read.csv("wiki_sonification_revisions.csv")

#-----------------#
# Data Wrangling:
#-----------------#

wiki <- wiki_raw[c("timestamp","diff","age","date","user","userid","size")]
colnames(wiki) <- c("timestamp","edit_size","age_days","date","user",
                    "userid","page_length")
wiki$age <- round(wiki$age, digits=3) # age is in days
wiki$year <- substr(wiki$date,1,4)
wiki$day <- floor(wiki$age)
wiki$day <- ifelse(wiki$day==0, 1, wiki$day)
wiki$edit_size[1] <- 0

# group:
## OKAY FOR SOME REASON MY SAME EXACT CODE FROM BEFORE IS NO LONGER WORKING UGH
#wiki_daily <- 
#  wiki %>% group_by(date, user, userid) %>% 
#  summarise(edit_size=sum(edit_size, na.rm=TRUE),
#            age=last(age),
#            timestamp=last(timestamp),
#            page_length=last(page_length),
#            day=last(day)
#            )

## TEST
test <- data.table(wiki) ## but data.table is working
test[,list(sumedit = sum(edit_size), freq = .N, pagelength=max(page_length)), by = c("date", "userid", "user")]

test[,list(edit_size = sum(edit_size), 
           age=last(age),
           timestamp=last(timestamp),
           page_length=last(page_length),
           day=last(day)
           ), 
     by = c("date", "userid", "user")]

## APPLY data table approach:
wiki_temp <- data.table(wiki)

## DAILY AGGREGATION:
wiki_daily <- 
  wiki_temp[,list(edit_size = sum(edit_size), 
             age=last(age),
             timestamp=last(timestamp),
             page_length=last(page_length),
             day=last(day)
  ), 
  by = c("date", "userid", "user")]


## Additional Vars:
wiki_daily$start_seconds <- round(wiki_daily$age/30, digits=3)
wiki_daily$time_between <- wiki_daily$start_seconds
for (x in 2:nrow(wiki_daily)) {
  wiki_daily$time_between[x] <- wiki_daily$start_seconds[x]-wiki_daily$start_seconds[x-1]
}
wiki_daily$release <- round(log(abs(wiki_daily$edit_size)), digits=3) # natural log
for (x in 1:nrow(wiki_daily)) {
  if (wiki_daily$edit_size[x]==0) {
    wiki_daily$release[x] <- 0
  } else if (wiki_daily$edit_size[x]==1 | wiki_daily$edit_size[x]==-1) {
    wiki_daily$release[x] <- 0.5
  }
}

write.csv(wiki_daily, "wiki_daily.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")


## WEEKLY AGGREGATION:
day2weekmap <- data.frame("day"=c(1:7028),"week"=rep(c(1:1004),each=7))
# 1004 weeks = 7028 days
wiki_temp2 <- join(wiki_temp, day2weekmap, by = "day", type = "left")

wiki_weekly <- 
  wiki_temp2[,list(edit_size = sum(edit_size), 
                  age=last(age),
                  timestamp=last(timestamp),
                  page_length=last(page_length)
  ), 
  by = c("week")]

## Additional Vars:
wiki_weekly$start_seconds <- round(wiki_weekly$age/30, digits=3)
wiki_weekly$time_between <- wiki_weekly$start_seconds
for (x in 2:nrow(wiki_weekly)) {
  wiki_weekly$time_between[x] <- wiki_weekly$start_seconds[x]-wiki_weekly$start_seconds[x-1]
}
wiki_weekly$release <- round(log(abs(wiki_weekly$edit_size)), digits=3) # natural log
for (x in 1:nrow(wiki_weekly)) {
  if (wiki_weekly$edit_size[x]==0) {
    wiki_weekly$release[x] <- 0
  } else if (wiki_weekly$edit_size[x]==1 | wiki_weekly$edit_size[x]==-1) {
    wiki_weekly$release[x] <- 0.5
  }
}

write.csv(wiki_weekly, "wiki_weekly.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")


## YEARLY PAGE TURN
pageturn <- data.frame("jan1_seconds"=seq(0, 228, by=12))

write.csv(pageturn, "yearly_pageturn.csv", 
          row.names=FALSE, fileEncoding="UTF-8", na="")



#### ----- SONIC PI DATA FRAME ----- ##

# save an example scale as list:
A_min_pentatonic_2.8 <- c(57.0, 60.0, 62.0, 64.0, 67.0, 69.0, 72.0, 74.0, 76.0, 79.0, 81.0)


## Sonic Pi Data Frame Prep
sonicpi <- data.frame("edit_size"=wiki_daily$edit_size,
                      "date"=wiki_daily$date,
                      "note"="",
                      "release"=round(wiki_daily$release, digits=4),
                      "time_between"=wiki_daily$time_between,
                      "start_seconds"=wiki_daily$start_seconds,
                      "user"=wiki_daily$user
                      )
for (i in 1:nrow(sonicpi)) {
  sonicpi$note[i] <- sample(A_min_pentatonic_2.8, size=1)
}

write.csv(sonicpi, "sonicpi.csv", row.names = FALSE, fileEncoding="UTF-8", na="")
                      
  


  
  
  
  