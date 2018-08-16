###   120 year Olympics Data (EDA)

### Variable Info:

# ID - Unique number for each athlete
# Name - Athlete's name
# Sex - M or F
# Age - Integer
# Height - In centimeters
# Weight - In kilograms
# Team - Team name
# NOC - National Olympic Committee 3-letter code
# Games - Year and season
# Year - Integer
# Season - Summer or Winter
# City - Host city
# Sport - Sport
# Event - Event
# Medal - Gold, Silver, Bronze, or NA

##########################
## Loading Data Sets. ####
##########################

athletes <- read.csv("athlete_events.csv",stringsAsFactors = FALSE)
regions <- read.csv("noc_regions.csv",stringsAsFactors = FALSE)

## We have 271116 Rows (records.) and 15 attributes.

################################################
######## Loading Required library units. #######
################################################

library(dplyr)
library(ggplot2)
library(tidyverse)
library(rvest)

library(magrittr)
library(ggmap)


##############################################################################

str(athletes)
str(regions)

########################
## Analysis By Sex #####
########################

sex_df <- athletes %>% 
            group_by(Season,Sex) %>% 
            summarise(Count = n()) %>%
            mutate(Percenatge = round(Count*100/sum(Count),2))


sex_df %>% ggplot(aes(x=Season,y=Percenatge,fill=Sex)) +
              geom_bar(stat = "identity",position = position_dodge()) +
              ggtitle("Participation Distributed by Sex") + 
              # geom_label(label=sex_df$Percenatge,position = position_dodge(0.9))+
              theme_minimal()

#########################################
###  Year vs NUmber of Season #####
#########################################

year_no <- athletes %>%
              group_by(Year,Season) %>%
              summarise(Count = n())

# year_no

year_no %>% ggplot(aes(x=Year,y=Count)) +
                geom_line(aes(color=Season)) +
                geom_point(aes(color=Season)) + 
                labs(x="Year",y="NUmber Of Participants",title="Number of Participants Over Time")

#########################################
### Number of participants vs Sex ratio #
#########################################

M_year <- athletes %>%
            filter(Sex == "M") %>%
            group_by(Year,Season) %>%
            summarise(Men_Partn = n())

F_year <- athletes %>%
            filter(Sex == "F") %>%
            group_by(Year,Season) %>%
            summarise(Female_Partn = n())

sex_ratio <- M_year %>% left_join(F_year) %>% mutate(Ratio = Men_Partn/Female_Partn)


