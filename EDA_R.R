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

sex_ratio$Ratio[is.na(sex_ratio$Ratio)] <- 175

plot_M <- sex_ratio %>%
          ggplot(aes(x=Year,y=Ratio,fill=Season))+
          geom_line(aes(color=Season))+
          geom_point(aes(color=Season))+
          labs(x="Year",y="Ratio",title="Sex Ratio over time")

plot_M  ## Seems Male participation was high earlier.

P1 <- M_year %>% ggplot(aes(x=Year,y=Men_Partn,color=Season)) + 
            geom_line(aes(color=Season),position = position_dodge(.9))+
            geom_point(aes(color=Season))+
            labs(x="Year",y="# Participation",title="Number of MALE Participation over Time")

P2 <- F_year %>% ggplot(aes(x=Year,y=Female_Partn,color=Season)) + 
  geom_line(aes(color=Season),position = position_dodge(.9))+
  geom_point(aes(color=Season))+
  labs(x="Year",y="# Participation",title="Number of FEMALE Participation over Time")

## P1 and P2 showing in one plot.
library(cowplot)

plot_grid(P1,P2,ncol = 1)

############################
## Distribution By age #####
############################

sum(is.na(athletes$Age))

# Age Total
d_age <- athletes %>%
  group_by(Age) %>%
  summarise(Num_age = n())
  
apply(d_age, 2,function(x) sum(is.na(x)))
d_age <- na.omit(d_age)

d_age %>%
    ggplot(aes(x=Age,y=Num_age))+
    geom_line()+
    labs(x="Age",y="Frequency",title="Age Distribution Since 1890")


# Age Male
dm_age <- athletes %>%
  filter(Sex=="M")%>%
  group_by(Age) %>%
  summarise(Num_age = n())

dm_age <- na.omit(dm_age)

a_p1 <- dm_age %>%
  ggplot(aes(x=Age,y=Num_age))+
  geom_line()+
labs(x="Male Age",y="Frequency",title="Age Distribution Since 1890 (Male)")
    
# Age FeMale
df_age <- athletes %>%
  filter(Sex=="F")%>%
  group_by(Age) %>%
  summarise(Num_age = n())

df_age <- na.omit(df_age)

a_p2 <- df_age %>%
  ggplot(aes(x=Age,y=Num_age))+
  geom_line()+
labs(x="Female Age",y="Frequency",title="Age Distribution Since 1890 (Female)")


plot_grid(a_p1,a_p2,ncol=1)

### Average Age in Each Year

a_age <- athletes %>%
  group_by(Year) %>%
  summarise(Ave_age = mean(Age,na.rm = TRUE))

# a_age <- na.omit(a_age)

a_age %>%
  ggplot(aes(x=Year,y=Ave_age))+
  geom_line()+
  geom_point()+
  labs(x="Year",y="Average Age",title="Year and Avearge Age since 1890")

#########  Analysing By Team.

Tm <- athletes %>%
  group_by(Year,Season) %>%
  summarise(Num_Team = length(unique(Team)))

Tm %>%
  ggplot(aes(x=Year,y=Num_Team,group=Season))+
  geom_line(aes(color=Season))+
  geom_point(aes(color=Season))+
  labs(x="Year",y="Number of Team",title="Number of Teams Participated Each Year")


################################################
### Merging athletes and region Data set. ######
################################################

merged_df <- athletes %>% left_join(regions, by = "NOC")

# Print() won't caoncatenate and print.Where as cat() will do.
cat("Total number of unique Teams participated till now - ",length(unique(merged_df$region)))

Tm_merged <- merged_df %>%
  group_by(Year,Season) %>%
  summarise(Num_Team = length(unique(region)))

Tm_merged %>%
  ggplot(aes(x=Year,y=Num_Team,group=Season))+
  geom_line(aes(color=Season))+
  geom_point(aes(color=Season))+
  labs(x="Year",y="Number of Team",title="Number of Teams Participated Each Year")

####################
### Boycott 1976 ###
####################

# Boycotted by 29 countries.
# Summer Olympics alsoboycotted on 1980.

############################
#### Medal Winners #########
############################

sum(is.na(merged_df$Medal))  ## We have so much of NA values in the Medal Data.
# This is explainable fact that,not every one could win the event. So, recode null values as
# "Did not Win" or "DNW"

table(merged_df$Medal)
merged_df$Medal[is.na(merged_df$Medal)] <- "DNW"

## Analysing by Medal Numbers

medal_df <- merged_df %>%
  group_by(region,Medal) %>%
  summarise(NUmber_Medal = n())


medal_gold <- medal_df %>% filter(Medal == "Gold")
medal_Silver <- medal_df %>% filter(Medal == "Silver")
medal_Bronze <- medal_df %>% filter(Medal == "Bronze")

colnames(medal_gold)[3] <- "Number_Gold"
colnames(medal_Silver)[3] <- "Number_Silver"
colnames(medal_Bronze)[3] <- "Number_Bronze"

medal_dummy <- full_join(x=medal_gold[,c(1,3)],y=medal_Silver[,c(1,3)],by="region")
medal_tot <- full_join(x=medal_dummy,y=medal_Bronze[,c(1,3)],by="region")
head(medal_tot)

# Top 10 Countries Based on Medal.
Top_Medal <- medal_tot[order(-medal_tot$Number_Gold,-medal_tot$Number_Silver,-medal_tot$Number_Bronze),]
View(Top_Medal)

Top_Medal[is.na(Top_Medal)] <- 0

head(Top_Medal) %>%
  ggplot(aes(x=region,y=Number_Gold))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(size = 10,angle = 45,vjust = .5))+
  labs(x="Region",y="Number of Gold",title="Top 5 Gold Winners since 1890")

# USA,Russia and Germany are the Top 3 Gold hunters.

#  View Table for more info.
View(Top_Medal)


### Plotting With MAP.

world_map <- map_data("world")

world_df <- left_join(world_map,Top_Medal,by="region")

View(world_df)


