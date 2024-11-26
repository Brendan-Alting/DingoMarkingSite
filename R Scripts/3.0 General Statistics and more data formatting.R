#3.0 Broadest results--- summary statistics. 
library(tidyverse)
library(lubridate)



AllBehaviours$Month <- months(AllBehaviours$Date_Time_AEST)
AllBehavioursDingoes <- AllBehaviours[AllBehaviours$Species == "Dingo",]
AllBehavioursDingoesAdults <- AllBehavioursDingoes[AllBehavioursDingoes$Age == "Adult",]


#15min threshold
threshold <- 15*60

indpendentevents <- AllBehavioursDingoes %>%
  arrange(Site_ID, Date_Time_AEST)%>%
  group_by(Site_ID)%>%
  mutate(time_diff = as.numeric(difftime(Date_Time_AEST, lag(Date_Time_AEST, default = first(Date_Time_AEST)), units = "secs"))) %>%
  filter(is.na(time_diff) | time_diff > threshold) %>%
  select(-time_diff)

#in total, 2375 independent events. 
indpendentevents %>% count(Site_ID) #mean = 238 (range = 46-685).

event_counts <- indpendentevents %>% 
  count(Site_ID)

sd_event_counts <- sd(event_counts$n)

#mean for sites = 2375 / 10 = 238 events. 

#independent events chicks 
indepedentchicks <- indpendentevents[indpendentevents$Sex == "Female",] #783 total
#indepednent events dudes
indepedentdudes <- indpendentevents[indpendentevents$Sex == "Male",] #1045 total

##now all behaviours

AllBehavioursDingoeschicks <- AllBehavioursDingoesAdults[AllBehavioursDingoesAdults$Sex == "Female",]
#1822 chicks

AllBehavioursDingoesdudes <- AllBehavioursDingoesAdults[AllBehavioursDingoesAdults$Sex == "Male",]
#2620 dudes


#Now work out the mean no. individuals. 
mean(indpendentevents$Count_Individuals) #1.436- range = 1-7
sd(indpendentevents$Count_Individuals)
#mean number of marks
AllBehavioursDingoeschicks %>%
  count(Behaviour == "Squat Urination") #281 in total 

AllBehavioursDingoesdudes %>%
  count(Behaviour == "Raised Leg Urination") #539 in total 

#urinations per independent visit chicks
281/783 #0.358876

#urinations per indepdent visit dudes
539/1045 # 0.515689


AllBehavioursDingoes %>%
  count(Behaviour == "Rake") #383 total

AllBehavioursDingoes %>%
  count(Behaviour == "Defecation") #56 total

AllBehavioursDingoes %>%
  count(Behaviour == "No Response") #1923 total

AllBehavioursDingoes %>%
  count(Behaviour == "Sniff") #2275 total


#end
