#Script 1= data restructuring, for both analyses. I effectively need to make an 'effort' column, which can describe how likely an event was to occur in that time. Make a sequence of dates, and then fill that with a 'yes' or 'no' for if the camera was active then. 

#I need to do this separately, for both the temporal analysis long term (e.g. just visitations, or just marking events), and for the behaviours where entire behaviour captured. 

library(tidyverse)


AllBehaviours <- read.csv(file = "Raw Data/ScentMarkingAllTogether.csv", header = T)

#Assign posixct to dates and times. 
AllBehaviours$Date_Time_AEST <- as.POSIXct(AllBehaviours$Date_Time_AEST, format = "%d/%m/%Y %H:%M")

#assign occasions for each site. Independent events. 

AllBehaviours <- AllBehaviours %>%
  arrange(Site_ID, Date_Time_AEST)%>%
  group_by(Site_ID)%>%
  mutate(Occasiontest = dense_rank(Date_Time_AEST))%>%
  ungroup()

#that worked.

#now need to get all behaviours and substrates into the same format, as called them different things. 

unique(AllBehaviours$Substrate)

AllBehaviours <- AllBehaviours%>%
  mutate(Substrate = case_when(
    Substrate == "Grass Tuft" ~ "GrassTuft",
    Substrate == "Earth" ~ "Ground",
    Substrate == "Earth Mound" ~ "Ground",
    Substrate == "Veg" ~ "Bush",
    Substrate == "Sand" ~ "Ground",
    Substrate == "Shrub" ~ "Bush",
    TRUE ~ Substrate
  ))
unique(AllBehaviours$Substrate)

#Done for substrate

#Now lets do for behaviour

#rename column because it's a disaster. 
AllBehaviours <- AllBehaviours %>%
  rename(Behaviour =Behaviour...NR...No.Response..SN...Sniff..RL...Raised.Leg.Urination..SQU...Squat.Urination..SPR...Spray.Urination..SBR...Sniff...Brush..RFL...Rake.Front.Legs..RBL...Rake.Back.Legs..RAL...Rake.All.Legs..D...Defecate..RO...Roll....)


AllBehaviours <- AllBehaviours%>%
  mutate(Behaviour = case_when(
    Behaviour == "RLu" ~ "Raised Leg Urination",
    Behaviour == "RLU" ~ "Raised Leg Urination",
    Behaviour == "N/A" ~ "No Response",
    Behaviour == "NR" ~ "No Response",
    Behaviour == "D" ~ "Defecation",
    Behaviour == "DF" ~ "Defecation",
    Behaviour == "RLL" ~ "Rake",
    Behaviour == "RAL" ~ "Rake",
    Behaviour == "RFL" ~ "Rake",
    Behaviour == "RBL" ~ "Rake",
    Behaviour == "SN " ~ "Sniff",
    Behaviour == "SN" ~ "Sniff",
    Behaviour == "Sn" ~ "Sniff",
    Behaviour == "sn" ~ "Sniff",
    Behaviour == "RLY" ~ "Raised Leg Urination",
    Behaviour == "SB" ~ "Sniff",
    Behaviour == "SPR" ~ "Spray",
    Behaviour == "SQU" ~ "Squat Urination",
    TRUE ~ Behaviour
  ))

unique(AllBehaviours$Behaviour)
#Great, done this. 

#Now for adult or subadult

unique(AllBehaviours$Age..all.likely.adults.unless.otherwise.stated.)

AllBehaviours <- AllBehaviours %>%
  rename(Age =Age..all.likely.adults.unless.otherwise.stated.)


AllBehaviours <- AllBehaviours%>%
  mutate(Age = case_when(
    Age == "" ~ "Adult",
    Age == "juv? " ~ "Juvenile",
    Age == "juv?" ~ "Juvenile",
    Age == "Sub-Adult" ~ "Juvenile",
    Age == "Pup" ~ "Juvenile",
    Age == "juvenile? " ~ "Juvenile",
    Age == "Maybe JUV" ~ "Juvenile",
    Age == "juvenile?" ~ "Juvenile",
    Age == "pup" ~ "Juvenile",
    Age == "N/A" ~ "Adult",
    Age == "Sub-adult" ~ "Juvenile",
    Age == "SubAdult" ~ "Juvenile",
    Age == "Juvenile?" ~ "Juvenile",
    TRUE ~ Age
  ))

unique(AllBehaviours$Age)
#Great, Done again. 

#Do for ground sniff. 

unique(AllBehaviours$SniffedGround)

AllBehaviours <- AllBehaviours%>%
  mutate(SniffedGround = case_when(
    SniffedGround == "n" ~ "N",
    SniffedGround == "y" ~ "Y",
    
    TRUE ~ SniffedGround
  ))
#done

#Next for Duration Behaviour.s


AllBehaviours <- AllBehaviours %>%
  rename(DurationBehaviour =Duration_Behaviour.s.)

unique(AllBehaviours$DurationBehaviour)


#Done for  duration behaviours

#Next for duration total at site

AllBehaviours <- AllBehaviours %>%
  rename(DurationTotal = Duration_Total.s.)

unique(AllBehaviours$DurationTotal)

#Done for duration total 



#Next video missed behaviour before and after. 

AllBehaviours <- AllBehaviours %>%
  rename(MissedBefore = Video.missed.behaviour.before.,
         MissedAfter = Video.Missed.Behaviour.After.)


#rename before

AllBehaviours <- AllBehaviours%>%
  mutate(MissedBefore = case_when(
    MissedBefore == "n" ~ "No",
    MissedBefore == "y" ~ "Yes",
    MissedBefore == "Y" ~ "Yes",
    MissedBefore == "N" ~ "No",
    TRUE ~ MissedBefore
  ))

unique(AllBehaviours$MissedBefore)

#rename after

AllBehaviours <- AllBehaviours%>%
  mutate(MissedAfter = case_when(
    MissedAfter == "n" ~ "No",
    MissedAfter == "y" ~ "Yes",
    MissedAfter == "Y" ~ "Yes",
    MissedAfter == "N" ~ "No",
    TRUE ~ MissedAfter
  ))

unique(AllBehaviours$MissedAfter)

#Done before and After. 

#Now do Sex
unique(AllBehaviours$Sex)

AllBehaviours <- AllBehaviours%>%
  mutate(Sex = case_when(
    Sex == "f" ~ "Female",
    Sex == "F" ~ "Female",
    Sex == "female" ~ "Female",
    Sex == "M" ~ "Male",
    Sex == "m" ~ "Male",
    Sex == "male" ~ "Male",
    Sex == "N/A" ~ "Unclear",
    TRUE ~ Sex
  ))

unique(AllBehaviours$Sex)
#Now done for sex
#Now do for camera 
unique(AllBehaviours$Camera)

AllBehaviours <- AllBehaviours%>%
  mutate(Camera = case_when(
    Camera == "a" ~ "A",
    Camera == "b" ~ "B",
    TRUE ~ Camera
  ))

unique(AllBehaviours$Camera)

#Now done for camera, do for species
unique(AllBehaviours$Species)

AllBehaviours <- AllBehaviours %>%
  mutate(Species = case_when(
    Species == "WildDog" ~ "PetDog",
    Species == "HumanDog" ~ "PetDog",
    TRUE ~ Species
  ))

unique(AllBehaviours$Species)
#Done for species

write.csv(AllBehaviours, "Derived Data/CleanedTranscription.csv", row.names=F)

#End. 

