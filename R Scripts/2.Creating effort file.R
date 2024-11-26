#2---Next step is to create the effort file from the camera operation matrix. 

#Read in the file. 
library(tidyverse)

CamOpMatrix <- read.csv("Raw Data/Camera Operability Matrix.csv", header = TRUE)

DatesComplete <- data.frame(seq(as.Date("2020-03-03"), as.Date("2023-06-01"), by = "day"))
DatesComplete <- DatesComplete %>%
  rename(Date = seq.as.Date..2020.03.03....as.Date..2023.06.01....by....day..)

#For the temporal analysis, we need to restrict to one camera per site, for consistency and effort. We'll restrict to just 'a' cameras. 


Onlyrelevantcam <- CamOpMatrix %>%
  filter(Camera %in% c("MS001", "MS002", "MS004", "MS007a", "MS017a", "MS023", "MS024a",  "MS026a", "MS016a", "MS021"))

Onlyrelevantcam$DateSiteStart <- as.POSIXct(Onlyrelevantcam$DateSiteStart, format = "%d/%m/%Y")
Onlyrelevantcam$DateSiteEnd <- as.POSIXct(Onlyrelevantcam$DateSiteEnd, format = "%d/%m/%Y")
Onlyrelevantcam$DateNotFunctionalStart1 <- as.POSIXct(Onlyrelevantcam$DateNotFunctionalStart1, format = "%d/%m/%Y")
Onlyrelevantcam$DateNotFunctionalEnd1 <- as.POSIXct(Onlyrelevantcam$DateNotFunctionalEnd1, format = "%d/%m/%Y")
Onlyrelevantcam$DateNotFunctionalStart2 <- as.POSIXct(Onlyrelevantcam$DateNotFunctionalStart2, format = "%d/%m/%Y")
Onlyrelevantcam$DateNotFunctionalEnd2 <- as.POSIXct(Onlyrelevantcam$DateNotFunctionalEnd2, format = "%d/%m/%Y")
Onlyrelevantcam$DateNotFunctionalStart3 <- as.POSIXct(Onlyrelevantcam$DateNotFunctionalStart3, format = "%d/%m/%Y")
Onlyrelevantcam$DateNotFunctionalEnd3 <- as.POSIXct(Onlyrelevantcam$DateNotFunctionalEnd3, format = "%d/%m/%Y")
Onlyrelevantcam$DateNotFunctionalStart4 <- as.POSIXct(Onlyrelevantcam$DateNotFunctionalStart4, format = "%d/%m/%Y")
Onlyrelevantcam$DateNotFunctionalEnd4 <- as.POSIXct(Onlyrelevantcam$DateNotFunctionalEnd4, format = "%d/%m/%Y")
Onlyrelevantcam$DateNotFunctionalStart5 <- as.POSIXct(Onlyrelevantcam$DateNotFunctionalStart5, format = "%d/%m/%Y")
Onlyrelevantcam$DateNotFunctionalEnd5 <- as.POSIXct(Onlyrelevantcam$DateNotFunctionalEnd5, format = "%d/%m/%Y")
Onlyrelevantcam$DateNotFunctionalStart6 <- as.POSIXct(Onlyrelevantcam$DateNotFunctionalStart6, format = "%d/%m/%Y")
Onlyrelevantcam$DateNotFunctionalEnd6 <- as.POSIXct(Onlyrelevantcam$DateNotFunctionalEnd6, format = "%d/%m/%Y")



active_dates <- rep(TRUE, nrow(DatesComplete))

for (i in 1:nrow(Onlyrelevantcam)) {
  camera_id <- Onlyrelevantcam$Camera[i]
  start_date <- Onlyrelevantcam$DateSiteStart[i]
  end_date <- Onlyrelevantcam$DateSiteEnd[i]
  NoFuncStart1 <- Onlyrelevantcam$DateNotFunctionalStart1[i]
  NoFuncEnd1 <- Onlyrelevantcam$DateNotFunctionalEnd1[i]
  NoFuncStart2 <- Onlyrelevantcam$DateNotFunctionalStart2[i]
  NoFuncEnd2 <- Onlyrelevantcam$DateNotFunctionalEnd2[i]
  NoFuncStart3 <- Onlyrelevantcam$DateNotFunctionalStart3[i]
  NoFuncEnd3 <- Onlyrelevantcam$DateNotFunctionalEnd3[i]
  NoFuncStart4 <- Onlyrelevantcam$DateNotFunctionalStart4[i]
  NoFuncEnd4 <- Onlyrelevantcam$DateNotFunctionalEnd4[i]
  NoFuncStart5 <- Onlyrelevantcam$DateNotFunctionalStart5[i]
  NoFuncEnd5 <- Onlyrelevantcam$DateNotFunctionalEnd5[i]
  NoFuncStart6 <- Onlyrelevantcam$DateNotFunctionalStart6[i]
  NoFuncEnd6 <- Onlyrelevantcam$DateNotFunctionalEnd6[i]
  
  active_dates <- (DatesComplete$Date >= start_date & DatesComplete$Date <= end_date)
  
  if (!is.na(NoFuncStart1) && !is.na(NoFuncEnd1)) {
    active_dates <- active_dates & !(DatesComplete$Date >= NoFuncStart1 & DatesComplete$Date <= NoFuncEnd1)
  }
  if (!is.na(NoFuncStart2) && !is.na(NoFuncEnd2)) {
    active_dates <- active_dates & !(DatesComplete$Date >= NoFuncStart2 & DatesComplete$Date <= NoFuncEnd2)
  }
  if (!is.na(NoFuncStart3) && !is.na(NoFuncEnd3)) {
    active_dates <- active_dates & !(DatesComplete$Date >= NoFuncStart3 & DatesComplete$Date <= NoFuncEnd3)
  }
  if (!is.na(NoFuncStart4) && !is.na(NoFuncEnd4)) {
    active_dates <- active_dates & !(DatesComplete$Date >= NoFuncStart4 & DatesComplete$Date <= NoFuncEnd4)
  }
  if (!is.na(NoFuncStart5) && !is.na(NoFuncEnd5)) {
    active_dates <- active_dates & !(DatesComplete$Date >= NoFuncStart5 & DatesComplete$Date <= NoFuncEnd5)
  }
  if (!is.na(NoFuncStart6) && !is.na(NoFuncEnd6)) {
    active_dates <- active_dates & !(DatesComplete$Date >= NoFuncStart6 & DatesComplete$Date <= NoFuncEnd6)
  }
  
  DatesComplete[[camera_id]] <- ifelse(active_dates, 'Active', 'Inactive')
  
  active_dates <- rep(TRUE, nrow(DatesComplete))
}

DatesComplete <- DatesComplete%>%
  rowwise()%>%
  mutate(ActiveCameras = sum(c_across(starts_with("MS")) == "Active"))%>%
  ungroup()

#This worked! This will be my file Which has the active dates. Now i need to add in the other 'non-functional' dates into this too. 

DatesCompleteLong <- DatesComplete %>%
  pivot_longer(
    cols = -c(Date, ActiveCameras),
    names_to = "Site",
    values_to = "Status"
  )
#We now have a 

#Change from 'a' to just site name. 

DatesCompleteLong <- DatesCompleteLong%>%
  mutate(Site = case_when(
    Site == "MS007a" ~ "MS007",
    Site == "MS017a" ~ "MS017",
    Site == "MS024a" ~ "MS024",
    Site == "MS026a" ~ "MS026",
    Site == "MS016a" ~ "MS016",
    
    TRUE ~ Site
  ))

#Add in the years as factor variables, to include as a random effect. 
DatesCompleteLong <- DatesCompleteLong%>%
  mutate(MonthDay = format(Date, "%m-%d"),
         Year = factor(year(Date), levels = c("2020", "2021", "2022", "2023")))


#Lets just see if i've done this wrong: Lets code a variable called "Active" which will be 1 or 0. Dependent on 'Status' being 'Active' or 'Inactive'

DatesCompleteLong <- DatesCompleteLong %>%
  mutate(Active = case_when(
    Status == "Active" ~ "1",
    Status == "Inactive"~ "0",
    TRUE ~ Status))

#summary no. days active
activedays <- DatesCompleteLong %>%
  group_by(Site)%>%
  summarise(ActiveDays = sum(as.numeric(Active)))

mean(activedays$ActiveDays) #====571.1
range(activedays$ActiveDays)### 367-889


#Save file
write.csv(DatesCompleteLong, "Derived Data/DatesCamerasActive.csv")

#done. 
