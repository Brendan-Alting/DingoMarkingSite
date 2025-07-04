########3.1 Generalised addtive models seasonal trends: 
library(tidyverse)
library(tidygam)
library(broom)
library(mgcv)
library(ggeffects)
library(grid)
library(RColorBrewer)
library(patchwork)
#create new column, which says how many individuals are in each sequence. Also, calculates if there are pairs or not, and they must contain both a male and a female. 



AllBehavioursDingoesAdultsUR <- AllBehavioursDingoesAdults %>%
  group_by(Date_Time_AEST)%>%
  mutate(Indivs = n_distinct(Position_in_video))%>%
  mutate(Behaviour = case_when(
    Behaviour == "Raised Leg Urination" ~ "Urination",
    Behaviour == "Squat Urination" ~ "Urination", 
    TRUE ~as.character(Behaviour)
  ))%>%
  mutate(Pairs = ifelse(Indivs == 2 & n_distinct(Sex) == 2, 1, 0))%>%
  ungroup()


#Using just camera 'a' to account for effort . 

justcama <- AllBehavioursDingoesAdultsUR[AllBehavioursDingoesAdultsUR$Camera == "A",]



#First lets reformat data so we can merge

behaviour_counts <- justcama %>%
  mutate(Date = date(Date_Time_AEST)) %>%
  group_by(Date_Time_AEST, Site_ID, Behaviour) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Behaviour, values_from = Count, values_fill = list(Count = 0)) %>%
  rename(Site = Site_ID)

indivs_counts <- justcama %>%
  mutate(Date = date(Date_Time_AEST)) %>%
  group_by(Date_Time_AEST, Site_ID) %>%
  summarise(Indivs = first(Indivs), .groups = 'drop') %>%
  rename(Site = Site_ID)

sniffsnomark <- justcama %>%
  mutate(Date = date(Date_Time_AEST)) %>%
  group_by(Date_Time_AEST, Site_ID, Pile_ID, Position_in_video) %>%
  mutate(
    SniffMark = if_else(
      any(Behaviour == "Sniff") &
        any(Behaviour %in% c("Urination", "Defecation", "Rake")),
      1, 0
    ),
    SniffNoMark = if_else(
      any(Behaviour == "Sniff") &
        !any(Behaviour %in% c("Urination", "Defecation", "Rake")),
      1, 0
    )
  ) %>%
  ungroup() %>%
  rename(Site = Site_ID) %>%
  select(Date_Time_AEST, Site, SniffNoMark, SniffMark)

sniffsnomark <- sniffsnomark %>%
  group_by(Date_Time_AEST, Site)%>%
  summarise(SniffMark = sum(SniffMark),
            SniffNoMark = sum(SniffNoMark))

pairs_counts <- justcama %>%
  mutate(Date = date(Date_Time_AEST)) %>%
  group_by(Date_Time_AEST, Site_ID) %>%
  summarise(Pairs = first(Pairs), .groups = 'drop') %>%
  rename(Site = Site_ID)


behaviour_counts <- behaviour_counts %>%
  left_join(indivs_counts, by = c("Date_Time_AEST", "Site"))

behaviour_counts <- behaviour_counts %>%
  left_join(pairs_counts, by = c("Date_Time_AEST", "Site"))

behaviour_counts <- behaviour_counts %>%
  left_join(sniffsnomark, by = c("Date_Time_AEST", "Site"))

behaviour_counts$Date <- date(behaviour_counts$Date_Time_AEST)



########SEPARATE

#Separately, doing the 'individual_time_spent
justcama$DurationTotal <- as.numeric(justcama$DurationTotal)

individual_time_spent <- justcama %>%
  group_by(Date_Time_AEST, Position_in_video,Site_ID) %>%
  summarise(DurationTotal = first(DurationTotal), .groups = 'drop')

individual_time_spent <- individual_time_spent[-is.na(individual_time_spent$DurationTotal),]



individual_time_spent <- individual_time_spent %>%
  mutate(Month = factor(month.name[month(Date_Time_AEST)], levels = month.name), 
         Year = factor(year(Date_Time_AEST), levels = c("2020", "2021", "2022", "2023")))



monthly_mean_time <- individual_time_spent %>%
  group_by(Year, Month,Site_ID) %>%
  summarise(MeanTimeSpent = sum(DurationTotal, na.rm = TRUE), .groups = 'drop')


monthly_mean_time$MeanTimeSpentLog <- log(monthly_mean_time$MeanTimeSpent)
monthly_mean_time$MeanTimeSpentten <- (monthly_mean_time$MeanTimeSpent)/9.75

monthly_mean_time<- monthly_mean_time%>%
  rename(Site=Site_ID)
##Done the time spent at site. 

###


EffortandBehaviour <- DatesCompleteLong %>%
  left_join(behaviour_counts, by = c("Date" = "Date", "Site" = "Site"))

EffortandBehaviour <- EffortandBehaviour[-c(2,8)]


EffortandBehaviour[is.na(EffortandBehaviour)] <- 0

EffortandBehaviour$Month <- months(EffortandBehaviour$Date)

#Finally, using the 'no response' column, we can create a 'response' column. 
EffortandBehaviour$Response <- EffortandBehaviour$Indivs - EffortandBehaviour$`No Response`

#Grouping everything together. 

SummedBehaviours <- EffortandBehaviour %>%
  group_by(Date,Site)%>%
  summarize(
    Active = as.numeric(Active),
    Month = first(Month),
    Year = first(Year),
    Pairs = sum(Pairs),
    `Sniff No Mark` = sum(`SniffNoMark`, na.rm=TRUE),
    `Sniff Mark` = sum(`SniffMark`, na.rm=TRUE),
    `No Response` = sum(`No Response`, na.rm = TRUE),
    `Response` = sum(`Response`, na.rm = TRUE),
    Sniff = sum(Sniff, na.rm = TRUE),
    `Urination` = sum(`Urination`, na.rm = TRUE),
    Rake = sum(Rake, na.rm = TRUE),
    Defecation = sum(Defecation, na.rm = TRUE),
    Visits = sum(Indivs, na.rm = TRUE),
    Site = Site,
    .groups = 'drop'
  )



MonthlySums <- SummedBehaviours %>%
  group_by(Month, Year, Site)%>%
  summarize(
    Active = sum(Active),
    Pairs = sum(Pairs, na.rm = TRUE),
    SniffMarkSum = sum(`Sniff Mark`, na.rm = TRUE),
    SniffNoMarkSum = sum(`Sniff No Mark`, na.rm = TRUE),
    URsum = sum(`Urination`, na.rm = TRUE),
    Sniffsum = sum(Sniff, na.rm = TRUE),
    Rakesum = sum(Rake, na.rm = TRUE),
    Defsum = sum(Defecation, na.rm = TRUE),
    Noresponsesum = sum(`No Response`, na.rm = TRUE),
    Responsesum = sum(`Response`, na.rm = TRUE),
    Visits = sum(Visits, na.rm = TRUE),
    .groups = 'drop'
  )

month_order <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')

MonthlySums$Month <- factor(MonthlySums$Month, levels = month_order)


#Now merge this with the 'monthlymeantime' dataset. 

MonthlySums <- MonthlySums %>%
  left_join(monthly_mean_time, by = c("Month" = "Month", "Year" = "Year","Site" = "Site"))



MonthlySumsLong <- MonthlySums %>%
  pivot_longer(
    cols = -c(Month,Active, Year, Site),
    names_to = "Behaviour",
    values_to = "Sum"
  )%>%
  mutate(Sum=replace_na(Sum,0))

#assign factor for gams. 

MonthlySumsLong$Behaviour <- factor(MonthlySumsLong$Behaviour, levels = c("MeanTimeSpentten", "URsum", "Sniffsum", "Rakesum", "Defsum", "Noresponsesum","Responsesum", "Visits","Pairs", "MeanTimeSpent", "SniffNoMarkSum", "SniffMarkSum"))

MonthlySumsLong$Site <- factor(MonthlySumsLong$Site, levels = c("MS001", "MS002", "MS003", "MS004", "MS007", "MS016", "MS017", "MS021", "MS023","MS024", "MS026"))

#Define month order.
month_order <- c("January", "February", "March", "April", "May", "June", "July", "August","September", "October", "November", "December")

#assign as numeric for month as it needs continuous. 
MonthlySumsLong$Month <- as.numeric(factor(MonthlySumsLong$Month, levels = month_order))

#Lets split into three groups. 

#Group 1 = 'overall visits' : Pairs, Visits, Mean Time Spent at site.
#Group 2 = 'responses' visits: Sniffs, No response
#Group 3 = 'Behavioural visits': UR, Rake, Defs 

#Final groups: 

Generalgroup <- MonthlySumsLong%>%
  filter(Behaviour %in% c("Pairs", "MeanTimeSpentten","Visits"))

Responsegroup <- MonthlySumsLong %>%
  filter(Behaviour %in% c("Responsesum", "Noresponsesum"))

Behaviourgroup <- MonthlySumsLong %>%
  filter(Behaviour %in% c("URsum",  "Rakesum", "Defsum"))


#Now run models for each. 


gamgeneral <- gam(Sum~ Behaviour +s(Month, by = Behaviour, k = 4, bs = "cc")+s(Year, bs = "re")+s(Site, bs = "re"), offset = log(Active+1), data = Generalgroup, family = "nb")
summary(gamgeneral)

plotdatageneral <- predict_gam(
  gamgeneral,
  exclude_terms = c("s(Year)", "s(Site)"),
  series = "Month",
  length_out = 100,
  tran_fun = exp
) %>%
  mutate(Behaviour = recode(Behaviour,
                            "Pairs" = "Paired visits", 
                            "MeanTimeSpentten" = "Mean visit \nduration(secs/10)",
                            "Visits" = "Total visits"))


plotgeneral <- ggplot(plotdatageneral, aes(x = Month, y = Sum, color = Behaviour, fill = Behaviour)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.4, color = NA) +
  geom_line(size = 0.8) +
  scale_x_continuous(breaks = 1:12, labels = month.abb[1:12]) +
  scale_color_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  theme_minimal() +
  theme(
    legend.text = element_text(size = 25),
    legend.title = element_blank(),
    legend.key.height = unit(1.5, "cm"),
    legend.spacing = unit(1.5, "cm"),
    axis.title = element_text(size = 26),
    axis.text = element_text(size = 23),
    axis.text.x = element_text(size = 25, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 25)
  ) +
  labs(y = "Behaviours / active camera night") +
  geom_vline(xintercept = 3, linetype = "dashed", color = "black", size = 1) +
  geom_vline(xintercept = 6, linetype = "dashed", color = "black", size = 1)

plotgeneral


#2. response- no response or response

gamresponse <- gam(Sum~ s(Month, by = Behaviour, k = 4, bs = "cc")+s(Year, bs = "re")+s(Site, bs = "re")+Behaviour, offset = log(Active+1), data = Responsegroup, family = "nb")
summary(gamresponse)

plotdataresponse <- predict_gam(
  gamresponse,
  exclude_terms = c("s(Year)", "s(Site)"),
  series = "Month",
  length_out = 100,
  tran_fun = exp
) %>%
  mutate(Behaviour = recode(
    Behaviour,
    "Responsesum" = "Sniff site during visit",
    "Noresponsesum" = "Ignore (do not sniff) \nsite during visit"
  ),
  Behaviour = factor(Behaviour, levels = c("Sniff site during visit", "Ignore (do not sniff) \nsite during visit")))




plotresponse <- ggplot(plotdataresponse, aes(x = Month, y = Sum, color = Behaviour, fill = Behaviour)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.4, color = NA) +
  geom_line(size = 0.8) +
  scale_x_continuous(breaks = 1:12, labels = month.abb[1:12]) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 25),
    legend.title = element_blank(),
    legend.key.height = unit(1.5, "cm"),
    legend.spacing = unit(1.5, "cm"),
    axis.title = element_text(size = 26),
    axis.text = element_text(size = 23),
    axis.text.x = element_text(size = 25, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 25)
  ) +
  labs(y = "Behaviours / active camera night") +
  geom_vline(xintercept = 3, linetype = "dashed", color = "black", size = 1) +
  geom_vline(xintercept = 6, linetype = "dashed", color = "black", size = 1)

plotresponse

#3. behavioural

gambehaviour <- gam(Sum~ s(Month, by = Behaviour, k = 4, bs = "cc")+s(Year, bs = "re")+s(Site, bs = "re")+Behaviour, offset = log(Active+1), data = Behaviourgroup, family = "nb")
summary(gambehaviour)

plotdatabehaviour <- predict_gam(
  gambehaviour,
  exclude_terms = c("s(Year)", "s(Site)"),
  series = "Month",
  length_out = 100,
  tran_fun = exp
) %>%
  mutate(Behaviour = recode(
    Behaviour,
    "URsum" = "Urinations",
    "Rakesum" = "Rakes",
    "Defsum" = "Defecations"
  ))

plotbehaviour <- ggplot(plotdatabehaviour, aes(x = Month, y = Sum, color = Behaviour, fill = Behaviour)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.4, color = NA) +
  geom_line(size = 0.8) +
  scale_x_continuous(breaks = 1:12, labels = month.abb[1:12]) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 25),
    legend.title = element_blank(),
    legend.key.height = unit(1.5, "cm"),
    legend.spacing = unit(1.5, "cm"),
    axis.title = element_text(size = 26),
    axis.text = element_text(size = 23),
    axis.text.x = element_text(size = 25, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 25)
  ) +
  labs(y = "Behaviours / active camera night") +
  geom_vline(xintercept = 3, linetype = "dashed", color = "black", size = 1) +
  geom_vline(xintercept = 6, linetype = "dashed", color = "black", size = 1)

plotbehaviour



#Now export :: use patchwork to stitch together: 
##print figures all together

png("Figures/General Gam.png", width = 10, height = 7, res= 300, units = "in")
plotgeneral 
dev.off()

png("Figures/Response Gam.png", width = 10, height = 7, res= 300, units = "in")
plotresponse 
dev.off()

png("Figures/Behaviour Gam.png", width = 10, height = 7, res= 300, units = "in")
plotbehaviour 
dev.off()

#All together
png("Figures/Gams Together.png", width = 15, height = 21, res= 300, units = "in")
(plotgeneral /plotresponse /plotbehaviour) + plot_annotation(tag_levels = "a")& theme(text = element_text(size = 20))

dev.off()

#finally print results for each for supp materials. 

gamgeneralsmooth <- tidy(gamgeneral, conf.int = TRUE) 
gamgeneralpara <- tidy(gamgeneral,parametric = TRUE, conf.int = TRUE) 

gamresponsesmooth <- tidy(gamresponse, conf.int = TRUE) 
gamresponsepara <- tidy(gamresponse,parametric = TRUE, conf.int = TRUE) 

gambehavioursmooth <- tidy(gambehaviour, conf.int = TRUE) 
gambehaviourpara <- tidy(gambehaviour,parametric = TRUE, conf.int = TRUE) 


#and save
write.csv(gamgeneralsmooth, "Derived Data/gamgeneralsmooth.csv")
write.csv(gamgeneralpara, "Derived Data/gamgeneralpara.csv")
write.csv(gamresponsesmooth, "Derived Data/gamresponsesmooth.csv")
write.csv(gamresponsepara, "Derived Data/gamresponsepara.csv")
write.csv(gambehavioursmooth, "Derived Data/gambehavioursmooth.csv")
write.csv(gambehaviourpara, "Derived Data/gambehaviourpara.csv")


#end