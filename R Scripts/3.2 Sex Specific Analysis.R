#3.2 looking at probability of marks. Who first, who last, who second third fourth etc. 
library(tidyverse)
library(MASS)
library(ggplot2)
library(lme4)
library(patchwork)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggeffects)


#first lets remove any events that we aren't sure we got whole behaviour. 

Onlyconf <- AllBehavioursDingoesAdults %>%
  filter(!(MissedBefore == "Yes" | MissedAfter == "Yes" | Sex == "Unclear" | Count_Individuals <= 1 ))%>%
  mutate(Behaviour = case_when(
    Behaviour == "Raised Leg Urination" ~ "Urination",
    Behaviour == "Squat Urination" ~ "Urination", 
    TRUE ~Behaviour))%>%
  group_by(Date_Time_AEST)%>%
  filter(all(c("Male", "Female")%in% Sex)) %>%
  ungroup()


Onlyconf$Date_Time_AEST <- as.POSIXct(Onlyconf$Date_Time_AEST, format = "%Y-%m-%d %H:%M:%S")

#Define 1st mark, 2nd mark, 3rd mark. 

#######Testing Testing 1 2
Onlyconf <- Onlyconf %>%
  group_by(Date_Time_AEST, Pile_ID) %>%
  mutate(
    # Identify the index of the first urination event, or Inf if none
    first_urination_idx = if_else(any(Behaviour == "Urination"), which.min(ifelse(Behaviour == "Urination", row_number(), Inf)), Inf),
    
    # Determine the sex of the individual who performed the first urination, or NA if none
    first_urination_sex = if_else(first_urination_idx == Inf, NA_character_, Sex[first_urination_idx]),
    
    first_sniff_before_urination_idx = if_else(
      any(Behaviour == "Sniff") & min(row_number()[Behaviour == "Sniff" & row_number() < first_urination_idx], default = Inf) != Inf,
      min(row_number()[Behaviour == "Sniff" & row_number() < first_urination_idx]),
      Inf
    ),
    
    # Determine the sex of the individual who performed the first sniff before urination, or NA if none
    first_sniff_before_urination_sex = if_else(
      first_sniff_before_urination_idx != Inf,
      Sex[first_sniff_before_urination_idx],
      NA_character_
    ),
    # Identify the index of the second urination event, or Inf if none
    second_urination_idx = if_else(
      any(Behaviour == "Urination") & sum(Behaviour == "Urination") > 1,
      which.min(ifelse(Behaviour == "Urination" & row_number() > first_urination_idx, row_number(), Inf)),
      Inf
    ),
    
    # Determine the sex of the individual who performed the second urination, or NA if none
    second_urination_sex = if_else(second_urination_idx == Inf, NA_character_, Sex[second_urination_idx]),
    
    # Identify the index of the third urination event, or Inf if none
    third_urination_idx = if_else(
      any(Behaviour == "Urination") & sum(Behaviour == "Urination") > 2,
      which.min(ifelse(Behaviour == "Urination" & row_number() > second_urination_idx, row_number(), Inf)),
      Inf
    ),
    
    # Determine the sex of the individual who performed the third urination, or NA if none
    third_urination_sex = if_else(third_urination_idx == Inf, NA_character_, Sex[third_urination_idx]),
    
    # Identify the index of the fourth urination event, or Inf if none
    fourth_urination_idx = if_else(
      any(Behaviour == "Urination") & sum(Behaviour == "Urination") > 3,
      which.min(ifelse(Behaviour == "Urination" & row_number() > third_urination_idx, row_number(), Inf)),
      Inf
    ),
    
    # Determine the sex of the individual who performed the fourth urination, or NA if none
    fourth_urination_sex = if_else(fourth_urination_idx == Inf, NA_character_, Sex[fourth_urination_idx]),
    
    # Identify the index of the fifth urination event, or Inf if none
    fifth_urination_idx = if_else(
      any(Behaviour == "Urination") & sum(Behaviour == "Urination") > 4,
      which.min(ifelse(Behaviour == "Urination" & row_number() > fourth_urination_idx, row_number(), Inf)),
      Inf
    ),
    
    # Determine the sex of the individual who performed the fifth urination, or NA if none
    fifth_urination_sex = if_else(fifth_urination_idx == Inf, NA_character_, Sex[fifth_urination_idx]),
    
    # Determine if there is a sniff event after the first urination and which sex performs it
    first_sniff_after_urination = if_else(
      any(Behaviour == "Sniff") & min(row_number()[Behaviour == "Sniff" & row_number() > first_urination_idx], default = Inf) != Inf,
      Sex[min(row_number()[Behaviour == "Sniff" & row_number() > first_urination_idx], default = Inf)],
      NA_character_
    ),
    
    # Determine if there is a sniff event after the second urination and which sex performs it
    second_sniff_after_urination = if_else(
      any(Behaviour == "Sniff") & min(row_number()[Behaviour == "Sniff" & row_number() > second_urination_idx], default = Inf) != Inf,
      Sex[min(row_number()[Behaviour == "Sniff" & row_number() > second_urination_idx], default = Inf)],
      NA_character_
    ),
    
    # Determine if there is a sniff event after the third urination and which sex performs it
    third_sniff_after_urination = if_else(
      any(Behaviour == "Sniff") & min(row_number()[Behaviour == "Sniff" & row_number() > third_urination_idx], default = Inf) != Inf,
      Sex[min(row_number()[Behaviour == "Sniff" & row_number() > third_urination_idx], default = Inf)],
      NA_character_
    ),
    
    # Determine if there is a sniff event after the fourth urination and which sex performs it
    fourth_sniff_after_urination = if_else(
      any(Behaviour == "Sniff") & min(row_number()[Behaviour == "Sniff" & row_number() > fourth_urination_idx], default = Inf) != Inf,
      Sex[min(row_number()[Behaviour == "Sniff" & row_number() > fourth_urination_idx], default = Inf)],
      NA_character_
    ),
    
    # Determine if there is a sniff event after the fifth urination and which sex performs it
    fifth_sniff_after_urination = if_else(
      any(Behaviour == "Sniff") & min(row_number()[Behaviour == "Sniff" & row_number() > fifth_urination_idx], default = Inf) != Inf,
      Sex[min(row_number()[Behaviour == "Sniff" & row_number() > fifth_urination_idx], default = Inf)],
      NA_character_
    )
  ) %>%
  ungroup()





####Now calculating for every individual event: 
Onlyconf_eventpile <- Onlyconf %>%
  group_by(Date_Time_AEST, Pile_ID) %>%
  slice(1) %>%  # Take the first row for each group
  filter(Pile_ID != "N/A") %>%
  dplyr::select(Date_Time_AEST, Site_ID, Count_Individuals, Position_in_video, Pile_ID, Seq_ID, Month,
         first_sniff_before_urination_sex, first_urination_sex, first_sniff_after_urination,
         second_urination_sex, second_sniff_after_urination, third_urination_sex, third_sniff_after_urination,
         fourth_urination_sex, fourth_sniff_after_urination, fifth_urination_sex, fifth_sniff_after_urination) %>%
  ungroup() %>%
  mutate(
    Marked_Last = case_when(
      is.na(second_urination_sex) & first_urination_sex == "Male" ~ "Male",
      is.na(second_urination_sex) & first_urination_sex == "Female" ~ "Female",
      is.na(third_urination_sex) & !is.na(second_urination_sex) & second_urination_sex == "Male" ~ "Male",
      is.na(third_urination_sex) & !is.na(second_urination_sex) & second_urination_sex == "Female" ~ "Female",
      is.na(fourth_urination_sex) & !is.na(third_urination_sex) & third_urination_sex == "Male" ~ "Male",
      is.na(fourth_urination_sex) & !is.na(third_urination_sex) & third_urination_sex == "Female" ~ "Female",
      is.na(fifth_urination_sex) & !is.na(fourth_urination_sex) & fourth_urination_sex == "Male" ~ "Male",
      is.na(fifth_urination_sex) & !is.na(fourth_urination_sex) & fourth_urination_sex == "Female" ~ "Female",
      TRUE ~ NA_character_
    )
  )


#Done, now we have each event with pairs, and who marked first, who sniffed, and who marked 2nd. 



#Lets add on column where it says if a female has marked before or no. 


Onlyconf_eventpile <- Onlyconf_eventpile %>%
  mutate(
    male_already_marked_1 = ifelse(first_urination_sex == "Male", "Yes", "No"),
    female_already_marked_1 = ifelse(first_urination_sex == "Female", "Yes", "No"),
    male_already_marked_2 = ifelse(first_urination_sex == "Male" | second_urination_sex == "Male","Yes","No"),
    female_already_marked_2 = ifelse(first_urination_sex == "Female" | second_urination_sex == "Female","Yes","No"),
    male_already_marked_3 = ifelse(first_urination_sex == "Male" | second_urination_sex == "Male"|third_urination_sex == "Male","Yes","No"),
    female_already_marked_3 = ifelse(first_urination_sex == "Female" | second_urination_sex == "Female"|third_urination_sex=="Female","Yes","No"),
    male_already_marked_4 = ifelse(first_urination_sex == "Male" | second_urination_sex == "Male"|third_urination_sex == "Male"|fourth_urination_sex=="Male","Yes","No"),
    female_already_marked_4 = ifelse(first_urination_sex == "Female" | second_urination_sex == "Female"|third_urination_sex=="Female"|fourth_urination_sex=="Female","Yes","No")
  )%>%
  mutate(
    male_already_marked_1 = ifelse(is.na(male_already_marked_1), "No", male_already_marked_1),
    female_already_marked_1 = ifelse(is.na(female_already_marked_1), "No", female_already_marked_1),
    male_already_marked_2 = ifelse(is.na(male_already_marked_2), "No", male_already_marked_2),
    female_already_marked_2 = ifelse(is.na(female_already_marked_2), "No", female_already_marked_2),
    male_already_marked_3 = ifelse(is.na(male_already_marked_3), "No", male_already_marked_3),
    female_already_marked_3 = ifelse(is.na(female_already_marked_3), "No", female_already_marked_3),
    male_already_marked_4 = ifelse(is.na(male_already_marked_4), "No", male_already_marked_4),
    female_already_marked_4 = ifelse(is.na(female_already_marked_4), "No", female_already_marked_4)
  )

#Reduce dataset to when mark first. So only the response to a mark, not the first mark. 
Firstsubset <- Onlyconf_eventpile%>%
  filter(first_urination_sex != "NA")

#Create an empty dataframe with variables we want. 
marking_data <- data.frame(
  mark = integer(),
  Sex_of_first_mark = character(),
  Sex_of_previous_mark = character(),
  Sex_of_Marker = character(),
  Month = character(),
  Date_Time_AEST = as.POSIXct(character()),
  Site = character(),
  MaleAlreadyMarked = character(),
  FemaleAlreadyMarked = character(),
  Number_of_Previous_Marks = integer(),
  stringsAsFactors = FALSE
)


#Run this silly code: 

###########SAVE FOR LATER######################THis is for only if they sniff- AKA encounter. 
# Iterate through each row of Firstsubset
for (i in seq_len(nrow(Firstsubset))) {
  row <- Firstsubset[i, ]
  
  # Define the marks, sniffs, and already marked columns
  urinations <- c(row$first_urination_sex, row$second_urination_sex, row$third_urination_sex, row$fourth_urination_sex, row$fifth_urination_sex)
  sniffs <- c(row$first_sniff_after_urination, row$second_sniff_after_urination, row$third_sniff_after_urination, row$fourth_sniff_after_urination, row$fifth_sniff_after_urination)
  
  # Define the already marked columns
  male_already_marked <- c(row$male_already_marked_1, row$male_already_marked_2, row$male_already_marked_3, row$male_already_marked_4, row$male_already_marked_5)
  female_already_marked <- c(row$female_already_marked_1, row$female_already_marked_2, row$female_already_marked_3, row$female_already_marked_4, row$female_already_marked_5)
  
  # Iterate through each urination mark from the second urination onwards
  for (j in 2:length(urinations)) {
    if (!is.na(urinations[j])) {
      # Create a row for a marking event
      new_row <- data.frame(
        mark = 1L,  # Marking event
        Sex_of_first_mark = urinations[1],
        Sex_of_previous_mark = urinations[j - 1],
        Sex_of_Marker = urinations[j],
        Month = row$Month,
        Date_Time_AEST = row$Date_Time_AEST,
        Site = row$Site_ID,
        MaleAlreadyMarked = male_already_marked[j - 1],
        FemaleAlreadyMarked = female_already_marked[j - 1],
        Number_of_Previous_Marks = j - 1  # Number of previous marks
      )
      marking_data <- bind_rows(marking_data, new_row)
    }
  }
  
  # Check for sniff after urination without subsequent urination
  for (j in 1:(length(urinations) - 1)) {
    if (!is.na(sniffs[j]) && is.na(urinations[j + 1])) {
      new_row <- data.frame(
        mark = 0L,  # No marking event
        Sex_of_first_mark = urinations[1],
        Sex_of_previous_mark = urinations[j],
        Sex_of_Marker = sniffs[j],
        Month = row$Month,
        Date_Time_AEST = row$Date_Time_AEST,
        Site = row$Site_ID,
        MaleAlreadyMarked = male_already_marked[j],
        FemaleAlreadyMarked = female_already_marked[j],
        Number_of_Previous_Marks = j  # Number of previous marks
      )
      marking_data <- bind_rows(marking_data, new_row)
    }
  }
}

#lets try run a glmm here. 
#first combine variables
marking_data <- marking_data %>%
  mutate(Sex.Previous_Sex.Marker = interaction(Sex_of_previous_mark,Sex_of_Marker))

marking_data$Sex.Previous_Sex.Marker <- factor(marking_data$Sex.Previous_Sex.Marker, levels = c("Male.Female","Male.Male","Female.Female","Female.Male"))

marking_data$Sex_of_first_mark <- factor(marking_data$Sex_of_first_mark,levels =c("Male","Female"))

marking_data$Sex_of_previous_mark <- factor(marking_data$Sex_of_previous_mark,levels =c("Male","Female"))

marking_data$Sex_of_Marker <- factor(marking_data$Sex_of_Marker,levels =c("Male","Female"))

marking_data$MaleAlreadyMarked <- factor(marking_data$MaleAlreadyMarked,levels =c("Yes","No"))

marking_data$FemaleAlreadyMarked <- factor(marking_data$FemaleAlreadyMarked,levels =c("Yes","No"))

marking_data$mark <- factor(marking_data$mark,levels =c("0","1"))

marking_data$Site <- factor(marking_data$Site,levels =c("MS001","MS002","MS004","MS007","MS016","MS017","MS021","MS023","MS024","MS026"))


###Finally, we'll remove female.Female as this only happened twice and ruining model. 

marking_data_nof <- marking_data%>%
  filter(!Sex.Previous_Sex.Marker == "Female.Female")


glmovermark <- glm(data = marking_data_nof,mark~ Sex.Previous_Sex.Marker+Number_of_Previous_Marks,family = binomial)



summary(glmovermark)

summaryglmm <- summary(glmovermark)

#Manually make 95% CIs from standard errors: 

estimates <- summaryglmm$coefficients[, "Estimate"]
se <- summaryglmm$coefficients[, "Std. Error"]

lower_bound <- estimates - 1.96 * se
upper_bound <- estimates + 1.96 * se

ci_manual <- data.frame(
  Estimate = estimates,
  Lower = lower_bound,
  Upper = upper_bound
)
###Lets properly plot:

predict_data <- expand.grid(
  Sex.Previous_Sex.Marker = levels(marking_data$Sex.Previous_Sex.Marker),
  Number_of_Previous_Marks = 1:4
  )%>%
  filter(!Sex.Previous_Sex.Marker == "Female.Female")



predicted_data <- predict(glmovermark, 
                                       newdata = predict_data, 
                                       type = "response",
                                       se.fit = TRUE,
                          re.form=TRUE)

predict_data$fit <- predicted_data$fit
predict_data$se.fit <- predicted_data$se.fit

predict_data$lower <- predict_data$fit -  predict_data$se.fit
predict_data$upper <- predict_data$fit +  predict_data$se.fit

###And change value name for plotting

predict_data <- predict_data %>%
  mutate(Sex.Previous_Sex.Marker = case_when(
    Sex.Previous_Sex.Marker == "Male.Male" ~ "Male dingo investigating\nmale scent",
    Sex.Previous_Sex.Marker == "Male.Female" ~ "Female dingo investigating\nmale scent",
    Sex.Previous_Sex.Marker == "Female.Male" ~ "Male dingo investigating\nfemale scent",
    TRUE ~ Sex.Previous_Sex.Marker  # Keep other values unchanged
  )
  )

predict_data$Sex.Previous_Sex.Marker <-factor(predict_data$Sex.Previous_Sex.Marker,levels = c("Male dingo investigating\nmale scent","Male dingo investigating\nfemale scent","Female dingo investigating\nmale scent"))

predictedplot<- ggplot(predict_data, aes(x = Number_of_Previous_Marks, y = fit)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3, position = position_dodge(width = 0.5)) +
  facet_wrap(~Sex.Previous_Sex.Marker, scales = "free_y") +  # Adjust scales if needed
  labs(
    title = "",
    x = "\nNumber of marks previously left at pile",
    y = "Predicted probability of dingo marking",
    color = "Sex of previous\nscent mark"
  ) +
  theme_minimal() +
  ylim(0,1)+
  theme(axis.title = element_text(size = 22),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        strip.text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))

predictedplot
#plotting model predicted effects from glmm

png("Figures/GLMMpredicted.png", width = 16, height = 9, res= 300, units = "in")
predictedplot
dev.off()







summary <- marking_data %>%
  group_by(Sex_of_first_mark, Sex_of_previous_mark, Sex_of_Marker, Number_of_Previous_Marks) %>%
  summarise(
    N = n(),
    count_1 = sum(mark == 1),
    count_0 = sum(mark == 0),
    prop_1 = mean(mark == 1),
    prop_0 = mean(mark == 0)
  )

summary2 <- marking_data %>%
  group_by(Sex_of_previous_mark, Sex_of_Marker, Number_of_Previous_Marks) %>%
  summarise(
    N = n(),
    count_1 = sum(mark == 1),
    count_0 = sum(mark == 0),
    prop_1 = mean(mark == 1),
    prop_0 = mean(mark == 0)
  )


summary2 <- summary2 %>%
  rename(`Times Investigated` = N,
         `Times Overmarked` = count_1)


###IGNORE####
library(tidyr)
data_long <- summary2 %>%
  pivot_longer(cols = c(`Times Investigated`,`Times Overmarked`), names_to = "count_type", values_to = "value")%>%
  ungroup()%>%
  filter(count_type == "Times Overmarked")


data_long$Number_of_Previous_Marksfm <- paste(data_long$Sex_of_previous_mark,data_long$Number_of_Previous_Marks)


data_long$count_type <- factor(data_long$count_type, levels = c("Times Overmarked"))
data_long$Sex_of_previous_mark <- factor(data_long$Sex_of_previous_mark, levels = c("Male", "Female"))
data_long$Number_of_Previous_Marks <- factor(data_long$Number_of_Previous_Marks, levels = c(1, 2, 3, 4))
data_long$Number_of_Previous_Marksfm <- factor(data_long$Number_of_Previous_Marksfm, levels = c("Male 1", "Male 2", "Male 3", "Male 4", "Female 1", "Female 2", "Female 3", "Female 4"))
data_long$Sex_and_Number <- with(data_long, interaction(Sex_of_previous_mark, Number_of_Previous_Marks))
data_long$Sex_and_Number <- factor(data_long$Sex_and_Number, levels = c("Male.1", "Male.2", "Male.3", "Male.4", "Female.1", "Female.2", "Female.3", "Female.4"))
data_long$Facet <- with(data_long, interaction(Sex_of_Marker, Sex_of_previous_mark))


overmarks <- ggplot(data_long, aes(x = Number_of_Previous_Marksfm, y = value, fill = Sex_of_Marker)) +
  geom_bar(stat = "identity", position = position_stack(), width = 0.8) +  # Adjust width and dodge position
  scale_fill_manual(values = c("turquoise3", "palegreen3")) +
  labs(
    x = "Position of prior mark",
    y = "No. overmarks",
    fill = "Sex"  # Remove legend title
  ) +
  scale_x_discrete(labels = c(
    "Male 1" = expression("Male Marks 1"^" st"),
    "Male 2" = expression("Male Marks 2"^" nd"),
    "Male 3" = expression("Male Marks 3"^" rd"),
    "Male 4" = expression("Male Marks 4"^" th"),
    "Female 1" = expression("Female Marks 1"^" st"),
    "Female 2" = expression("Female Marks 2"^" nd"),
    "Female 3" = expression("Female Marks 3"^" rd"),
    "Female 4" = expression("Female Marks 4"^" th")))+
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 80,hjust = 0.5, size = 13, margin = margin(r=50)),  # Larger x-axis text
    axis.text.x.bottom = element_text(vjust = 0.5),
    axis.text.y = element_text(size = 14),  # Larger y-axis text
    axis.title.x = element_text(size = 16, margin = margin(t = 30)),  # Larger x-axis title and lower margin
    axis.title.y = element_text(size = 16),  # Larger y-axis title
    legend.text = element_text(size = 16),  # Larger legend text
    legend.title = element_text(size =16, ),  # Remove legend title
    strip.text.x = element_blank(),  # Larger and bold strip text
    strip.background = element_blank(),  # Remove strip background
    plot.margin = margin(t = 40, l = 10, b = 5),
    panel.spacing = unit(1, "lines")  # Increase panel spacing
  ) +
  guides(fill = guide_legend(override.aes = list(size = 10, labels = c("Male" = "Male Overmark",
                                                                       "Female" = "Female Overmark"))))  # Adjust legend labels

overmarks


####Going to print this plot and use it :::

png("Figures/Overmarks2.png", width = 11, height = 9, res= 300, units = "in")
overmarks
dev.off()


















#Assign factors for binocount_1#Assign factors for binomial model: 
marking_data$Number_of_Previous_Marks <- factor(marking_data$Number_of_Previous_Marks, levels = c("1","2","3", "4"))
marking_data$mark <- factor(marking_data$mark, levels = c("0","1"))
marking_data$Sex_of_first_mark <- factor(marking_data$Sex_of_first_mark, levels = c("Male", "Female"))
marking_data$Sex_of_previous_mark <- factor(marking_data$Sex_of_previous_mark, levels = c("Male", "Female"))
marking_data$Sex_of_Marker <- factor(marking_data$Sex_of_Marker, levels = c("Male", "Female"))


#using the summary data, im going to try to create contingency table

cont_table <- xtabs(N ~ Sex_of_first_mark + Sex_of_previous_mark + Sex_of_Marker + Number_of_Previous_Marks, data=summary)
loglinmod <- loglm(~Sex_of_previous_mark+Sex_of_Marker, data = cont_table)
loglinmodint <- loglm(~Sex_of_previous_mark*Sex_of_Marker, data = cont_table)

summary(loglinmodint)

anova(loglinmod, loglinmodint)


###Ignore below
#Run binomial model 

binommodel <- glm(mark ~ Sex_of_Marker*Sex_of_previous_mark+Number_of_Previous_Marks, 
                  data = marking_data,
                  family = binomial(link = "probit"))
summary(binommodel)


predictions <- ggpredict(binommodel, terms = c("Number_of_Previous_Marks","Sex_of_Marker", "Sex_of_previous_mark"))
plot(predictions)
