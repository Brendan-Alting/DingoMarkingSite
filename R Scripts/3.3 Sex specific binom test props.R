###3.3 binomial test of proportions for first, second, third mark etc. 


marking_presence <- Onlyconf_eventpile %>%
  group_by(Month)%>%
  summarise(
    First_Mark_Male = sum(ifelse(first_urination_sex == "Male", 1, 0), na.rm = TRUE),
    First_Mark_Female = sum(ifelse(first_urination_sex == "Female", 1, 0), na.rm = TRUE),
    Second_Mark_Male = sum(ifelse(second_urination_sex == "Male", 1, 0), na.rm = TRUE),
    Second_Mark_Female = sum(ifelse(second_urination_sex == "Female", 1, 0), na.rm = TRUE),
    Third_Mark_Male = sum(ifelse(third_urination_sex == "Male", 1, 0), na.rm = TRUE),
    Third_Mark_Female = sum(ifelse(third_urination_sex == "Female", 1, 0), na.rm = TRUE),
    Fourth_Mark_Male = sum(ifelse(fourth_urination_sex == "Male", 1, 0), na.rm = TRUE),
    Fourth_Mark_Female = sum(ifelse(fourth_urination_sex == "Female", 1, 0), na.rm = TRUE),
    Fifth_Mark_Male = sum(ifelse(fifth_urination_sex == "Male", 1, 0), na.rm = TRUE),
    Fifth_Mark_Female = sum(ifelse(fifth_urination_sex == "Female", 1, 0), na.rm = TRUE),
    Last_Mark_Male = sum(ifelse(Marked_Last == "Male", 1,0),na.rm = TRUE),
    Last_Mark_Female = sum(ifelse(Marked_Last == "Female", 1,0),na.rm = TRUE)
    
  ) 

#summarise to events

marking_presence <- marking_presence %>%
  pivot_longer(
    cols = -Month,
    names_to = c("mark", "sex"),
    names_pattern = "(.*)_Mark_(.*)",
    values_to = "count"
  ) %>%
  mutate(mark = factor(mark, levels = c("First", "Second", "Third", "Fourth", "Fifth", "Last")))%>%
  group_by(mark, Month)%>%
  mutate(Total = sum(count))%>%
  ungroup()%>%
  mutate(Prop = count/Total)


marking_presence$Month <- factor(marking_presence$Month, levels= month_order)

#for binom test of proportions, for first,second,third,fourth,fifth marks: 

marking_presence_order <- marking_presence %>%
  filter(mark %in% c("First", "Second", "Third", "Fourth", "Fifth", "Last"))%>%
  group_by(sex, mark)%>%
  summarise(total_count = sum(count, na.rm = TRUE), .groups = 'drop')


####Now for binom test. 

###first get into first marks, second marks, etc. 


# Summarize the counts for "First_Mark" 

#male first mark
first_mark_male_sum <- sum(marking_presence_order$total_count[marking_presence_order$sex == "Male" & marking_presence_order$mark == "First"])

#female first mark
first_mark_female_sum <- sum(marking_presence_order$total_count[marking_presence_order$sex == "Female" & marking_presence_order$mark == "First"])

#male last mark
last_mark_male_sum <- sum(marking_presence_order$total_count[marking_presence_order$sex == "Male" & marking_presence_order$mark == "Last"])

#female last mark
last_mark_female_sum <- sum(marking_presence_order$total_count[marking_presence_order$sex == "Female" & marking_presence_order$mark == "Last"])

#male Second mark
second_mark_male_sum <- sum(marking_presence_order$total_count[marking_presence_order$sex == "Male" & marking_presence_order$mark == "Second"])

#female last mark
second_mark_female_sum <- sum(marking_presence_order$total_count[marking_presence_order$sex == "Female" & marking_presence_order$mark == "Second"])

#male Third mark
third_mark_male_sum <- sum(marking_presence_order$total_count[marking_presence_order$sex == "Male" & marking_presence_order$mark == "Third"])

#female last mark
third_mark_female_sum <- sum(marking_presence_order$total_count[marking_presence_order$sex == "Female" & marking_presence_order$mark == "Third"])

#male Fourth mark
fourth_mark_male_sum <- sum(marking_presence_order$total_count[marking_presence_order$sex == "Male" & marking_presence_order$mark == "Fourth"])

#female last mark
fourth_mark_female_sum <- sum(marking_presence_order$total_count[marking_presence_order$sex == "Female" & marking_presence_order$mark == "Fourth"])

#male Fifth mark
fifth_mark_male_sum <- sum(marking_presence_order$total_count[marking_presence_order$sex == "Male" & marking_presence_order$mark == "Fifth"])

#female last mark
fifth_mark_female_sum <- sum(marking_presence_order$total_count[marking_presence_order$sex == "Female" & marking_presence_order$mark == "Fifth"])



total_sum_first <- first_mark_female_sum + first_mark_male_sum
total_sum_last <- last_mark_female_sum + last_mark_male_sum
total_sum_second <- second_mark_female_sum +second_mark_male_sum
total_sum_third <- third_mark_female_sum +third_mark_male_sum
total_sum_fourth <- fourth_mark_female_sum +fourth_mark_male_sum
total_sum_fifth <- fifth_mark_female_sum +fifth_mark_male_sum




first_mark_binom <- binom.test(first_mark_male_sum, total_sum_first, p = 0.5)
last_mark_binom <- binom.test(last_mark_male_sum, total_sum_last, p =0.5)
second_mark_binom <- binom.test(second_mark_male_sum, total_sum_second, p =0.5)
third_mark_binom <- binom.test(third_mark_male_sum, total_sum_third, p =0.5)
fourth_mark_binom <- binom.test(fourth_mark_male_sum, total_sum_fourth, p =0.5)
fifth_mark_binom <- binom.test(fifth_mark_male_sum, total_sum_fifth, p =0.5)


#print results
first_mark_binom
last_mark_binom
second_mark_binom
third_mark_binom
fourth_mark_binom
fifth_mark_binom


#for plotting basic counts:: get rid of last. 

marking_presence_order_counts <- marking_presence_order %>%
  filter(!mark == 'Last')%>%
  group_by(mark)%>%
  mutate(total = sum(total_count))%>%
  ungroup()%>%
  mutate(proportion = total_count / total,
         SE = sqrt(proportion*(1-proportion)/total))%>%
  rename(Sex = sex)

marking_presence_order_counts$Sex <- factor(marking_presence_order_counts$Sex, levels = c("Male", "Female"))
##and plot

marking_props <- ggplot(marking_presence_order_counts, aes(x = mark, y = proportion, fill = Sex)) +
  scale_fill_manual(values = c("turquoise3","palegreen3"))+
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(aes(ymin = proportion - SE, ymax = proportion + SE),
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  labs(x = "Mark", y = "Proportion") +
  theme_minimal()+
  theme(legend.text = element_text(size = 15),  # Adjust legend text size
        legend.title = element_text(size =15),
        axis.title = element_text(size = 16),   # Adjust axis title size
        axis.text = element_text(size = 13),    # Adjust axis text size
        axis.text.x = element_text(size = 15),  # Adjust x-axis text size
        axis.text.y = element_text(size = 15))
marking_props

#print final plot: 

png("Figures/Proportions of Marks Female Male.png", width = 11, height = 7, res= 300, units = "in")
marking_props
dev.off()

#end
