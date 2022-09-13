
install.packages("tidyverse", repos="http://cran.us.r-project.org")
library(tidyverse)

activity_data = read_csv("Fitabase_Data_4_12_16-5_12_16/numOfDaysForActivityTrack.csv")
sleep_data = read_csv("Fitabase_Data_4_12_16-5_12_16/numOfDaysForSleepTrack.csv")
data_merged = merge(x = activity_data, y = sleep_data, by = "ID", all = TRUE)
data_merged = mutate(data_merged, char_ID = as.character(data_merged$ID))
data_merged[is.na(data_merged)] <- 0
head(data_merged)

long_data = data.frame(Participant =  c(data_merged$char_ID, data_merged$char_ID), Purpose = rep(c('Activity', 'Sleep'), each = length(data_merged$ID)), NumOfDays = c(data_merged$NumOfActiveDays, data_merged$NumOfSleepDays))
head(long_data)

sum_data = data.frame(Purpose = c("Activity","Sleep"), Average = c(mean(data_merged$NumOfActiveDays), mean(data_merged$NumOfSleepDays)), SE = c(sd(data_merged$NumOfActiveDays)/sqrt(length(data_merged$ID)), sd(data_merged$NumOfSleepDays)/sqrt(length(data_merged$ID))))
sum_data

ggplot(long_data, aes(y = Participant, x = NumOfDays, fill=Purpose)) + 
  geom_bar(position = 'dodge', stat = "identity", width = 0.8) +
  labs(title="The numbers of days used for tracking for each individual", y = "Participants", x = "The number of days") 

ggplot(sum_data, aes(x=Purpose, y=Average)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_errorbar(aes(ymin=Average-SE, ymax=Average+SE), width=.1, position=position_dodge(.9)) +
  labs(title="Averaged number of days used for tracking across participants", x="Purpose", y="Averaged number of days")

t.test(data_merged$NumOfActiveDays, data_merged$NumOfSleepDays, paired=TRUE)

#r = cor(data_merged$NumOfActiveDays, data_merged$NumOfSleepDays)
#ggplot(data_merged, mapping=aes(x=NumOfActiveDays, y=NumOfSleepDays)) + geom_point() +
#  geom_smooth(method="lm") + 
#  labs(title="Association between the numbers of days used for tracking activity and sleep", x="The number of days #(Activity)", y = "The number of days (Sleep)") +
#  annotate(geom = "text", x = 4, y = 31, label=sprintf("R = %.2f", r))

activity_merged = read_csv("Fitabase_Data_4_12_16-5_12_16/activityLogPercentage.csv")
head(activity_merged)

activity_wide = summarize(activity_merged, ID, LightlyActivePerc, FairlyActivePerc, VeryActivePerc)
activity_long = gather(activity_wide, Activity, Percentage, -ID)
activity_long = mutate(activity_long, char_ID = as.character(activity_long$ID))
head(activity_long)

ggplot(activity_long, aes(y = char_ID, x = Percentage, fill=Activity)) + 
  geom_col(position="fill", stat="identity") +
  #theme(axis.text.x = element_text(angle=90)) + 
  labs(title = "", y = "Participants", x = "Active duration (%)") +
  scale_fill_discrete("", labels = c('Fairly Active', 'Light Active', 'Very Active'))
  #scale_fill_manual("", values = c('FairlyActivePerc'='blue', 'LightlyActivePerc'='red', 'VeryActivePerc'='green'), labels = c('Fairly Active', 'Light Active', 'Very Active'))

activity_sum = data.frame(Purpose = c("LightlyActive","FairlyActive", "VeryActive"), Average = c(mean(activity_merged$LightlyActiveMinutesTotal), mean(activity_merged$FairlyActiveMinutesTotal), mean(activity_merged$VeryActiveMinutesTotal)), SE = c(sd(activity_merged$LightlyActiveMinutesTotal)/sqrt(length(activity_merged$ID)), sd(activity_merged$FairlyActiveMinutesTotal)/sqrt(length(activity_merged$ID)), sd(activity_merged$VeryActiveMinutesTotal)/sqrt(length(activity_merged$ID))))
activity_sum

ggplot(activity_sum, aes(x=Purpose, y=Average)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_errorbar(aes(ymin=Average-SE, ymax=Average+SE), width=.1, position=position_dodge(.9)) +
  labs(title="Averaged usage for tracking activity in 30 days across participants", x="Purpose", y="Averaged active duration [min]")

active_min_wide = summarize(activity_merged, ID, LightlyActiveMinutesTotal, FairlyActiveMinutesTotal, VeryActiveMinutesTotal)
active_min_long = gather(active_min_wide, Activity, Duration, -ID)
active_min_long = mutate(active_min_long, char_ID = as.character(active_min_long$ID))
head(active_min_long)

one.way <- aov(Duration ~ Activity, data = active_min_long)
summary(one.way)
