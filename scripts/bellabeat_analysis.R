# Bellabeat User Behavior Analysis
# Prepared by: [Udo Chiemerie]
# Tools: R,tidyverse, here, skimr, janitor, lubridate

# 1. Load Libraries 
library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(dplyr)
library(lubridate)

# 2. Load Data 
daily_activity <- read_csv("dailyActivity_merged.csv")
hourly_steps <- read_csv("hourlySteps_merged.csv")
sleep_day <- read_csv("sleepDay_merged.csv")

# 3. Data Cleaning 
# Remove duplicates
daily_activity <- distinct(daily_activity)
sleep_day <- distinct(sleep_day)
hourly_steps <- distinct(hourly_steps)

# Format Dates
daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate, format = "%m/%d/%Y")

sleep_day$SleepDay <- as.POSIXct(sleep_day$SleepDay, format = "%m/%d/%Y %I:%M:%S %p")
sleep_day$SleepDay <- as.Date(sleep_day$SleepDay)

hourly_steps$ActivityHour <- as.POSIXct(hourly_steps$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")
hourly_steps$Hour <- hour(hourly_steps$ActivityHour)
hourly_steps$Date <- as.Date(hourly_steps$ActivityHour)

# 4. Analysis & Visualizations 

# Trend 1: Steps vs Calories
cor(daily_activity$TotalSteps, daily_activity$Calories)

ggplot(daily_activity, aes(x = TotalSteps, y = Calories)) +
  geom_jitter(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Positive Correlation: More Steps Lead to More Calories Burned",
       x = "Total Steps", y = "Calories Burned") +
  theme_minimal()

# Trend 2: Sedentary Time
daily_activity %>%
  summarise(
    Sedentary = mean(SedentaryMinutes) / 60,
    Light_Activity = mean(LightlyActiveMinutes) / 60,
    Moderate_Activity = mean(FairlyActiveMinutes) / 60,
    Intense_Activity = mean(VeryActiveMinutes) / 60
  ) %>%
  pivot_longer(everything(), names_to = "Activity", values_to = "Hours") %>%
  ggplot(aes(x = Activity, y = Hours, fill = Activity)) +
  geom_col() +
  labs(title = "Average hours per day by activity level", y = "Hours") +
  theme_minimal()

# Trend 3: Sleep Deficiency (44% of nights under 7 hours)
ggplot(sleep_day, aes(x = TotalMinutesAsleep / 60)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 7, color = "red", linetype = "dashed") +
  labs(title = "Sleep Duration Distribution", x = "Hours of Sleep") +
  theme_minimal()

# Trend 4: Peak Activity Hours
hourly_summary <- hourly_steps %>%
  group_by(Hour) %>%
  summarise(Avg_Steps = mean(StepTotal))

ggplot(hourly_summary, aes(x = Hour, y = Avg_Steps)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Peak Activity Hours", x = "Hour of Day", y = "Avg Steps") +
  theme_minimal()
