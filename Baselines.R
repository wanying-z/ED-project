setwd("~/Desktop/ED_project")

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)

df = read_xlsx("ED_Daily_Forecast_Final2.xlsx",
               sheet = "Yearly Raw - Daily Census (DS)")
df$Full_Date = date(df$Full_Date)

#2024-02-29 data (missed in this file)
df_240229 = data.frame(Year = 2024, Month = 2, Date = 29,
                       Full_Date = as.Date("2024-02-29"),
                       Day_of_Week = wday(as.Date("2024-02-29"), week_start = 7),
                       Total_Patients = 231
)
df = rbind(df, df_240229)

#Add 2026 Aug data
df_26_Aug = data.frame(Year = 2026, Month = 8, Date = 1:16,
                       Full_Date = seq(as.Date("2026-08-01"), as.Date("2026-08-16"), by = "day")
)
df_26_Aug["Day_of_Week"] = wday(df_26_Aug$Full_Date, week_start = 7)
df_26_Aug["Total_Patients"] = c(196, 174, 217, 213, 221, 198, 217, 191, 223, 206,
                                228, 240, 214, 233, 192, 220)
df = rbind(df, df_26_Aug)

#Six zero values
df$Total_Patients_Raw = df$Total_Patients
View(df[df$Total_Patients == 0,])
df$Total_Patients[df$Total_Patients==0] = NA

#Make sure data are ordered
df = df %>%
  arrange(Full_Date)


#############################################################################
#############################################################################
######################### Use data cleaned by Mohit #########################
df = read_xlsx("ed_daily_model_dataset.xlsx",
                sheet = "Model Data")
df$date = date(df$date)
df$month_name = factor(
  df$month_name,
  levels = month.name
)


test_start = as.Date("2026-06-01")
test_end = as.Date("2026-08-16")



#############################################################################
#############################################################################
################### Seasonal Naive (same day last week) #####################
df_test = df %>%
  filter(date >= test_start & date <= test_end) %>%
  select(date, year, day_name, total_census)

df_test = df_test %>%
  mutate(
    ref_date = date - 7
  ) %>%
  left_join(df %>%
              select(date, total_census, day_name) %>%
              rename(ref_census = total_census),
            by = join_by(ref_date == date, day_name)
            )

mean(abs(df_test$ref_census - df_test$total_census))
sqrt(mean((df_test$ref_census - df_test$total_census)^2))



#############################################################################
#############################################################################
######################### Two week moving average ###########################
df_test = df %>%
  filter(date >= test_start & date <= test_end) %>%
  select(date, year, day_name, total_census) %>%
  mutate(census_ma = NA)

for (i in 1:nrow(df_test)) {
  
  current_date <- df_test$date[i]
  
  previous <- df %>%
    select(date, total_census) %>%
    filter(
      date < current_date,
      date >= current_date - 14
    )
  
  df_test$census_ma[i] <- mean(
    previous$total_census,
    na.rm = TRUE
  )
}
remove(previous)

mean(abs(df_test$census_ma - df_test$total_census))
sqrt(mean((df_test$census_ma - df_test$total_census)^2))
