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

#Impute zero values using the median of previous 4 same weekdays
for (i in which(is.na(df$Total_Patients))) {
  
  current_date <- df$Full_Date[i]
  current_dow  <- df$Day_of_Week[i]
  
  previous <- df %>%
    filter(
      Day_of_Week == current_dow,
      Full_Date < current_date,
      Full_Date >= current_date - 28,
      !is.na(Total_Patients)
    )
  
  df$Total_Patients[i] <- median(
    previous$Total_Patients,
    na.rm = TRUE
  )
}

train_start = as.Date("2025-01-01")
train_end = as.Date("2026-05-31")
test_start = as.Date("2026-06-01")
test_end = as.Date("2026-08-16")

df_train = df %>%
  filter(Full_Date >= train_start & Full_Date <= train_end)

df_test = df %>%
  filter(Full_Date >= test_start & Full_Date <= test_end)

yearly_pivot = df_train %>%
  group_by(Month, Year) %>%
  summarise(
    Total_Patients = sum(Total_Patients, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Year,
    names_prefix = "Year",
    values_from = Total_Patients
  ) %>%
  mutate(RltChg_Patients = Year2026/Year2025 -1)

yearly_pivot
mean_rltchg = mean(yearly_pivot$RltChg_Patients, na.rm = T)
sd_rltchg = sd(yearly_pivot$RltChg_Patients, na.rm = T)

yearly_avg_chg = yearly_pivot %>%
  select(RltChg_Patients) %>%
  filter(!is.na(RltChg_Patients)) %>%
  filter(RltChg_Patients > (mean_rltchg-sd_rltchg) 
         & RltChg_Patients < (mean_rltchg+sd_rltchg)) %>%
  summarise(mean(RltChg_Patients))
yearly_avg_chg = as.numeric(yearly_avg_chg)

monthly_pivot = df_train %>%
  group_by(Day_of_Week, Month) %>%
  summarise(
    Total_Patients = sum(Total_Patients, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Month) %>%
  mutate(
    Proportion = Total_Patients / sum(Total_Patients)
  ) %>%
  ungroup()

monthly_pivot

calendar_count <- data.frame(
  Full_Date = seq(
    as.Date("2026-01-01"),
    as.Date("2026-12-31"),
    by = "day"
  )
) %>%
  mutate(
    Year = year(Full_Date),
    Month = month(Full_Date),
    Day_of_Week = wday(Full_Date, week_start = 7)
  ) %>%
  count(Year, Month, Day_of_Week, name = "Count")

df_test <- df_test %>%
  left_join(
    calendar_count,
    by = c("Year", "Month", "Day_of_Week")
  )

df_test = df_test %>%
  left_join(yearly_pivot[, c("Month", "Year2025")], by = "Month") %>%
  left_join(monthly_pivot[, c("Month", "Day_of_Week", "Proportion")], 
            by = c("Month", "Day_of_Week"))

#Projection
df_test = df_test %>%
  mutate(Pred = Year2025 * (1 + yearly_avg_chg) * Proportion / Count)

#Evaluation
##Delta
sum(df_test$Pred)/sum(df_test$Total_Patients) - 1
##MAE
mean(abs(df_test$Pred - df_test$Total_Patients))
##RMSE
sqrt(mean((df_test$Pred - df_test$Total_Patients)^2))


#Plot
plot(df_test$Full_Date, df_test$Total_Patients, type="b", pch = 20,
     ylim = c(155, 300))
points(df_test$Full_Date, df_test$Pred, pch = 20, col = "blue", type = "b")
