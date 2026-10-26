setwd("~/Desktop/ED_project")

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(timeDate)
library(stringr)

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
df$day_name = factor(df$day_name, levels = 
                       c("Sunday", "Monday", "Tuesday", 
                         "Wednesday", "Thursday", "Friday", "Saturday"))

#Visualize variable of interest by month
yearly_pivot = df %>%
  group_by(month_name, year) %>%
  summarise(
    bed_holds = sum(bed_holds, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(yearly_pivot, 
       aes(month_name, bed_holds, colour = factor(year), group = year)) + geom_line() + geom_point()

#Visualize variable of interest by day of week
monthly_pivot = df %>%
  group_by(day_name, month_name) %>%
  summarise(
    bed_holds = sum(bed_holds, na.rm = TRUE),
    .groups = "drop"
    ) %>%
  group_by(month_name) %>%
  ungroup()

ggplot(monthly_pivot, 
       aes(day_name, bed_holds, colour = month_name, group = month_name)) 
+ geom_line() + geom_point()


#############################################################################
#############################################################################
######################### Weather data - holidays #########################
weather = read_xlsx("ED_Model_Weather_Calendar_2021_2026.xlsx",
                    sheet = "Daily Data")
weather$date = as.Date(weather$date)

holidays = unique(weather$holiday_name[!is.na(weather$holiday_name)])
holidays_unique = holidays[!str_detect(holidays, "observed")]
names(holidays_unique) = listHolidays(pattern = "US")[c(10, 3, 17, 16, 2, 
                                                        14, 12, 18, 13, 9, 8)]
get_holiday_df <- function(start_day, end_day, holidays){
  #holidays: Named vectors. Names is a subset of listHolidays in package "timeDate";
  #          Values are holiday names used in the weather dataset.
  
  #Initial data frame for holidays
  yr1 = year(start_day)
  yr2 = year(end_day)
  df = holiday(yr1:yr2, Holiday = names(holidays), names = T)
  df = data.frame(date = as.Date(df), holiday_name = holidays[names(df)])
  
  #Change the date of Washington's Birthday holiday to the third Monday in Feb.
  df <- df %>%
    mutate(
      date = if_else(
        holiday_name == "Washington's Birthday",
        as.Date(paste0(year(date), "-02-01")) +
          (8 - wday(as.Date(paste0(year(date), "-02-01")),
                    week_start = 1)) %% 7 + 14,
        date
      )
    ) %>%
    arrange(date)
  
  #Add observed holidays
  observed_holidays <- df %>%
    mutate(
      date = case_when(
        wday(date, week_start = 1) == 6 ~ date - 1,
        wday(date, week_start = 1) == 7 ~ date + 1,
        TRUE ~ date
      ),
      holiday_name = paste0(holiday_name, " (observed)")
    ) %>%
    filter(!date %in% df$date)
  
  df <- bind_rows(
    df,
    observed_holidays
  ) %>%
    arrange(date)
  
  #Output
  df$dow_name = weekdays(df$date)
  df$year = year(df$date); df$month = month(df$date); df$day = day(df$date)
  df = df %>% filter(date >= start_day & date <= end_day)
  rownames(df) = NULL
  return(df)
}

#get_holiday_df(start_day, end_day, holidays_unique)
#############################################################################
#############################################################################
############################## CNO & KPI Data ###############################
cno = read_xlsx("ED_Operations_2026_YTD_DAILY_CNO_REBUILT.xlsx",
                sheet = "Master Daily View")
cno$Date = date(cno$Date)
cno$Day = factor(cno$Day, levels = 
                       c("Sunday", "Monday", "Tuesday", 
                         "Wednesday", "Thursday", "Friday", "Saturday"))

#Rename columns
colnames(cno) = c("date", "day_name", "CNO_status", "CNO_total_census",
                  "adult_census", "CHOG_census", "bed_holds", "NEDOC", 
                  "patients_bedded", "discharges", "KPI_status", 
                  "total_census", "acuity", "arrival_triage", "arrival_physician",
                  "arrival_disposition", "LOS", "LWBS", "note")
apply(cno, 2, function(x) sum(is.na(x)))

#Add total_census in df (ED daily census) from EPIC
cno = cno %>%
  left_join(df %>%
              select(date, day_name, total_census) %>%
              rename(total_census_df = total_census),
            by = c("date", "day_name"))

#Check census data
id = which(!is.na(cno$total_census) & cno$date <= as.Date("2026-08-18"))
table(cno$total_census[id] == cno$total_census_df[id]) #all TRUE
remove(id)

#Fill in missing total_census using total_census_df
id = which(is.na(cno$total_census) & cno$date <= as.Date("2026-08-18"))
cno$total_census[id] = cno$total_census_df[id]
cno = cno %>% select(!total_census_df)
remove(id)

#Add bed_holds in df (ED daily census) from EPIC
cno = cno %>%
  left_join(df %>%
              select(date, day_name, bed_holds) %>%
              rename(bed_holds_df = bed_holds),
            by = c("date", "day_name"))

#Check bed holds data
id = which(!is.na(cno$bed_holds) & cno$date <= as.Date("2026-08-18"))
table(cno$bed_holds[id] == cno$bed_holds_df[id]) #one FALSE
id = which(!is.na(cno$bed_holds) & cno$date <= as.Date("2026-08-18")
           & !is.na(cno$bed_holds_df)
           & cno$bed_holds != cno$bed_holds_df)
View(cno[id,]) #2026-04-09
remove(id)
cno$bed_holds[id] = cno$bed_holds_df[id]

#Fill in missing bed_holds using bed_holds_df
id = which(is.na(cno$bed_holds) & cno$date <= as.Date("2026-08-16"))
cno$bed_holds[id] = cno$bed_holds_df[id]
cno = cno %>% select(!bed_holds_df)
remove(id)

#Correlations
vars = c("total_census", "bed_holds", "NEDOC", "patients_bedded", 
         "discharges", "acuity", "arrival_triage", "arrival_physician", 
         "arrival_disposition", "LOS", "LWBS")
cor(cno[, vars], use = "pairwise.complete.obs")

#Create lagged covariates
cno <- cno %>%
  arrange(date) %>%
  mutate(
    total_census_lag1 = lag(total_census, 1),
    bed_holds_lag1 = lag(bed_holds, 1),
    patients_bedded_lag1 = lag(patients_bedded, 1),
    discharges_lag1 = lag(discharges, 1),
    acuity_lag1 = lag(acuity, 1),
    CNO_census_lag1 = lag(CNO_total_census, 1),
    LOS_lag1 = lag(LOS, 1),
    LWBS_lag1 = lag(LWBS, 1)
  )
cno$total_census_lag1[1] = df$total_census[df$date == as.Date("2025-12-31")]
cno$bed_holds_lag1[1] = df$bed_holds[df$date == as.Date("2025-12-31")]

apply(cno, 2, function(x) sum(is.na(x)))

#Correlations with lagged variables
vars = c("total_census", "total_census_lag1", "bed_holds_lag1", 
         "patients_bedded_lag1", "discharges_lag1", "acuity_lag1", 
         "CNO_census_lag1", "LOS_lag1", "LWBS_lag1")
cor(cno[, vars], use = "pairwise.complete.obs")
