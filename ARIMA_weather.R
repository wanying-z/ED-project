setwd("~/Desktop/ED_project")

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)

#Load ED census data
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


#Load weather data
weather = read_xlsx("ED_Model_Weather_Calendar_2021_2026.xlsx",
                    sheet = "Daily Data")
weather$date = as.Date(weather$date)

#Join data
df = df %>%
  left_join(weather, by = join_by(Full_Date == date))


#Before, during, after a holiday
df <- df %>%
  arrange(Full_Date) %>%
  mutate(
    holiday_lag1  = lag(is_federal_holiday, 1),
    holiday_lead1 = lead(is_federal_holiday, 1)
  )
df$holiday_lag1[1] <- 0
df$holiday_lead1[nrow(df)] <- 0

df$near_holiday <- df$is_federal_holiday==1 | df$holiday_lead1==1

df <- df %>%
  mutate(
    any_event = as.integer(
      is_federal_holiday == 1 |
        holiday_lead1 == 1 |
        masters_week == 1 |
        resident_transition_july == 1
    )
  )
##############################################################################
##############################################################################
#################### Testing holidays ##########################
############################ Daily Forecast ##################################
train_start = as.Date("2023-01-01")
test_start = as.Date("2026-04-01")
test_end = as.Date("2026-08-16")
df_test = df %>%
  filter(Full_Date >= test_start & Full_Date <= test_end)
#Covariates for all data
xreg_all = model.matrix(~ factor(Year) + factor(Day_of_Week) 
                        + any_event
                        , df)[, -1, drop = FALSE]
colnames(xreg_all)[1:9] = c("Year2024", "Year2025", "Year2026",
                       "Monday", "Tuesday", "Wednesday",
                       "Thursday", "Friday", "Saturday"
)

#Store predicted values in pred_results
pred_results = data.frame(
  Full_Date = df_test$Full_Date,
  Total_Patients = df_test$Total_Patients,
  Pred_m1 = NA,
  Pred_m2 = NA
)

for(i in 1:nrow(df_test)){
  
  #Forecast date
  fc_date = df_test$Full_Date[i]
  new_idx <- which(df$Full_Date == fc_date)
  #print(paste("Forcast date:", fc_date))
  
  #Expanding training window
  train_idx <- which(
    df$Full_Date >= train_start &
      df$Full_Date <= (fc_date - 1)
  )
  
  
  # ARMA(1,1) + year + day-of-week
  m1 <- arima(
    df$Total_Patients[train_idx],
    order = c(1, 0, 1),
    xreg = xreg_all[train_idx, 1:8],
    include.mean = TRUE,
    method = "ML"
  )
  pred_m1 = predict(m1, n.ahead = 1, 
                    newxreg = xreg_all[new_idx, 1:8, drop = FALSE])
  pred_results[i, "Pred_m1"] = pred_m1$pred
  
  # ARMA(1,1) + year + day-of-week + holiday with lags
  m2 <- arima(
    df$Total_Patients[train_idx],
    order = c(1, 0, 1),
    xreg = xreg_all[train_idx, -9],
    include.mean = TRUE,
    method = "ML"
  )
  pred_m2 = predict(m2, n.ahead = 1,
                    newxreg = xreg_all[new_idx, -9, drop = FALSE])
  pred_results[i, "Pred_m2"] = pred_m2$pred
  
}

c(m1$aic, m2$aic)
View(cbind(m1$coef, sqrt(diag(m1$var.coef))))
View(cbind(m2$coef, sqrt(diag(m2$var.coef))))

#Evaluation
m_eval = data.frame(
  row.names = c("M1", "M2"),
  Delta = c(NA, NA),
  MAE = c(NA, NA),
  RMSE = c(NA, NA)
)

m_eval[,"Delta"] =
  c(sum(pred_results$Pred_m1)/sum(pred_results$Total_Patients) - 1,
    sum(pred_results$Pred_m2)/sum(pred_results$Total_Patients) - 1)

m_eval[,"MAE"] = 
  c(mean(abs(pred_results$Pred_m1 - pred_results$Total_Patients)),
    mean(abs(pred_results$Pred_m2 - pred_results$Total_Patients)))

m_eval[,"RMSE"] = 
  c(sqrt(mean((pred_results$Pred_m1 - pred_results$Total_Patients)^2)),
    sqrt(mean((pred_results$Pred_m2 - pred_results$Total_Patients)^2)))

m_eval



##############################################################################
##############################################################################
#################### Testing weather variables ##########################
############################ Daily Forecast ##################################
#Missing values
weather_vars <- c(
  "tmax_f",
  "tmin_f",
  "tavg_f",
  "prcp_in",
  "wind_avg_mph",
  "wind_gust_mph",
  "wx_thunderstorm",
  "wx_fog",
  "wx_ice_glaze",
  "wx_smoke_haze",
  "extreme_heat_100",
  "hot_day_95",
  "freeze_day",
  "hard_freeze_25",
  "heavy_rain_1in",
  "extreme_rain_2.5in",
  "high_wind_gust_35"
)

table(df$weather_data_missing, useNA = "ifany")

data.frame(
  Variable = weather_vars,
  N_missing = colSums(is.na(df[, weather_vars])),
  Percent_missing = round(
    colMeans(is.na(df[, weather_vars])) * 100,
    2
  )
)

df <- df %>%
  mutate(
    any_weather_NA = as.integer(
      !complete.cases(across(all_of(weather_vars)))
    )
  )


weather_vars_derived <- c(
  "extreme_heat_100",
  "hot_day_95",
  "freeze_day",
  "hard_freeze_25",
  "heavy_rain_1in",
  "extreme_rain_2.5in",
  "high_wind_gust_35"
)
weather_vars_wx <- c(
  "wx_thunderstorm",
  "wx_fog",
  "wx_ice_glaze",
  "wx_smoke_haze"
)
weather_vars_cont <- c(
  "tmax_f",
  "tmin_f",
  "tavg_f",
  "prcp_in",
  "wind_avg_mph",
  "wind_gust_mph"
)

train_start = as.Date("2024-01-01")
test_start = as.Date("2026-04-01")
test_end = as.Date("2026-08-16")
df_test = df %>%
  filter(Full_Date >= test_start & Full_Date <= test_end)
#Weather covariates for all data: low frequency binaries are not considered
#Impute missing binaries with 0
xreg_all = model.matrix(~ factor(Year) + factor(Day_of_Week)
                        + factor(Month), df)[, -1, drop = FALSE]
xreg_all = cbind(xreg_all, df[, c("freeze_day","wx_fog",
                                  "holiday_lag1",
                                  "is_federal_holiday",
                                  "holiday_lead1")])
xreg_all[is.na(xreg_all)] = 0
colnames(xreg_all)[1:9] = c("Year2024", "Year2025", "Year2026",
                            "Monday", "Tuesday", "Wednesday",
                            "Thursday", "Friday", "Saturday"
)
colnames(xreg_all)[10:20] = paste0("Month", 2:12)

#Store predicted values in pred_results
pred_results = data.frame(
  Full_Date = df_test$Full_Date,
  Total_Patients = df_test$Total_Patients,
  Pred_m1 = NA,
  Pred_m2 = NA
)

for(i in 1:nrow(df_test)){
  
  #Forecast date
  fc_date = df_test$Full_Date[i]
  new_idx <- which(df$Full_Date == fc_date)
  #print(paste("Forcast date:", fc_date))
  
  #Expanding training window
  train_idx <- which(
    df$Full_Date >= train_start &
      df$Full_Date <= (fc_date - 1)
  )
  
  
  # ARMA(1,1) + year + day-of-week
  m1 <- arima(
    df$Total_Patients[train_idx],
    order = c(1, 0, 1),
    xreg = xreg_all[train_idx, c(3:8, 14:15)],
    include.mean = TRUE,
    method = "ML"
  )
  pred_m1 = predict(m1, n.ahead = 1,
                    newxreg = xreg_all[new_idx, c(3:8, 14:15), drop = FALSE])
  pred_results[i, "Pred_m1"] = pred_m1$pred

  # ARMA(1,1) + year + day-of-week + weather
  m2 <- arima(
    df$Total_Patients[train_idx],
    order = c(1, 0, 1),
    xreg = xreg_all[train_idx, c(3:8, 14:15, 21:22, 25)],
    include.mean = TRUE,
    method = "ML"
  )
  pred_m2 = predict(m2, n.ahead = 1,
                    newxreg = xreg_all[new_idx, c(3:8, 14:15, 21:22, 25), drop = FALSE])
  pred_results[i, "Pred_m2"] = pred_m2$pred
  
}

c(m1$aic, m2$aic)
View(cbind(m1$coef, sqrt(diag(m1$var.coef))))
View(cbind(m2$coef, sqrt(diag(m2$var.coef))))

#Evaluation
m_eval = data.frame(
  row.names = c("M1", "M2"),
  Delta = c(NA, NA),
  MAE = c(NA, NA),
  RMSE = c(NA, NA)
)

m_eval[,"Delta"] =
  c(sum(pred_results$Pred_m1)/sum(pred_results$Total_Patients) - 1,
    sum(pred_results$Pred_m2)/sum(pred_results$Total_Patients) - 1)

m_eval[,"MAE"] = 
  c(mean(abs(pred_results$Pred_m1 - pred_results$Total_Patients)),
    mean(abs(pred_results$Pred_m2 - pred_results$Total_Patients)))

m_eval[,"RMSE"] = 
  c(sqrt(mean((pred_results$Pred_m1 - pred_results$Total_Patients)^2)),
    sqrt(mean((pred_results$Pred_m2 - pred_results$Total_Patients)^2)))

m_eval
