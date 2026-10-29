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





######################################################################################
######################################################################################
############ Predict daily census until 2027-05-01 ###############
last_date <- cno$date[nrow(cno)]
end_date  <- as.Date("2027-05-01")

future_dates <- seq(
  from = last_date + 1,
  to = end_date - 1,
  by = "day"
)

h <- length(future_dates)
h

#Future holidays
holidays_future = get_holiday_df(start_day = last_date, 
                                 end_day = end_date, holidays_unique)

# *Observed* future holidays
holidays_future =  holidays_future[!holidays_future$dow_name %in% 
                                     c("Saturday", "Sunday"), ]
#Future design matrix
future <- data.frame(date = future_dates) %>%
  mutate(
    day_name = factor(
      weekdays(date),
      levels = c(
        "Sunday", "Monday", "Tuesday",
        "Wednesday", "Thursday",
        "Friday", "Saturday"
      )
    ),
    
    is_federal_holiday = as.integer(date %in% holidays_future$date),
    
    holiday_lag1 = as.integer(
      (date - 1) %in% holidays_future$date
    ),
    
    holiday_lead1 = as.integer(
      (date + 1) %in% holidays_future$date
    )
  )

#Note that future days to be predicted don't include June or July, which are 
#predictors in the model
xreg_future <- model.matrix(
  ~ day_name +
    is_federal_holiday +
    holiday_lag1 +
    holiday_lead1,
  data = future
)[, -1, drop = FALSE]
colnames(xreg_future)[1:6] = c("Monday", "Tuesday", "Wednesday",
                               "Thursday", "Friday", "Saturday"
)
xreg_future = as.data.frame(xreg_future)
xreg_future$Month6 = 0
xreg_future$Month7 = 0
xreg_future = xreg_future[, c(1:6, 10:11, 7:9)]
colSums(is.na(xreg_future))
xreg_future = data.frame(Year2024 = 0, Year2025 = 0, Year2026 = 1,
                            xreg_future)

#Combine df and CNO daily census
df = df %>%
  full_join(cno %>%
              select(date, day_name, total_census) %>%
              rename(total_census_cno = total_census),
            by = c("date", "day_name"))
df$year = year(df$date)
df$month_name = month(df$date)
df$month_name = factor(df$month_name, labels = month.name)


#Check census data
id = which(!is.na(df$total_census) & df$date >= as.Date("2026-01-01")
           & df$date <= as.Date("2026-08-18"))
table(df$total_census[id] == df$total_census_cno[id], useNA = "ifany") #all TRUE except missing total_census_cno
remove(id)

#Append total_census after 2026-08-18 using CNO data
id = which(df$date > as.Date("2026-08-18"))
df$total_census[id] = df$total_census_cno[id]
df = df %>% select(!total_census_cno)
remove(id)

#Observed federal holiday for training data
holidays_df = get_holiday_df(start_day = df$date[1], 
                                 end_day = df$date[nrow(df)], holidays_unique)

# *Observed* future holidays
holidays_df =  holidays_df[!holidays_df$dow_name %in% 
                                     c("Saturday", "Sunday"), ]
df$is_federal_holiday_observed = as.integer(df$date %in% holidays_df$date)

#Training data design matrix (train from 2023)
xreg_all = model.matrix(~ factor(year) + day_name
                        + month_name, df)[, -1, drop = FALSE]
colnames(xreg_all)[1:11] = c("Year2022", "Year2023",
                            "Year2024", "Year2025", "Year2026",
                            "Monday", "Tuesday", "Wednesday",
                            "Thursday", "Friday", "Saturday"
)
colnames(xreg_all)[12:22] = paste0("Month", 2:12)
xreg_all = cbind(xreg_all, df[, "is_federal_holiday_observed"])
colnames(xreg_all)[23] = "is_federal_holiday"
xreg_all$holiday_lag1 = lag(xreg_all$is_federal_holiday, 1)
xreg_all$holiday_lag1[1] = 0
xreg_all$holiday_lead1 = lead(xreg_all$is_federal_holiday, 1)
xreg_all$holiday_lead1[nrow(xreg_all)] = 0

vars = c("Year2022", "Year2023", paste0("Month", 2:5), paste0("Month", 8:12))
vars_id = which(names(xreg_all) %in% vars)
xreg_all = xreg_all[, -vars_id]
colSums(is.na(xreg_all))

#Train model
train_idx = which(df$date >= as.Date("2023-01-01"))
fit0_census <- arima(
  df$total_census[train_idx],
  order = c(1, 0, 1),
  xreg = xreg_all[train_idx, -9], #Combine Saturday and Sunday
  include.mean = TRUE,
  method = "ML"
)
summary(fit0_census)

fc_census <- predict(
  fit0_census,
  n.ahead = h,
  newxreg = xreg_future[, -9]
)

# Forecast through April 30, 2027
forecast_results_census <- data.frame(
  date = future_dates,
  predicted_ed_census = as.numeric(fc_census$pred),
  lower95 = as.numeric(fc_census$pred - 1.96 * fc_census$se),
  upper95 = as.numeric(fc_census$pred + 1.96 * fc_census$se)
)

#Plot
ggplot(forecast_results_census,
       aes(x = date, y = predicted_ed_census)) +
  geom_ribbon(
    aes(ymin = lower95, ymax = upper95),
    alpha = 0.2
  ) +
  geom_line(color = "blue", linewidth = 0.7) +
  labs(
    title = "ED Census Forecast",
    subtitle = "October 6, 2026 – April 30, 2027",
    x = "Date",
    y = "Predicted ED Census"
  ) +
  theme_minimal()

write.csv(forecast_results_census, file = "Forecast_ed_census_wz.csv", row.names = F)

pred_results = data.frame(
  date = df_test$date,
  total_census = df_test$total_census,
  pred_m1 = NA,
  pred_m2 = NA
)
for(i in 1:nrow(df_test)){
  
  #Forecast date
  fc_date = df_test$date[i]
  new_idx <- which(df$date == fc_date)
  #print(paste("Forcast date:", fc_date))
  
  #Expanding training window
  train_idx <- which(
    df$date >= train_start &
      df$date <= (fc_date - 1)
  )
  
  
  # ARMA(1,1) + year + day-of-week + M6 + M7 + holiday
  m1 <- arima(
    df$total_census[train_idx],
    order = c(1, 0, 1),
    xreg = xreg_all[train_idx, -9],
    include.mean = TRUE,
    method = "ML"
  )
  pred_m1 = predict(m1, n.ahead = 1, 
                    newxreg = xreg_all[new_idx, -9, drop = FALSE])
  pred_results[i, "pred_m1"] = pred_m1$pred
  
  # ARMA(1,1) + year + day-of-week + holiday + holiday_lead1
  m2 <- arima(
    df$total_census[train_idx],
    order = c(1, 0, 1),
    xreg = xreg_all[train_idx, -c(9, 12)],
    include.mean = TRUE,
    method = "ML"
  )
  pred_m2 = predict(m2, n.ahead = 1,
                    newxreg = xreg_all[new_idx, -c(9, 12), drop = FALSE])
  pred_results[i, "pred_m2"] = pred_m2$pred
  
}
c(m1$aic, m2$aic)
summary(m1); summary(m2)
#MAE
sapply(3:4, function(j) mean(abs(pred_results[, j] - pred_results[, 2]), na.rm = T))
#RMSE
sapply(3:4, function(j) sqrt(mean((pred_results[, j] - pred_results[, 2])^2, na.rm = T)))

#Plot
library(dplyr)
library(ggplot2)

# 1. Calculate forecast errors
pred_results <- pred_results %>%
  mutate(error = pred_m1 - total_census)

# 2. Calculate summary statistics
mean_error <- mean(pred_results$error, na.rm = TRUE)
sd_error   <- sd(pred_results$error, na.rm = TRUE)

Q1 <- quantile(pred_results$error, 0.25, na.rm = TRUE)
Q3 <- quantile(predict_results$error, 0.75, na.rm = TRUE)
IQR_error <- Q3 - Q1

# Mean +/- SD
lower_sd <- mean_error - sd_error
upper_sd <- mean_error + sd_error

# Tukey lower and upper fences
lower_fence <- Q1 - 1.5 * IQR_error
upper_fence <- Q3 + 1.5 * IQR_error

# 3. Plot
ggplot(pred_results, aes(x = date, y = error)) +
  
  # Daily forecast errors
  geom_line(color = "black", linewidth = 0.8) +
  
  # Zero reference line
  geom_hline(
    yintercept = 0,
    color = "gray50",
    linewidth = 0.6
  ) +
  
  # Mean error
  geom_hline(
    yintercept = mean_error,
    color = "blue",
    linewidth = 1
  ) +
  
  # Mean +/- SD
  geom_hline(
    yintercept = c(lower_sd, upper_sd),
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  # Lower and upper fences
  geom_hline(
    yintercept = c(lower_fence, upper_fence),
    color = "darkgreen",
    linetype = "dotted",
    linewidth = 0.9
  ) +
  
  labs(
    title = "Daily ED Census Forecast Errors",
    subtitle = paste0(
      "Mean = ", round(mean_error, 2),
      " | SD = ", round(sd_error, 2),
      " | IQR = ", round(IQR_error, 2)
    ),
    x = "Date",
    y = "Predicted - Actual"
  ) +
  
  theme_minimal(base_size = 13)
