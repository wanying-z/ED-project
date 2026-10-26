setwd("~/Desktop/ED_project")

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(xgboost)
library(DiagrammeR)

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

# df$near_holiday <- df$is_federal_holiday==1 | df$holiday_lead1==1
# 
# df <- df %>%
#   mutate(
#     any_event = as.integer(
#       is_federal_holiday == 1 |
#         holiday_lead1 == 1 |
#         masters_week == 1 |
#         resident_transition_july == 1
#     )
#   )

df_xgb <- df %>%
  arrange(Full_Date) %>%
  mutate(
    Year = factor(year(Full_Date)),
    
    dow = factor(dow_name,
      levels = c(
        "Sunday", "Monday", "Tuesday", "Wednesday",
        "Thursday", "Friday", "Saturday"
      )
    ),
    
    M6 = as.integer(Month == 6),
    M7 = as.integer(Month == 7),
    
    lag1 = lag(Total_Patients, 1),
    lag2 = lag(Total_Patients, 2),
    lag7 = lag(Total_Patients, 7)
  ) %>%
  filter(
    complete.cases(
      Total_Patients,
      lag1,
      lag2,
      lag7
    )
  )

df_xgb$wx_fog[is.na(df_xgb$wx_fog)] = 0

xreg_xgb <- model.matrix(
  ~ Year + dow +
    M6 + M7 + holiday_lead1 +
    freeze_day + wx_fog +
    lag1 + lag2 + lag7,
  data = df_xgb
)[, -1, drop = FALSE]
colnames(xreg_xgb)[4:9] = c("Monday", "Tuesday", "Wednesday",
                            "Thursday", "Friday", "Saturday"
)


#Hyperparameters selection
param_grid <- expand.grid(
  max_depth = c(2, 3, 4),
  learning_rate = c(0.03, 0.05, 0.1),
  min_child_weight = c(1, 5),
  subsample = c(0.8, 1.0),
  colsample_bytree = c(0.8, 1.0)
)

nrow(param_grid)
nrounds = 200
train_start <- as.Date("2023-01-01")
validation_start <- as.Date("2026-01-01")
validation_end   <- as.Date("2026-03-31")
test_start <- as.Date("2026-04-01")
test_end   <- as.Date("2026-08-16")


evaluate_xgb <- function(
    max_depth,
    learning_rate,
    min_child_weight,
    subsample,
    colsample_bytree) {
  
  validation_dates <- seq(
    validation_start,
    validation_end,
    by = "day"
  )
  
  actual <- numeric(length(validation_dates))
  pred   <- numeric(length(validation_dates))
  
  for (i in seq_along(validation_dates)) {
    
    fc_date <- validation_dates[i]
    
    # Expanding training window
    train_idx <- which(
      df_xgb$Full_Date >= train_start &
        df_xgb$Full_Date < fc_date
    )
    
    test_idx <- which(
      df_xgb$Full_Date == fc_date
    )
    
    x_train <- xreg_xgb[
      train_idx, ,
      drop = FALSE
    ]
    
    y_train <- df_xgb$Total_Patients[
      train_idx
    ]
    
    x_test <- xreg_xgb[
      test_idx, ,
      drop = FALSE
    ]
    
    fit <- xgboost(
      x = x_train,
      y = y_train,
      
      objective = "reg:squarederror",
      
      nrounds = 200,
      max_depth = max_depth,
      learning_rate = learning_rate,
      min_child_weight = min_child_weight,
      subsample = subsample,
      colsample_bytree = colsample_bytree,
      
      verbosity = 0
    )
    
    pred[i] <- predict(
      fit,
      x_test
    )
    
    actual[i] <- df_xgb$Total_Patients[
      test_idx
    ]
  }
  
  MAE <- mean(
    abs(actual - pred),
    na.rm = TRUE
  )
  
  RMSE <- sqrt(
    mean((actual - pred)^2, na.rm = TRUE)
  )
  
  return(
    c(
      MAE = MAE,
      RMSE = RMSE
    )
  )
}

tuning_results <- param_grid
tuning_results$MAE  <- NA_real_
tuning_results$RMSE <- NA_real_

for (j in seq_len(nrow(param_grid))) {
  
  cat(
    "Model",
    j,
    "of",
    nrow(param_grid),
    "\n"
  )
  
  result <- evaluate_xgb(
    max_depth =
      param_grid$max_depth[j],
    
    learning_rate =
      param_grid$learning_rate[j],
    
    min_child_weight =
      param_grid$min_child_weight[j],
    
    subsample =
      param_grid$subsample[j],
    
    colsample_bytree =
      param_grid$colsample_bytree[j]
  )
  
  tuning_results$MAE[j] <-
    result["MAE"]
  
  tuning_results$RMSE[j] <-
    result["RMSE"]
}

tuning_results <- tuning_results %>%
  arrange(MAE)

head(tuning_results, 10)
best_params <- tuning_results[1, ]
best_params



#Final prediction
train_start <- as.Date("2023-01-01")
test_start  <- as.Date("2026-04-01")
test_end    <- as.Date("2026-08-16")

train_idx <- which(
  df_xgb$Full_Date >= train_start &
    df_xgb$Full_Date < test_start
)

test_idx <- which(
  df_xgb$Full_Date >= test_start &
    df_xgb$Full_Date <= test_end
)

forecast_dates <- seq(test_start, test_end, by = "day")

results_xgb <- data.frame(
  Date = forecast_dates,
  Actual = NA_real_,
  Forecast = NA_real_
)

for (i in seq_along(forecast_dates)) {
  
  fc_date <- forecast_dates[i]
  
  # 1. Expanding training set: everything before forecast date
  train_idx <- which(
    df_xgb$Full_Date >= train_start &
      df_xgb$Full_Date < fc_date
  )
  
  # 2. Current forecast date
  test_idx <- which(df_xgb$Full_Date == fc_date)
  
  # 3. Training X and Y
  x_train <- xreg_xgb[train_idx, 
                      !colnames(xreg_xgb) %in% c("Saturday"), 
                      drop = FALSE]
  y_train <- df_xgb$Total_Patients[train_idx]
  
  # 4. Predictor values for forecast date
  x_test <- xreg_xgb[test_idx, 
                     !colnames(xreg_xgb) %in% c("Saturday"), 
                     drop = FALSE]
  
  # 5. Refit XGBoost
  fit_xgb <- xgboost(
    x = x_train,
    y = y_train,
    
    objective = "reg:squarederror",
    
    nrounds = 200,
    max_depth = tuning_results$max_depth[4],
    learning_rate = tuning_results$learning_rate[4],
    min_child_weight = tuning_results$min_child_weight[4],
    subsample = tuning_results$subsample[4],
    colsample_bytree = tuning_results$colsample_bytree[4],
    
    verbosity = 0
  )
  
  # 6. One-day-ahead prediction
  pred <- predict(
    fit_xgb,
    x_test
  )
  
  # 7. Save result
  results_xgb$Forecast[i] <- as.numeric(pred)
  results_xgb$Actual[i] <-
    df_xgb$Total_Patients[test_idx]
}

results_xgb <- results_xgb %>%
  mutate(
    Error = Actual - Forecast,
    Abs_Error = abs(Error),
    Sq_Error = Error^2
  )

MAE_xgb <- mean(
  results_xgb$Abs_Error,
  na.rm = TRUE
)

RMSE_xgb <- sqrt(
  mean(results_xgb$Sq_Error, na.rm = TRUE)
)

MAE_xgb
RMSE_xgb
