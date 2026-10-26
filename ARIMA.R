setwd("~/Desktop/ED_project")

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(forecast)

#Load data
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
  
  df$Total_Patients[i] <- round(median(
    previous$Total_Patients,
    na.rm = TRUE
  ))
}
View(df[df$Total_Patients_Raw == 0,])
##############################################################################
##############################################################################
#################### Training with Expanding Window ##########################
############################ Daily Forecast ##################################
train_start = as.Date("2023-01-01")
test_start = as.Date("2026-04-01")
test_end = as.Date("2026-08-16")
df_test = df %>%
  filter(Full_Date >= test_start & Full_Date <= test_end)
#Covariates for all data
xreg_all = model.matrix(~ factor(Year) + factor(Day_of_Week)
                        + factor(Month), df)[, -1, drop = FALSE]
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
    xreg = xreg_all[train_idx, 1:9],
    include.mean = TRUE,
    method = "ML"
  )
  pred_m1 = predict(m1, n.ahead = 1, 
                    newxreg = xreg_all[new_idx, 1:9, drop = FALSE])
  pred_results[i, "Pred_m1"] = pred_m1$pred
  
  # ARMA(1,1) + year + day-of-week (combining weekends)
  m2 <- arima(
    df$Total_Patients[train_idx],
    order = c(1, 0, 1),
    xreg = xreg_all[train_idx, 1:8],
    include.mean = TRUE,
    method = "ML"
  )
  pred_m2 = predict(m2, n.ahead = 1,
                    newxreg = xreg_all[new_idx, 1:8, drop = FALSE])
  pred_results[i, "Pred_m2"] = pred_m2$pred
  
}

c(m1$aic, m2$aic)
View(cbind(m1$coef, sqrt(diag(m1$var.coef))))
View(cbind(m2$coef, sqrt(diag(m2$var.coef))))

#Evaluation
m_eval = data.frame(
  row.names = c("M2_Yr_DOM", "M2_Yr_DOM_weekends"),
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
############################ Comapre ARMA and ES #############################
##############################################################################
train_start = as.Date("2025-01-01")
train_end = as.Date("2026-05-31")
test_start = as.Date("2026-06-01")
test_end = as.Date("2026-08-16")
patients_train = df %>%
  filter(Full_Date >= train_start,
         Full_Date <= train_end) %>%
  pull(Total_Patients)

patients_test = df %>%
  filter(Full_Date >= test_start,
         Full_Date <= test_end) %>%
  pull(Total_Patients)


# Auto select ets
m_ets = ets(ts(patients_train, frequency = 7))
summary(m_ets)
pred_ets = forecast(m_ets, h = length(patients_test))
pred = as.numeric(pred_ets$mean)
mean(abs(patients_test - pred))
sqrt(mean((patients_test - pred)^2))
sum(pred)/sum(actual) - 1


#ARMA
xreg_all = model.matrix(~ factor(Year) + factor(Day_of_Week), 
                        df)[, -1, drop = FALSE]
colnames(xreg_all) = c("Year2024", "Year2025", "Year2026",
                       "Monday", "Tuesday", "Wednesday",
                       "Thursday", "Friday", "Saturday"
)

train_idx = which(df$Full_Date >= train_start &
                  df$Full_Date <= train_end)
new_idx = which(df$Full_Date >= test_start &
                df$Full_Date <= test_end)
m_arma <- arima(
  patients_train,
  order = c(1, 0, 1),
  xreg = xreg_all[train_idx, 3:8],
  include.mean = TRUE,
  method = "ML"
)
pred_arma = predict(m_arma, n.ahead = length(patients_test),
                  newxreg = xreg_all[new_idx, 3:8, drop = FALSE])
mean(abs(patients_test - pred_arma$pred))
sqrt(mean((patients_test - pred_arma$pred)^2))
sum(pred_arma$pred)/sum(patients_test) - 1


#Compare ETS and ARMA using rolling window
train_start = as.Date("2025-01-01")
train_end = as.Date("2026-05-31")
test_start = as.Date("2026-06-01")
test_end = as.Date("2026-08-16")
forecast_dates <- seq(test_start, test_end, by = "day")
results <- data.frame(
  Date = forecast_dates,
  Actual = NA_real_,
  ARMA = NA_real_,
  ETS = NA_real_
)
for (i in seq_along(forecast_dates)) {
  
  fc_date <- forecast_dates[i]
  
  # Everything available before forecast date
  train_idx <- which(
    df$Full_Date >= train_start &
      df$Full_Date < fc_date
  )
  
  test_idx <- which(df$Full_Date == fc_date)
  
  y_train <- df$Total_Patients[train_idx]
  
  
  # -------------------------
  # 1. ARMA + Year + DOW
  # -------------------------
  
  fit_arma <- arima(
    y_train,
    order = c(1, 0, 1),
    xreg = xreg_all[train_idx, -c(1,2,9), drop = FALSE],
    include.mean = TRUE,
    method = "ML"
  )
  
  pred_arma <- predict(
    fit_arma,
    n.ahead = 1,
    newxreg = xreg_all[test_idx, -c(1,2,9), drop = FALSE]
  )
  
  results$ARMA[i] <- as.numeric(pred_arma$pred)
  
  
  # -------------------------
  # 2. ETS(M,N,M)
  # -------------------------
  
  y_train_ts <- ts(
    y_train,
    frequency = 7
  )
  
  fit_ets <- ets(
    y_train_ts,
    model = "MNM"
  )
  
  pred_ets <- forecast(
    fit_ets,
    h = 1
  )
  
  results$ETS[i] <- as.numeric(pred_ets$mean)
  
  
  # Actual
  results$Actual[i] <- df$Total_Patients[test_idx]
}

results <- results %>%
  mutate(
    error_ARMA = Actual - ARMA,
    error_ETS  = Actual - ETS
  )

performance <- data.frame(
  Model = c(
    "ARMA(1,1) + Year + DOW",
    "ETS(M,N,M)"
  ),
  
  MAE = c(
    mean(abs(results$error_ARMA)),
    mean(abs(results$error_ETS))
  ),
  
  RMSE = c(
    sqrt(mean(results$error_ARMA^2)),
    sqrt(mean(results$error_ETS^2))
  )
)

performance
