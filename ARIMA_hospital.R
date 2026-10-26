setwd("~/Desktop/ED_project")

library(dplyr)
library(timeDate)

######################################################################################
######################################################################################
############ Prediction for total_census ###################

model_vars <- list(
  M0 = c("total_census_lag1"),
  M1 = c("total_census_lag1", "bed_holds_lag1"),
  M2 = c("total_census_lag1", "bed_holds_lag1",
         "patients_bedded_lag1", "discharges_lag1"),
  M3 = c("total_census_lag1", "bed_holds_lag1",
         "patients_bedded_lag1", "discharges_lag1",
         "CNO_census_lag1"),
  M4 = c("total_census_lag1", "bed_holds_lag1",
         "patients_bedded_lag1", "discharges_lag1",
         "CNO_census_lag1", "acuity_lag1",
         "LOS_lag1", "LWBS_lag1")
)

sapply(model_vars, function(vars) {
  sum(complete.cases(cno[, c("total_census", vars)]))
})

all_vars <- unique(unlist(model_vars))
common_idx <- complete.cases(
  cno[, c("total_census", all_vars)]
)
sum(common_idx)

#Training & testing split(#66 with 2 missing total_census)
train_start = as.Date("2026-01-01")
train_end = as.Date("2026-07-31")
test_start = as.Date("2026-08-01")
test_end = as.Date("2026-10-05")

sum(
  common_idx &
    cno$date >= test_start &
    cno$date <= test_end
)

eval_dates <- cno$date[
  cno$date >= test_start &
    cno$date <= test_end &
    !is.na(cno$total_census)
] #64 with non-missing response


#Impute missing values for predictors except total_census_lag1
for (v in all_vars[-1]) {
  
  original <- cno[[v]]
  imputed <- original
  
  for (i in seq_along(original)) {
    
    if (is.na(original[i]) && i > 1) {
      
      history <- original[seq_len(i - 1)]
      history <- history[!is.na(history)]
      
      if (length(history) > 0) {
        imputed[i] <- median(history)
      }
    }
  }
  
  cno[[v]] <- imputed
}

#Impute missing values for total_census_lag1
for (i in which(is.na(cno$total_census_lag1))) {
  
  current_date <- cno$date[i]
  current_dow  <- cno$day_name[i]
  
  previous <- cno %>%
    filter(
      day_name == current_dow,
      date < current_date,
      date >= current_date - 28,
      !is.na(total_census_lag1)
    )
  
  cno$total_census_lag1[i] <- median(
    previous$total_census_lag1,
    na.rm = TRUE
  )
}

apply(cno[, all_vars], 2, function(x) sum(is.na(x)))


#New training set excluding rows with missing predictors
train_start = as.Date("2026-01-03")
train_end = as.Date("2026-07-31")
test_start = as.Date("2026-08-01")
test_end = as.Date("2026-10-05")

cno = cno %>% 
  mutate(day_of_week = wday(date, week_start = 7))
cno_test = cno %>%
  filter(date >= test_start & date <= test_end)


xreg_all = model.matrix(~ factor(day_of_week), cno)[, -1, drop = FALSE]
xreg_all = cbind(xreg_all, cno[, all_vars[-1]])
colnames(xreg_all)[1:6] = c("Monday", "Tuesday", "Wednesday",
                            "Thursday", "Friday", "Saturday"
)

pred_results = data.frame(
  Full_Date = cno_test$date,
  Total_Census = cno_test$total_census,
  Pred_m0 = NA,
  Pred_m1 = NA,
  Pred_m2 = NA,
  Pred_m3 = NA
)

for(i in 1:nrow(cno_test)){
  
  #Forecast date
  fc_date = cno_test$date[i]
  new_idx <- which(cno$date == fc_date)
  #print(paste("Forcast date:", fc_date))
  
  #Expanding training window
  train_idx <- which(
    cno$date >= train_start &
      cno$date <= (fc_date - 1)
  )
  
  #Linear regression
  # lm2 <- lm(total_census ~ .,
  #   data = train_data[train_idx, ]
  # )
  # pred_lm2 = predict(lm2, train_data[new_idx, 2:8, drop = FALSE])
  # pred_results[i, "Pred_lm2"] = pred_lm2
  
  
  # M0: ARMA(1,1) + day_of_week
  m0 <- arima(
    cno$total_census[train_idx],
    order = c(1, 0, 1),
    xreg = xreg_all[train_idx, 1:6],
    include.mean = TRUE,
    method = "ML"
  )
  pred_m0 = predict(m0, n.ahead = 1,
                    newxreg = xreg_all[new_idx, 1:6, drop = FALSE])
  pred_results[i, "Pred_m0"] = pred_m0$pred
  
  
  # M1: ARMA(1,1) + day-of-week + bed_holds
  m1 <- arima(
    cno$total_census[train_idx],
    order = c(1, 0, 1),
    xreg = xreg_all[train_idx, 1:7],
    include.mean = TRUE,
    method = "ML"
  )
  pred_m1 = predict(m1, n.ahead = 1,
                    newxreg = xreg_all[new_idx, 1:7, drop = FALSE])
  pred_results[i, "Pred_m1"] = pred_m1$pred
  
  # M2: ARMA(1,1) + year + day-of-week + bed_holds + patients_bedded 
  #     + discharges
  m2 <- arima(
    cno$total_census[train_idx],
    order = c(1, 0, 1),
    xreg = xreg_all[train_idx, 1:9],
    include.mean = TRUE,
    method = "ML"
  )
  pred_m2 = predict(m2, n.ahead = 1,
                    newxreg = xreg_all[new_idx, 1:9, drop = FALSE])
  pred_results[i, "Pred_m2"] = pred_m2$pred
  
  
  # M3: ARMA(1,1) + year + day-of-week + bed_holds + patients_bedded 
  #     + discharges
  m3 <- arima(
    cno$total_census[train_idx],
    order = c(1, 0, 1),
    xreg = xreg_all[train_idx, 1:10],
    include.mean = TRUE,
    method = "ML"
  )
  pred_m3 = predict(m3, n.ahead = 1,
                    newxreg = xreg_all[new_idx, 1:10, drop = FALSE])
  pred_results[i, "Pred_m3"] = pred_m3$pred
  
  
  # M4: ARMA(1,1) + year + day-of-week + bed_holds + patients_bedded 
  #     + discharges
  m4 <- arima(
    cno$total_census[train_idx],
    order = c(1, 0, 1),
    xreg = xreg_all[train_idx, ],
    include.mean = TRUE,
    method = "ML"
  )
  pred_m4 = predict(m4, n.ahead = 1,
                    newxreg = xreg_all[new_idx, , drop = FALSE])
  pred_results[i, "Pred_m4"] = pred_m4$pred
  
}

c(m0$aic, m1$aic, m2$aic, m3$aic)
summary(m0); summary(m1); summary(m2); summary(m3)

#Evaluation
m_eval = data.frame(
  row.names = c("M0", "M1", "M2", "M3"),
  Delta = rep(NA, 4),
  MAE = rep(NA, 4),
  RMSE = rep(NA, 4)
)

eval_idx = which(!is.na(cno_test$total_census))

m_eval[,"Delta"] = sapply(3:6, 
                        function(j) sum(pred_results[eval_idx, j])/sum(pred_results[eval_idx, 2]) - 1)

m_eval[,"MAE"] = sapply(3:6, 
                        function(j) mean(abs(pred_results[eval_idx, j] 
                                             - pred_results[eval_idx, 2])))
  c(sum(pred_results$Pred_m1)/sum(pred_results$Total_Patients) - 1,
    sum(pred_results$Pred_m2)/sum(pred_results$Total_Patients) - 1,
    )


m_eval[,"RMSE"] = sapply(3:6, 
                         function(j) sqrt(mean((pred_results[eval_idx, j] 
                                              - pred_results[eval_idx, 2])^2)))

m_eval






######################################################################################
######################################################################################
############ Prediction for bed_holds ###################
#Determine orders of ARIMA
# y <- df$bed_holds[df$year >= 2025]
# xreg_all = model.matrix(~ factor(year) + day_name, df)[, -1, drop = FALSE]
# xreg_all = cbind(xreg_all, cno[, all_vars[-1]])
# colnames(xreg_all) = c("Year2022", "Year2023", "Year2024", "Year2025", "Year2026",
#                        "Monday", "Tuesday", "Wednesday",
#                        "Thursday", "Friday", "Saturday"
# )
# 
# #Training & testing split(#66 with 2 missing total_census)
# train_idx = df$year >= 2025
# fit <- arima(
#   y,
#   order = c(1, 0, 1),
#   seasonal = list(
#     order = c(1, 0, 0),
#     period = 7
#   ),
#   xreg = xreg_all[train_idx, 5:11],
#   method = "ML"
# )
# summary(fit)
# plot.ts(fit$residuals)
# acf(fit$residuals, lag.max = 100, na.action = na.pass)
# pacf(fit$residuals, lag.max = 100, na.action = na.pass)
# res = as.numeric(fit$residuals); res = res[!is.na(res)]
# sapply(c(7, 14, 21, 28, 42), function(h) {
#   Box.test(
#     res,
#     lag = h,
#     type = "Ljung-Box",
#     fitdf = 2
#   )$p.value
# })

model_vars <- list(
  M0 = c("bed_holds_lag1"),
  M1 = c("bed_holds_lag1", "CNO_census_lag1"),
  M2 = c("bed_holds_lag1", "CNO_census_lag1",
         "discharges_lag1"),
  M3 = c("bed_holds_lag1", "CNO_census_lag1",
         "discharges_lag1", "total_census_lag1",
         "patients_bedded_lag1"),
  M4 = c("bed_holds_lag1", "CNO_census_lag1",
         "discharges_lag1", "total_census_lag1",
         "patients_bedded_lag1", "LOS_lag1",
         "acuity_lag1")
)

sapply(model_vars, function(vars) {
  sum(complete.cases(cno[, c("bed_holds", vars)]))
})

all_vars <- unique(unlist(model_vars))
common_idx <- complete.cases(
  cno[, c("bed_holds", all_vars)]
)
sum(common_idx)

#Training & testing split(#66 with 2 missing total_census)
train_start = as.Date("2026-01-01")
train_end = as.Date("2026-07-31")
test_start = as.Date("2026-08-01")
test_end = as.Date("2026-10-05")

sum(
  common_idx &
    cno$date >= test_start &
    cno$date <= test_end
)

eval_dates <- cno$date[
  cno$date >= test_start &
    cno$date <= test_end &
    !is.na(cno$bed_holds)
] #64 with non-missing response


#Impute missing values for predictors except bed_holds_lag1
for (v in all_vars[-1]) {
  
  original <- cno[[v]]
  imputed <- original
  
  for (i in seq_along(original)) {
    
    if (is.na(original[i]) && i > 1) {
      
      history <- original[seq_len(i - 1)]
      history <- history[!is.na(history)]
      
      if (length(history) > 0) {
        imputed[i] <- median(history)
      }
    }
  }
  
  cno[[v]] <- imputed
}

#Impute missing values for bed_holds_lag1
for (i in which(is.na(cno$bed_holds_lag1))) {
  
  current_date <- cno$date[i]
  current_dow  <- cno$day_name[i]
  
  previous <- cno %>%
    filter(
      day_name == current_dow,
      date < current_date,
      date >= current_date - 28,
      !is.na(bed_holds_lag1)
    )
  
  cno$bed_holds_lag1[i] <- median(
    previous$bed_holds_lag1,
    na.rm = TRUE
  )
}

apply(cno[, all_vars], 2, function(x) sum(is.na(x)))
View(cno[, all_vars])


#New training set excluding rows with missing predictors
train_start = as.Date("2026-01-03")
train_end = as.Date("2026-07-31")
test_start = as.Date("2026-08-01")
test_end = as.Date("2026-10-05")

cno_test = cno %>%
  filter(date >= test_start & date <= test_end)

xreg_all = model.matrix(~ day_name, cno)[, -1, drop = FALSE]
xreg_all = cbind(xreg_all, cno[, all_vars[-1]])
colnames(xreg_all)[1:6] = c("Monday", "Tuesday", "Wednesday",
                            "Thursday", "Friday", "Saturday"
)

pred_results = data.frame(
  date = cno_test$date,
  bed_holds = cno_test$bed_holds,
  pred_m0 = NA,
  pred_m1 = NA,
  pred_m2 = NA,
  pred_m3 = NA,
  pred_m4 = NA
)

for(i in 1:nrow(cno_test)){
  
  #Forecast date
  fc_date = cno_test$date[i]
  new_idx <- which(cno$date == fc_date)
  #print(paste("Forcast date:", fc_date))
  
  #Expanding training window
  train_idx <- which(
    cno$date >= train_start &
      cno$date <= (fc_date - 1)
  )
  
  #Linear regression
  # lm2 <- lm(bed_holds ~ .,
  #   data = train_data[train_idx, ]
  # )
  # pred_lm2 = predict(lm2, train_data[new_idx, 2:8, drop = FALSE])
  # pred_results[i, "Pred_lm2"] = pred_lm2
  
  
  # M0: ARMA(1,1) + day_of_week
  m0 <- arima(
    cno$bed_holds[train_idx],
    order = c(1, 0, 1),
    xreg = xreg_all[train_idx, 1:6],
    include.mean = TRUE,
    method = "ML"
  )
  pred_m0 = predict(m0, n.ahead = 1,
                    newxreg = xreg_all[new_idx, 1:6, drop = FALSE])
  pred_results[i, "pred_m0"] = pred_m0$pred
  
  
  # M1: ARMA(1,1) + day-of-week + CNO_census
  m1 <- arima(
    cno$bed_holds[train_idx],
    order = c(1, 0, 1),
    xreg = xreg_all[train_idx, 1:7],
    include.mean = TRUE,
    method = "ML"
  )
  pred_m1 = predict(m1, n.ahead = 1,
                    newxreg = xreg_all[new_idx, 1:7, drop = FALSE])
  pred_results[i, "pred_m1"] = pred_m1$pred
  
  # M2: ARMA(1,1) + year + day-of-week + CNO_census + discharges 
  #     + discharges
  m2 <- arima(
    cno$bed_holds[train_idx],
    order = c(1, 0, 1),
    xreg = xreg_all[train_idx, 1:8],
    include.mean = TRUE,
    method = "ML"
  )
  pred_m2 = predict(m2, n.ahead = 1,
                    newxreg = xreg_all[new_idx, 1:8, drop = FALSE])
  pred_results[i, "pred_m2"] = pred_m2$pred
  
  
  # M3: ARMA(1,1) + year + day-of-week + CNO_census + discharges
  #     + total_census + patients__bedded
  m3 <- arima(
    cno$bed_holds[train_idx],
    order = c(1, 0, 1),
    xreg = xreg_all[train_idx, 1:10],
    include.mean = TRUE,
    method = "ML"
  )
  pred_m3 = predict(m3, n.ahead = 1,
                    newxreg = xreg_all[new_idx, 1:10, drop = FALSE])
  pred_results[i, "pred_m3"] = pred_m3$pred
  
  
  # M4: ARMA(1,1) + year + day-of-week + CNO_census + discharges
  #     + total_census + patients__bedded + LOS + acuity
  m4 <- arima(
    cno$bed_holds[train_idx],
    order = c(1, 0, 1),
    xreg = xreg_all[train_idx, ],
    include.mean = TRUE,
    method = "ML"
  )
  pred_m4 = predict(m4, n.ahead = 1,
                    newxreg = xreg_all[new_idx, , drop = FALSE])
  pred_results[i, "pred_m4"] = pred_m4$pred
  
}

c(m0$aic, m1$aic, m2$aic, m3$aic, m4$aic)
summary(m0); summary(m1); summary(m2); summary(m3); summary(m4)

#Evaluation
m_eval = data.frame(
  row.names = c("M0", "M1", "M2", "M3", "M4"),
  Delta = rep(NA, 5),
  MAE = rep(NA, 5),
  RMSE = rep(NA, 5)
)

eval_idx = which(!is.na(cno_test$bed_holds))

m_eval[,"Delta"] = sapply(3:7, 
                          function(j) sum(pred_results[eval_idx, j])/sum(pred_results[eval_idx, 2]) - 1)

m_eval[,"MAE"] = sapply(3:7, 
                        function(j) mean(abs(pred_results[eval_idx, j] 
                                             - pred_results[eval_idx, 2])))

m_eval[,"RMSE"] = sapply(3:7, 
                         function(j) sqrt(mean((pred_results[eval_idx, j] 
                                                - pred_results[eval_idx, 2])^2)))

m_eval



#Try other predictors based on the estimated coefficients above
pred_results$pred_m5 = NA
for(i in 1:nrow(cno_test)){
  
  #Forecast date
  fc_date = cno_test$date[i]
  new_idx <- which(cno$date == fc_date)
  #print(paste("Forcast date:", fc_date))
  
  #Expanding training window
  train_idx <- which(
    cno$date >= train_start &
      cno$date <= (fc_date - 1)
  )
  
  
  # M5: ARMA(1,1) + day_of_week + total_census + acuity
  m5 <- arima(
    cno$bed_holds[train_idx],
    order = c(1, 0, 1),
    xreg = xreg_all[train_idx, c(1:6, 9, 12)],
    include.mean = TRUE,
    method = "ML"
  )
  pred_m5 = predict(m5, n.ahead = 1,
                    newxreg = xreg_all[new_idx, c(1:6, 9, 12), drop = FALSE])
  pred_results[i, "pred_m5"] = pred_m5$pred
  
}
c(m5$aic)
summary(m5)

#Evaluation
j = 6
m_eval = rbind(m_eval, rep(NA, 3)); row.names(m_eval)[j] = "M5"

eval_idx = which(!is.na(cno_test$bed_holds))

m_eval[j,"Delta"] = sum(pred_results[eval_idx, j + 2])/sum(pred_results[eval_idx, 2]) - 1

m_eval[j,"MAE"] = mean(abs(pred_results[eval_idx, j + 2] 
                                             - pred_results[eval_idx, 2]))

m_eval[j,"RMSE"] = sqrt(mean((pred_results[eval_idx, j + 2] 
                                                - pred_results[eval_idx, 2])^2))

m_eval


#Include holidays
#Function "get_holiday_df" and vector "holiday_unique" are from "Data.R"
cno_holiday = get_holiday_df(start_day = cno$date[1], 
                          end_day = cno$date[nrow(cno)], holidays_unique)
cno$is_federal_holiday = as.numeric(cno$date %in% cno_holiday$date)



######################################################################################
######################################################################################
######################################################################################
#Forecast using EPIC data
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
df_new = df[, c("date", "year", "day_name","bed_holds")]
df_new = df_new %>%
  full_join(cno %>%
               select(date, day_name, bed_holds) %>%
               rename(bed_holds_cno = bed_holds),
             by = c("date", "day_name"))
df_new$year[is.na(df_new$year)] = 2026

#Check bed holds data
id = which(!is.na(df_new$bed_holds) & df_new$date >= as.Date("2026-01-01"))
table(df_new$bed_holds[id] == df_new$bed_holds_cno[id]) #all TRUE

#Fill in missing bed_holds using bed_holds_cno
id = which(is.na(df_new$bed_holds) & df_new$date >= as.Date("2026-01-01") 
           & !is.na(df_new$bed_holds_cno))
df_new$bed_holds[id] = df_new$bed_holds_cno[id]
df_new = df_new %>% select(!bed_holds_cno)
remove(id)

#Impute missing values for bed_holds
df_new$bed_holds_raw = df_new$bed_holds
for (i in which(is.na(df_new$bed_holds))) {
  
  current_date <- df_new$date[i]
  current_dow  <- df_new$day_name[i]
  
  previous <- df_new %>%
    filter(
      day_name == current_dow,
      date < current_date,
      date >= current_date - 28,
      !is.na(bed_holds)
    )
  
  df_new$bed_holds[i] <- median(
    previous$bed_holds,
    na.rm = TRUE
  )
}
sum(is.na(df_new$bed_holds)); sum(is.na(df_new$bed_holds[df$year >= 2025]))

train_start = as.Date("2023-01-01")
train_end = as.Date("2026-07-31")
test_start = as.Date("2026-08-01")
test_end = as.Date("2026-10-05")

df_test = df_new %>%
  filter(date >= test_start & date <= test_end)

xreg_all = model.matrix(~ factor(year) + day_name, df_new)[, -1, drop = FALSE]
colnames(xreg_all) = c("Year2022", "Year2023", "Year2024", "Year2025", "Year2026",
                       "Monday", "Tuesday", "Wednesday", 
                       "Thursday", "Friday", "Saturday"
)

pred_results = data.frame(
  date = df_test$date,
  bed_holds = df_test$bed_holds_raw,
  pred_m0 = NA
)

for(i in 1:nrow(df_test)){
  
  #Forecast date
  fc_date = df_test$date[i]
  new_idx <- which(df_new$date == fc_date)
  #print(paste("Forcast date:", fc_date))
  
  #Expanding training window
  train_idx <- which(
    df_new$date >= train_start &
      df_new$date <= (fc_date - 1)
  )
  
  
  # M0: ARMA(1,1) + day_of_week
  m0 <- arima(
    df_new$bed_holds[train_idx],
    order = c(1, 0, 1),
    xreg = xreg_all[train_idx, 3:11],
    include.mean = TRUE,
    method = "ML"
  )
  pred_m0 = predict(m0, n.ahead = 1,
                    newxreg = xreg_all[new_idx, 3:11, drop = FALSE])
  pred_results[i, "pred_m0"] = pred_m0$pred
  
}

c(m0$aic)
summary(m0)

#Evaluation
m_eval = data.frame(
  row.names = c("M0_tr25", "M1_tr23"),
  Delta = rep(NA, 2),
  MAE = rep(NA, 2),
  RMSE = rep(NA, 2)
)

eval_idx = which(!is.na(df_test$bed_holds_raw))
j = 2
m_eval[j,"Delta"] = sum(pred_results[eval_idx, 3])/sum(pred_results[eval_idx, 2]) - 1

m_eval[j,"MAE"] = mean(abs(pred_results[eval_idx, 3] 
                                             - pred_results[eval_idx, 2]))

m_eval[j,"RMSE"] = sqrt(mean((pred_results[eval_idx, 3] 
                                                - pred_results[eval_idx, 2])^2))

m_eval
