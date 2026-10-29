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

cno_test = cno %>%
  filter(date >= test_start & date <= test_end)

xreg_all = model.matrix(~ day_name, cno)[, -1, drop = FALSE]
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
  
  
  # M0: ARMA(1,0) + day_of_week
  m0 <- arima(
    cno$bed_holds[train_idx],
    order = c(1, 0, 0),
    xreg = xreg_all[train_idx, 1:6],
    include.mean = TRUE,
    method = "ML"
  )
  pred_m0 = predict(m0, n.ahead = 1,
                    newxreg = xreg_all[new_idx, 1:6, drop = FALSE])
  pred_results[i, "pred_m0"] = pred_m0$pred
  
  
  # M1: ARMA(1,0) + day-of-week + CNO_census
  m1 <- arima(
    cno$bed_holds[train_idx],
    order = c(1, 0, 0),
    xreg = xreg_all[train_idx, 1:7],
    include.mean = TRUE,
    method = "ML"
  )
  pred_m1 = predict(m1, n.ahead = 1,
                    newxreg = xreg_all[new_idx, 1:7, drop = FALSE])
  pred_results[i, "pred_m1"] = pred_m1$pred
  
  # M2: ARMA(1,0) + year + day-of-week + CNO_census + discharges 
  #     + discharges
  m2 <- arima(
    cno$bed_holds[train_idx],
    order = c(1, 0, 0),
    xreg = xreg_all[train_idx, 1:8],
    include.mean = TRUE,
    method = "ML"
  )
  pred_m2 = predict(m2, n.ahead = 1,
                    newxreg = xreg_all[new_idx, 1:8, drop = FALSE])
  pred_results[i, "pred_m2"] = pred_m2$pred
  
  
  # M3: ARMA(1,0) + year + day-of-week + CNO_census + discharges
  #     + total_census + patients__bedded
  m3 <- arima(
    cno$bed_holds[train_idx],
    order = c(1, 0, 0),
    xreg = xreg_all[train_idx, 1:10],
    include.mean = TRUE,
    method = "ML"
  )
  pred_m3 = predict(m3, n.ahead = 1,
                    newxreg = xreg_all[new_idx, 1:10, drop = FALSE])
  pred_results[i, "pred_m3"] = pred_m3$pred
  
  
  # M4: ARMA(1,0) + year + day-of-week + CNO_census + discharges
  #     + total_census + patients__bedded + LOS + acuity
  m4 <- arima(
    cno$bed_holds[train_idx],
    order = c(1, 0, 0),
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
  
  
  # M5: ARMA(1,0) + day_of_week + total_census + acuity
  m5 <- arima(
    cno$bed_holds[train_idx],
    order = c(1, 0, 0),
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


######################################################################################
######################################################################################
#Include holidays
#Function "get_holiday_df" and vector "holiday_unique" are from "Data.R"
cno_holiday = get_holiday_df(start_day = cno$date[1], 
                          end_day = cno$date[nrow(cno)], holidays_unique)
#Dummy variable for *observed* federal holiday
cno$is_federal_holiday = as.numeric(cno$date %in% cno_holiday[!cno_holiday$dow_name %in% 
                                                  c("Saturday", "Sunday"), "date"])
cno <- cno %>%
  arrange(date) %>%
  mutate(
    holiday_lag1  = lag(is_federal_holiday, 1),
    holiday_lead1 = lead(is_federal_holiday, 1)
  )
cno$holiday_lag1[1] <- 0
cno$holiday_lead1[nrow(cno)] <- 0

xreg_all = model.matrix(~ day_name, cno)[, -1, drop = FALSE]
xreg_all = cbind(xreg_all, cno[, c("CNO_total_census",
                                   "is_federal_holiday", "holiday_lag1", "holiday_lead1",
                                   "bed_holds", "bed_holds_lag1")])
colnames(xreg_all)[1:6] = c("Monday", "Tuesday", "Wednesday",
                            "Thursday", "Friday", "Saturday"
)
xreg_all$CNO_total_census_lag1 = lag(xreg_all$CNO_total_census, 1)

# Impute using historical median only
for (i in seq_len(nrow(xreg_all))) {
  
  if (is.na(xreg_all$CNO_total_census[i]) && i > 1) {
    
    historical_values <- xreg_all$CNO_total_census[1:(i-1)]
    
    if (any(!is.na(historical_values))) {
      xreg_all$CNO_total_census[i] <- median(
        historical_values,
        na.rm = TRUE
      )
    }
  }
}

# Recalculate lagged CNO census
xreg_all$CNO_total_census_lag1 = lag(xreg_all$CNO_total_census, 1)

apply(xreg_all, 2, function(x) sum(is.na(x)))

#Training set excluding rows with missing predictors
train_start = as.Date("2026-01-03")
train_end = as.Date("2026-07-31")
test_start = as.Date("2026-08-01")
test_end = as.Date("2026-10-05")

cno_test = cno %>%
  filter(date >= test_start & date <= test_end)

pred_results = data.frame(
  date = cno_test$date,
  bed_holds = cno_test$bed_holds,
  pred0 = NA,
  pred1 = NA,
  pred2 = NA,
  pred3 = NA,
  pred2_cno = NA,
  cno = cno_test$CNO_total_census
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
  
  
  #M0: AR1 + dow + holiday
  fit0 <- arima(
    cno$bed_holds[train_idx],
    order = c(1, 0, 0),
    xreg = xreg_all[train_idx, c(1:6, 8:10)],
    include.mean = TRUE,
    method = "ML"
  )
  pred = predict(fit0, n.ahead = 1,
                 newxreg = xreg_all[new_idx, c(1:6, 8:10), drop = FALSE])
  pred_results[i, "pred0"] = pred$pred
  
  #M1: AR1 + dow + holiday + CNO_total_census_lag1
  fit1 <- arima(
    cno$bed_holds[train_idx],
    order = c(1, 0, 0),
    xreg = xreg_all[train_idx, c(1:6, 8:10, 13)],
    include.mean = TRUE,
    method = "ML"
  )
  pred = predict(fit1, n.ahead = 1,
                 newxreg = xreg_all[new_idx, c(1:6, 8:10, 13), drop = FALSE])
  pred_results[i, "pred1"] = pred$pred
  
  #M2: AR1 + dow + holiday + predicted CNO_total_census
  fit_cno <- arima(
    cno$CNO_total_census[train_idx],
    order = c(1, 0, 0),
    xreg = xreg_all[train_idx, 1:6],
    include.mean = TRUE,
    method = "ML"
  )
  pred_cno = predict(fit_cno, n.ahead = 1,
                     newxreg = xreg_all[new_idx, 1:6, drop = FALSE])
  pred_cno = pred_cno$pred
  pred_results[i, "pred2_cno"] = pred_cno
  
  fit2 <- arima(
    cno$bed_holds[train_idx],
    order = c(1, 0, 0),
    xreg = xreg_all[train_idx, 1:10],
    include.mean = TRUE,
    method = "ML"
  )
  
  newdata = xreg_all[new_idx, 1:10]
  newdata$CNO_total_census = pred_cno
  
  pred = predict(fit2, n.ahead = 1, newxreg = newdata)
  pred_results[i, "pred2"] = pred$pred
  
  #M3:AR1 + dow + holiday + actual CNO_total_census
  fit3 <- arima(
    cno$bed_holds[train_idx],
    order = c(1, 0, 0),
    xreg = xreg_all[train_idx, c(1:10)],
    include.mean = TRUE,
    method = "ML"
  )
  pred = predict(fit3, n.ahead = 1,
                 newxreg = xreg_all[new_idx, 1:10, drop = FALSE])
  pred_results[i, "pred3"] = pred$pred
  
}
apply(pred_results, 2, function(x) sum(is.na(x)))

#Evaluation
m_eval = data.frame(
  row.names = c("M0", "M1", "M2", "M3"),
  Delta = rep(NA, 4),
  MAE = rep(NA, 4),
  RMSE = rep(NA, 4)
)


#m_eval[,"Delta"] = sapply(3:6, 
#function(j) sum(pred_results_cno_total[eval_idx, j])/sum(pred_results_cno_total[eval_idx, 2]) - 1)

m_eval[,"MAE"] = sapply(3:6, 
                        function(j) mean(abs(pred_results[, j] 
                                             - pred_results[, 2]), na.rm = T))

m_eval[,"RMSE"] = sapply(3:6, 
                         function(j) sqrt(mean((pred_results[, j] 
                                                - pred_results[, 2])^2, na.rm = T)))

m_eval



######################################################################################
######################################################################################
############ Two stage prediction for bed_holds using CNO_total_census ###############
#Predict CNO_total_census
y <- cno$CNO_total_census
xreg_all = model.matrix(~ day_name, cno)[, -1, drop = FALSE]
colnames(xreg_all) = c("Monday", "Tuesday", "Wednesday",
                       "Thursday", "Friday", "Saturday"
)

#ARMA
fit1 <- arima(
  y,
  order = c(1, 0, 0),
  xreg = xreg_all,
  include.mean = TRUE,
  method = "ML"
)

fit2 <- arima(
  y,
  order = c(2, 0, 0),
  xreg = xreg_all,
  include.mean = TRUE,
  method = "ML"
)

fit3 <- arima(
  y,
  order = c(1, 0, 0),
  seasonal = list(
    order = c(1, 0, 0),
    period = 7
  ),
  xreg = xreg_all,
  method = "ML"
)

c(fit1$aic, fit2$aic, fit3$aic)
summary(fit1); summary(fit2); summary(fit3)

plot.ts(fit3$residuals)
acf(fit3$residuals, lag.max = 100, na.action = na.pass)
pacf(fit3$residuals, lag.max = 100, na.action = na.pass)
Box.test(fit3$residuals, type = "Ljung-Box", fitdf = 1, lag = 63)


#Training set excluding rows with missing predictors
train_start = as.Date("2026-01-01")
train_end = as.Date("2026-07-31")
test_start = as.Date("2026-08-01")
test_end = as.Date("2026-10-05")

xreg_all = model.matrix(~ day_name, cno)[, -1, drop = FALSE]
xreg_all = cbind(xreg_all, cno$CNO_total_census)
colnames(xreg_all) = c("Monday", "Tuesday", "Wednesday",
                       "Thursday", "Friday", "Saturday",
                       "CNO_total_census"
)
xreg_all = as.data.frame(xreg_all)
xreg_all$CNO_total_census_lag1 = lag(xreg_all$CNO_total_census, 1)
apply(xreg_all, 2, function(x) sum(is.na(x)))

cno_test = cno %>%
  select(date, CNO_total_census) %>%
  filter(date >= test_start & date <= test_end)

pred_results_cno_total = data.frame(
  date = cno_test$date,
  bed_holds = cno_test$CNO_total_census,
  pred1 = NA,
  pred2 = NA,
  pred3 = NA,
  pred_lm = NA
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
  
  
  # AR(1) + dow
  fit1 <- arima(
    cno$CNO_total_census[train_idx],
    order = c(1, 0, 0),
    xreg = xreg_all[train_idx, 1:6],
    include.mean = TRUE,
    method = "ML"
  )
  pred = predict(fit1, n.ahead = 1,
                 newxreg = xreg_all[new_idx, 1:6, drop = FALSE])
  pred_results_cno_total[i, "pred1"] = pred$pred
  
  # AR(2) + dow
  fit2 <- arima(
    cno$CNO_total_census[train_idx],
    order = c(2, 0, 0),
    xreg = xreg_all[train_idx, 1:6],
    include.mean = TRUE,
    method = "ML"
  )
  pred = predict(fit2, n.ahead = 1,
                 newxreg = xreg_all[new_idx, 1:6, drop = FALSE])
  pred_results_cno_total[i, "pred2"] = pred$pred
  
  # SAR(1)(1)_7 + dow
  fit3 <- arima(
    cno$CNO_total_census[train_idx],
    order = c(1, 0, 0),
    seasonal = list(
      order = c(1, 0, 0),
      period = 7
    ),
    xreg = xreg_all[train_idx, 1:6],
    include.mean = TRUE,
    method = "ML"
  )
  pred = predict(fit3, n.ahead = 1,
                 newxreg = xreg_all[new_idx, 1:6, drop = FALSE])
  pred_results_cno_total[i, "pred3"] = pred$pred
  
  #Linear regression
  fit_lm <- lm(CNO_total_census ~ .,
               data = xreg_all[train_idx, ]
  )
  pred = predict(fit_lm, xreg_all[new_idx, , drop = FALSE])
  pred_results_cno_total[i, "pred_lm"] = pred
  
}

#Evaluation
m_eval = data.frame(
  row.names = c("AR1", "AR2", "SAR1", "LM"),
  MAE = rep(NA, 4),
  RMSE = rep(NA, 4)
)


m_eval[,"MAE"] = sapply(3:6, 
                        function(j) mean(abs(pred_results_cno_total[, j] 
                                             - pred_results_cno_total[, 2]), na.rm = T))

m_eval[,"RMSE"] = sapply(3:6, 
                         function(j) sqrt(mean((pred_results_cno_total[, j] 
                                                - pred_results_cno_total[, 2])^2, na.rm = T)))

m_eval


#Two-stage prediction for bed_holds 
#Choose predicted CNO_total_census using AR1 + dow
xreg_all = model.matrix(~ day_name, cno)[, -1, drop = FALSE]
xreg_all = cbind(xreg_all, cno[, c("CNO_total_census",
                                   "is_federal_holiday", "holiday_lag1", "holiday_lead1",
                                   "bed_holds", "bed_holds_lag1")])
colnames(xreg_all)[1:6] = c("Monday", "Tuesday", "Wednesday",
                            "Thursday", "Friday", "Saturday"
)
xreg_all$CNO_total_census_lag1 = lag(xreg_all$CNO_total_census, 1)

# Impute using historical median only
for (i in seq_len(nrow(xreg_all))) {
  
  if (is.na(xreg_all$CNO_total_census[i]) && i > 1) {
    
    historical_values <- xreg_all$CNO_total_census[1:(i-1)]
    
    if (any(!is.na(historical_values))) {
      xreg_all$CNO_total_census[i] <- median(
        historical_values,
        na.rm = TRUE
      )
    }
  }
}

# Recalculate lagged CNO census
xreg_all$CNO_total_census_lag1 = lag(xreg_all$CNO_total_census, 1)

apply(xreg_all, 2, function(x) sum(is.na(x)))

#Training set excluding rows with missing predictors
train_start = as.Date("2026-01-01")
train_end = as.Date("2026-07-31")
test_start = as.Date("2026-08-01")
test_end = as.Date("2026-10-05")

cno_test = cno %>%
  filter(date >= test_start & date <= test_end)

pred_results_lm = data.frame(
  date = cno_test$date,
  bed_holds = cno_test$bed_holds,
  pred0 = NA,
  pred1 = NA,
  pred2 = NA,
  pred3 = NA,
  pred2_cno = NA,
  cno = cno_test$CNO_total_census
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
  
  
  #M0: LM + dow + holiday
  fit0 <- lm(bed_holds ~ .,
             data = xreg_all[train_idx, c(1:6, 8:12)]
  )
  pred = predict(fit0, xreg_all[new_idx, c(1:6, 8:12), drop = FALSE])
  pred_results_lm[i, "pred0"] = pred
  
  #M1: LM + dow + holiday + CNO_total_census_lag1
  fit1 <- lm(bed_holds ~ .,
             data = xreg_all[train_idx, c(1:6, 8:12, 13)]
  )
  pred = predict(fit1, xreg_all[new_idx, c(1:6, 8:12, 13), drop = FALSE])
  pred_results_lm[i, "pred1"] = pred
  
  #M2: LM + dow + holiday + predicted CNO_total_census
  fit_cno <- arima(
    cno$CNO_total_census[train_idx],
    order = c(1, 0, 0),
    xreg = xreg_all[train_idx, 1:6],
    include.mean = TRUE,
    method = "ML"
  )
  pred_cno = predict(fit_cno, n.ahead = 1,
                 newxreg = xreg_all[new_idx, 1:6, drop = FALSE])
  pred_cno = pred_cno$pred
  pred_results_lm[i, "pred2_cno"] = pred_cno
  
  fit2 <- lm(bed_holds ~ .,
             data = xreg_all[train_idx, 1:12]
  )
  
  newdata = xreg_all[new_idx, c(1:6, 8:10, 12)]
  newdata$CNO_total_census = pred_cno
  
  pred = predict(fit2, newdata)
  pred_results_lm[i, "pred2"] = pred
  
  #M3:LM + dow + holiday + actual CNO_total_census
  fit3 <- lm(bed_holds ~ .,
             data = xreg_all[train_idx, 1:12]
  )
  pred = predict(fit3, xreg_all[new_idx, 1:12, drop = FALSE])
  pred_results_lm[i, "pred3"] = pred
  
}

#Evaluation
m_eval = data.frame(
  row.names = c("M0", "M1", "M2", "M3"),
  Delta = rep(NA, 4),
  MAE = rep(NA, 4),
  RMSE = rep(NA, 4)
)


#m_eval[,"Delta"] = sapply(3:6, 
#function(j) sum(pred_results_cno_total[eval_idx, j])/sum(pred_results_cno_total[eval_idx, 2]) - 1)

m_eval[,"MAE"] = sapply(3:6, 
                        function(j) mean(abs(pred_results_lm[, j] 
                                             - pred_results_lm[, 2]), na.rm = T))

m_eval[,"RMSE"] = sapply(3:6, 
                         function(j) sqrt(mean((pred_results_lm[, j] 
                                                - pred_results_lm[, 2])^2, na.rm = T)))

m_eval


######################################################################################
######################################################################################
############ Predict bed_holds until 2027-05-01 ###############
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

#Training data design matrix
xreg_all = model.matrix(~ day_name, cno)[, -1, drop = FALSE]
xreg_all = cbind(xreg_all, cno[, c("is_federal_holiday", "holiday_lag1", "holiday_lead1")])
colnames(xreg_all)[1:6] = c("Monday", "Tuesday", "Wednesday",
                            "Thursday", "Friday", "Saturday"
)

fit0 <- arima(
  cno$bed_holds,
  order = c(1, 0, 0),
  xreg = xreg_all,
  include.mean = TRUE,
  method = "ML"
)
summary(fit0)

fc <- predict(
  fit0,
  n.ahead = h,
  newxreg = xreg_future
)

# Forecast through April 30, 2027
forecast_results <- data.frame(
  date = future_dates,
  predicted_bed_holds = as.numeric(fc$pred),
  lower95 = as.numeric(fc$pred - 1.96 * fc$se),
  upper95 = as.numeric(fc$pred + 1.96 * fc$se)
)

#Plot
ggplot(forecast_results,
       aes(x = date, y = predicted_bed_holds)) +
  geom_ribbon(
    aes(ymin = lower95, ymax = upper95),
    alpha = 0.2
  ) +
  geom_line(color = "blue", linewidth = 0.7) +
  labs(
    title = "Bed Holds Forecast",
    subtitle = "October 6, 2026 – April 30, 2027",
    x = "Date",
    y = "Predicted Bed Holds"
  ) +
  theme_minimal()

write.csv(forecast_results, file = "Forecast_bed_holds_wz.csv", row.names = F)





#Plot
c(fit0$aic, fit1$aic, fit2$aic)
summary(fit0); summary(fit1); summary(fit2)
#MAE
sapply(3:6, function(j) mean(abs(pred_results[, j] - pred_results[, 2]), na.rm = T))
#RMSE
sapply(3:6, function(j) sqrt(mean((pred_results[, j] - pred_results[, 2])^2, na.rm = T)))
# 1. Calculate forecast errors
pred_results <- pred_results %>%
  mutate(error = pred0 - bed_holds)

# 2. Calculate summary statistics
mean_error <- mean(predict_results$error, na.rm = TRUE)
sd_error   <- sd(predict_results$error, na.rm = TRUE)

Q1 <- quantile(predict_results$error, 0.25, na.rm = TRUE)
Q3 <- quantile(predict_results$error, 0.75, na.rm = TRUE)
IQR_error <- Q3 - Q1

# Mean +/- SD
lower_sd <- mean_error - sd_error
upper_sd <- mean_error + sd_error

# Tukey lower and upper fences
lower_fence <- Q1 - 1.5 * IQR_error
upper_fence <- Q3 + 1.5 * IQR_error

# 3. Plot
ggplot(predict_results, aes(x = date, y = error)) +
  
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
    title = "Daily Bed Holds Forecast Errors",
    subtitle = paste0(
      "Mean = ", round(mean_error, 2),
      " | SD = ", round(sd_error, 2),
      " | IQR = ", round(IQR_error, 2)
    ),
    x = "Date",
    y = "Predicted - Actual"
  ) +
  
  theme_minimal(base_size = 13)
