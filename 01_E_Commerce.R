# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Machine Learning Homework 01
# Purpose:      E-commerce
# programmer:   Zhe Liu
# Date:         2022-10-05
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Setup environment variables ----
options(java.parameters = "-Xmx2048m",
        stringsAsFactors = FALSE, 
        encoding = 'UTF-8')


##---- Loading the required packages ----
suppressPackageStartupMessages({
  library(zip)
  library(openxlsx)
  library(readxl)
  library(writexl)
  library(RcppRoll)
  library(plyr)
  library(stringi)
  library(feather)
  library(RODBC)
  library(MASS)
  library(car)
  library(data.table)
  library(tidyverse)
  library(lubridate)
  # TS
  library(tseries)
  library(forecast)
  library(lmtest)
  library(FinTS)
  # DT
  library(party)
  library(partykit)
  library(rpart)
  # RF
  library(randomForest)
  # plot
  library(isotone)
})


##---- Readin data ----
fresh.raw <- read_csv('Tseq_Sales.csv')

fresh.clean <- fresh.raw %>% 
  separate(date, into = c('year', 'month', 'day'), sep = '/') %>% 
  mutate(month = stri_pad_left(month, 2, 0), 
         day = stri_pad_left(day, 2, 0)) %>% 
  arrange(is_train, year, month, day)

fresh.train <- filter(fresh.clean, is_train == 0)
fresh.test <- filter(fresh.clean, is_train == 1)

plot(ts(fresh.train$sales))


##---- ARIMA ----
## ADF test
## Optimal Level of Lag is decided using Akaike Information Criterion (AIC) or 
## Schwartz Bayesian Information Criterion (SBIC).
# fresh.adf <- adf.test(fresh.train$sales)
# fresh.adf1 <- adf.test(diff(fresh.train$sales, lag = 1))

## ACF
fresh.arima.acf1 <- acf(diff(fresh.train$sales, differences = 1), lag.max = 30)

## 一阶差分平稳
fresh.sta <- fresh.train %>% 
  mutate(sales_sta = sales - lag(sales)) %>% 
  filter(!is.na(sales_sta)) %>% 
  select(year, month, day, sales_sta)

plot(ts(fresh.sta$sales_sta))

## Box test
fresh.arima.box <- Box.test(fresh.sta$sales_sta, type = 'Ljung-Box')
## 非白噪声

## 一阶差分的ACF一阶截尾
fresh.arima.acf <- acf(fresh.sta$sales_sta, lag.max = 30)

## 一阶差分的PACF拖尾
fresh.arima.pacf <- pacf(fresh.sta$sales_sta, lag.max = 30)

## 拟合ARIMA(0,1,1)模型
fresh.arima.fit <- arima(fresh.train$sales, order = c(0, 1, 1))

## 残差白噪声检验（模型显著性检验）
fresh.arima.res <- Box.test(fresh.arima.fit$residuals, type = 'Ljung-Box')
# 残差为白噪声

## 参数显著性检验
CoefTestFunc <- function(object) {
  coef <- object$coef[object$coef != 0]
  var.coef <- object$var.coef
  len <- length(coef)
  df <- object$nobs - len
  for (i in 1:len) {
    t <- coef[i] / sqrt(var.coef[i, i])
    lower <- ifelse(coef[i] < 0, 1, 0)
    pt <- pt(t, df = df, lower.tail = lower)
    print(pt)
  }
}

CoefTestFunc(fresh.arima.fit)
# 系数显著非零

## 预测
# x_t = x_{t-1} + \epsilon_t - \theta_1 * \epsilon_{t-1}
# fresh.fc <- forecast(fresh.arima.fit, h = 5, level = c(90, 95))
sd <- sqrt(fresh.arima.fit$sigma2)
coef <- fresh.arima.fit$coef[1]
set.seed(5)
fresh.arima.pred <- fresh.test %>% 
  mutate(e = rnorm(nrow(fresh.test), sd = sd), 
         sales_pred = sales[1])
for (i in 2:nrow(fresh.arima.pred)) {
  pred <- fresh.arima.pred$sales_pred[i-1] + fresh.arima.pred$e[i] - coef * 
    fresh.arima.pred$e[i-1]
  fresh.arima.pred$sales_pred[i] <- ifelse(pred > 0, pred, 0)
}


##---- Residual auto-regressive ----
## 拟合关于时间t的线性回归模型
fresh.sales <- ts(fresh.train$sales)
t <- 1:length(fresh.sales)
t2 <- t^2
fresh.lm.fit1 <- lm(fresh.sales ~ t + t2)
summary(fresh.lm.fit1)

## 拟合关于延迟变量的自回归模型
fresh.sales.lag <- fresh.sales[2:length(fresh.sales)]
fresh.sales2 <- fresh.sales[1:(length(fresh.sales)-1)]
fresh.lm.fit2 <- lm(fresh.sales2 ~ fresh.sales.lag)
summary(fresh.lm.fit2)

## 两个趋势拟合模型的拟合效果图
fresh.lm.ts1 <- ts(fresh.lm.fit1$fitted.values)
fresh.lm.ts2 <- ts(fresh.lm.fit2$fitted.values)
plot(fresh.sales, pch = 4)
lines(fresh.lm.ts1, col = 2)
lines(fresh.lm.ts2, col = 4)

## DW test
dwtest(fresh.lm.fit1)
# 残差序列高度正相关

dwtest(fresh.lm.fit2, order.by = fresh.sales.lag)
# 残差序列不存在显著相关

## ACF拖尾
fresh.rar.acf <- acf(fresh.lm.fit1$residuals, lag.max = 30)

## PACF七阶结尾
fresh.rar.pacf <- pacf(fresh.lm.fit1$residuals, lag.max = 30)

## 拟合ARIMA((1,5),0,0)模型
fresh.rar.fit <- arima(fresh.lm.fit1$residuals, 
                       order = c(5, 0, 0), 
                       include.mean = FALSE, 
                       transform.pars = FALSE, 
                       fixed = c(NA, 0, 0, 0, NA))
# auto.arima(fresh.lm.fit1$residuals)

## 残差白噪声检验（模型显著性检验）
fresh.rar.res <- Box.test(fresh.rar.fit$residuals, type = 'Ljung-Box')
# 残差为白噪声

## 参数显著性检验
CoefTestFunc(fresh.rar.fit)
# 系数显著非零

## 预测
# x_t = c + bt + a * t^2
# \epsilon_t = a1 * \epsilon_{t-1} + a2 * \epsilon_{t-3} + a3 * \epsilon_{t-5} + 
# a4 * \epsilon_{t-7} + e_t
fresh.rar.coef1 <- fresh.lm.fit1$coefficients[1]
fresh.rar.coef2 <- fresh.lm.fit1$coefficients[2]
fresh.rar.coef3 <- fresh.lm.fit1$coefficients[3]
sd <- sqrt(fresh.rar.fit$sigma2)

fresh.rar.pred <- fresh.test %>% 
  mutate(t = row_number(), 
         e = NA)
set.seed(10)
fresh.rar.pred$e[1:5] <- rnorm(5, sd = sd)
set.seed(15)
for (i in 6:nrow(fresh.rar.pred)) {
  fresh.rar.pred$e[i] <- sum(fresh.rar.pred$e[(i-1):(i-5)] * fresh.rar.fit$coef)
}
fresh.rar.pred <- fresh.rar.pred %>% 
  mutate(sales_pred = fresh.rar.coef1 + fresh.rar.coef2 * t + fresh.rar.coef3 * t^2 + e, 
         sales_pred_ad = ifelse(sales_pred > 0, sales_pred, 0))


##---- GLM ----
## 拟合关于时间t的线性回归模型
fresh.sales <- ts(fresh.train$sales)
t <- 1:length(fresh.sales)
t2 <- t^2
t3 <- t^3
t4 <- t^4
t5 <- t^5
fresh.glm.fit <- glm(fresh.sales ~ t + t2 + t3 + t4 + t5, family = gaussian())
summary(fresh.glm.fit)
# 模型显著，参数显著

## 拟合效果图
plot(fresh.sales, pch = 4)
lines(ts(fresh.glm.fit$fitted.values), col = 2)

## 预测
GLMPredFunc <- function(t, object) {
  coef <- object$coefficients
  pred <- sum(c(1, t, t^2, t^3, t^4, t^5) * coef)
  return(pred)
}

fresh.glm.pred <- fresh.test %>% 
  mutate(t = row_number(), 
         sales_pred = sapply(t, GLMPredFunc, object = fresh.glm.fit))


##---- GARCH ----
## ARCH test
for (i in 1:30) {
  fresh.q <- Box.test(fresh.arima.fit$residuals^2, lag = i, type = 'Ljung-Box')
  if (fresh.q$p.value > 0.05) {
    print(fresh.q)
  }
}

for (i in 1:30) {
  fresh.lm <- ArchTest(fresh.train$sales, lags = i)
  if (fresh.lm$p.value > 0.05) {
    print(fresh.lm)
  }
}

## 拟合GARCH(1,1)模型
fresh.garch.fit <- garch(fresh.arima.fit$residuals, order = c(1, 1))
summary(fresh.garch.fit)
# 不显著


##---- MSE-flexibility ----
fresh.arima.test.mse <- sum((fresh.arima.pred$sales - fresh.arima.pred$sales_pred)^2) / 
  (nrow(fresh.arima.pred) - 1)
fresh.rar.test.mse <- sum((fresh.rar.pred$sales[6:nrow(fresh.rar.pred)] - 
                             fresh.rar.pred$sales_pred[6:nrow(fresh.rar.pred)])^2) / 
  (nrow(fresh.rar.pred) - 5)
fresh.glm.test.mse <- sum((fresh.glm.pred$sales - fresh.glm.pred$sales_pred)^2) / 
  nrow(fresh.glm.pred)

mse.flex.test <- data.frame(flexibility = c(length(fresh.arima.fit$coef), 
                                            length(fresh.rar.fit$coef), 
                                            length(fresh.glm.fit$coefficients)), 
                            mse = c(fresh.arima.test.mse, 
                                    fresh.rar.test.mse, 
                                    fresh.glm.test.mse)) %>% 
  arrange(flexibility)

plot(mse.flex.test, type = 'p', pch = 4)
lines(lowess(mse.flex.test), col = 2)






