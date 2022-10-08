# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Machine Learning Homework 01
# Purpose:      Ink
# programmer:   Zhe Liu
# Date:         2022-10-06
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
ink.raw <- read_table('inks5_CLASSdataset.txt')

# train set1
ink.train1 <- ink.raw %>% 
  filter(Itemtype == 'TRAIN', stri_sub(Name, -2, -1) != '.1') %>% 
  mutate(label = as.factor(stri_sub(Name, -1, -1)))

# train set2
ink.train2 <- ink.raw %>% 
  filter(Itemtype == 'TRAIN', stri_sub(Name, -2, -1) == '.1') %>% 
  mutate(label = as.factor(stri_sub(Name, -3, -3)))

# test set
ink.test <- ink.raw %>% 
  filter(Itemtype == 'TEST') %>% 
  mutate(label = as.factor(stri_sub(Name, -1, -1)))


##---- Logistic ----
ink.logistic.fit1 <- glm(`1` ~ x + y + z, family = binomial(), data = ink.train1)
summary(ink.logistic.fit1)

ink.logistic.fit2 <- glm(`2` ~ x + y + z, family = binomial(), data = ink.train1)
summary(ink.logistic.fit2)


##---- Decision tree ----
# 条件推断树
ink.ctree.fit <- ctree(label ~ x + y + z, data = ink.train1)

ink.ctree.pred <- ink.test %>% 
  mutate(label_pred = predict(ink.ctree.fit, ink.test)) %>% 
  mutate(r = ifelse(label == label_pred, 1, 0))

# 递归分割树
ink.rpart.fit <- rpart(label ~ x + y + z, data = ink.train1, method = 'class')

ink.rpart.pred <- ink.test %>%
  mutate(label_pred = predict(ink.rpart.fit, ink.test, type = 'class')) %>% 
  mutate(r = ifelse(label == label_pred, 1, 0))


##---- Random forest of train1 ----
# pre-trained
set.seed(1)
ink.rf1.fit <- randomForest(label ~ x + y + z, data = ink.train1, 
                            xtest = ink.test[4:6], ytest = ink.test$label, 
                            importance = TRUE)
ink.rf1.fit

# hyper-parameter
ink.rf1.df <- data.frame(num = rep(1:10, each = 30), 
                         mtry = rep(1:3, each = 10, times = 10), 
                         ntree = rep((1:10) * 100, times = 30), 
                         obb = NA, 
                         test = NA)
set.seed(30)
for (i in 1:10) {
  for (j in 1:3) {
    for (k in seq(100, 1000, 100)) {
      rf.fit <- randomForest(
        label ~ x + y + z, data = ink.train1, 
        mtry = j, ntree = k, 
        xtest = ink.test[4:6], ytest = ink.test$label, 
        importance = TRUE
      )
      ink.rf1.df$obb[ink.rf1.df$num == i & ink.rf1.df$mtry == j & ink.rf1.df$ntree == k] <- mean(rf.fit$confusion[, 6])
      ink.rf1.df$test[ink.rf1.df$num == i & ink.rf1.df$mtry == j & ink.rf1.df$ntree == k] <- mean(rf.fit$test$confusion[, 6])
    }
  }
}

# fitting
set.seed(50)
ink.rf1.fit <- randomForest(label ~ x + y + z, data = ink.train1, 
                            mtry = 1, ntree = 100, 
                            xtest = ink.test[4:6], ytest = ink.test$label, 
                            importance = TRUE)
ink.rf1.fit


##---- Random forest of train2 ----
# pre-trained
set.seed(1)
ink.rf2.fit <- randomForest(label ~ x + y + z, data = ink.train2, 
                            xtest = ink.test[4:6], ytest = ink.test$label, 
                            importance = TRUE)
ink.rf2.fit

# hyper-parameter
ink.rf2.df <- data.frame(num = rep(1:10, each = 30), 
                         mtry = rep(1:3, each = 10, times = 10), 
                         ntree = rep((1:10) * 100, times = 30), 
                         obb = NA, 
                         test = NA)
set.seed(30)
for (i in 1:10) {
  for (j in 1:3) {
    for (k in seq(100, 1000, 100)) {
      rf.fit <- randomForest(
        label ~ x + y + z, data = ink.train2, 
        mtry = j, ntree = k, 
        xtest = ink.test[4:6], ytest = ink.test$label, 
        importance = TRUE
      )
      ink.rf2.df$obb[ink.rf2.df$num == i & ink.rf2.df$mtry == j & ink.rf2.df$ntree == k] <- mean(rf.fit$confusion[, 6])
      ink.rf2.df$test[ink.rf2.df$num == i & ink.rf2.df$mtry == j & ink.rf2.df$ntree == k] <- mean(rf.fit$test$confusion[, 6])
    }
  }
}

# fitting
set.seed(50)
ink.rf2.fit <- randomForest(label ~ x + y + z, data = ink.train1, 
                            mtry = 2, ntree = 100, 
                            xtest = ink.test[4:6], ytest = ink.test$label, 
                            importance = TRUE)
ink.rf2.fit


##---- Error ----
## train1 error
ink.train1.label <- ink.train1 %>% 
  mutate(prop = 1) %>% 
  pivot_wider(names_from = label, 
              values_from = prop, 
              values_fill = 0) %>% 
  select(`1`, `2`, `3`, `4`, `5`) %>% 
  as.matrix()

ink.train1.error <- colSums((ink.rf1.fit$votes - ink.train1.label)^2) / 50
ink.train1.error.total <- sum((ink.rf1.fit$votes - ink.train1.label)^2) / 50

## test error
ink.test.label <- ink.test %>% 
  mutate(prop = 1) %>% 
  pivot_wider(names_from = label, 
              values_from = prop, 
              values_fill = 0) %>% 
  select(`1`, `2`, `3`, `4`, `5`) %>% 
  as.matrix()

ink.test1.error <- colSums((ink.rf1.fit$test$votes - ink.test.label)^2) / 50
ink.test1.error.total <- sum((ink.rf1.fit$test$votes - ink.test.label)^2) / 50


##---- D10 ----
## histogram
source('Histogram.R')

h1 <- ink.rf1.fit$test$votes[1:10, 1]
h2 <- 1 - ink.rf1.fit$test$votes[1:10, 1]
histograms(LR.H1.exp = h1, LR.H2.exp = h2)

h1 <- ink.rf1.fit$test$votes[21:30, 2]
h2 <- 1 - ink.rf1.fit$test$votes[21:30, 2]
histograms(LR.H1.exp = h1, LR.H2.exp = h2)

h1 <- ink.rf1.fit$test$votes[41:50, 3]
h2 <- 1 - ink.rf1.fit$test$votes[41:50, 3]
histograms(LR.H1.exp = h1, LR.H2.exp = h2)

h1 <- ink.rf1.fit$test$votes[31:40, 4]
h2 <- 1 - ink.rf1.fit$test$votes[31:40, 4]
histograms(LR.H1.exp = h1, LR.H2.exp = h2)

h1 <- ink.rf1.fit$test$votes[11:20, 5]
h2 <- 1 - ink.rf1.fit$test$votes[41:50, 3]
histograms(LR.H1.exp = h1, LR.H2.exp = h2)

## tippett
source('Tippett.R')

h1 <- ink.rf1.fit$test$votes[1:10, 1]
h2 <- 1 - ink.rf1.fit$test$votes[1:10, 1]
Tippett(LR.H1.exp = h1, LR.H2.exp = h2)

h1 <- ink.rf1.fit$test$votes[21:30, 2]
h2 <- 1 - ink.rf1.fit$test$votes[21:30, 2]
Tippett(LR.H1.exp = h1, LR.H2.exp = h2)

h1 <- ink.rf1.fit$test$votes[41:50, 3]
h2 <- 1 - ink.rf1.fit$test$votes[41:50, 3]
Tippett(LR.H1.exp = h1, LR.H2.exp = h2)

h1 <- ink.rf1.fit$test$votes[31:40, 4]
h2 <- 1 - ink.rf1.fit$test$votes[31:40, 4]
Tippett(LR.H1.exp = h1, LR.H2.exp = h2)

h1 <- ink.rf1.fit$test$votes[11:20, 5]
h2 <- 1 - ink.rf1.fit$test$votes[41:50, 3]
Tippett(LR.H1.exp = h1, LR.H2.exp = h2)

## DET
source('DET.R')

h1 <- ink.rf1.fit$test$votes[1:10, 1]
h2 <- 1 - ink.rf1.fit$test$votes[1:10, 1]
DET(LR.H1.exp = h1, LR.H2.exp = h2)

h1 <- ink.rf1.fit$test$votes[21:30, 2]
h2 <- 1 - ink.rf1.fit$test$votes[21:30, 2]
DET(LR.H1.exp = h1, LR.H2.exp = h2)

h1 <- ink.rf1.fit$test$votes[41:50, 3]
h2 <- 1 - ink.rf1.fit$test$votes[41:50, 3]
DET(LR.H1.exp = h1, LR.H2.exp = h2)

h1 <- ink.rf1.fit$test$votes[31:40, 4]
h2 <- 1 - ink.rf1.fit$test$votes[31:40, 4]
DET(LR.H1.exp = h1, LR.H2.exp = h2)

h1 <- ink.rf1.fit$test$votes[11:20, 5]
h2 <- 1 - ink.rf1.fit$test$votes[41:50, 3]
DET(LR.H1.exp = h1, LR.H2.exp = h2)

## ECE
source('ECE.R')

h1 <- ink.rf1.fit$test$votes[1:10, 1]
h2 <- 1 - ink.rf1.fit$test$votes[1:10, 1]
ECE(LR.H1.exp = h1, LR.H2.exp = h2)

h1 <- ink.rf1.fit$test$votes[21:30, 2]
h2 <- 1 - ink.rf1.fit$test$votes[21:30, 2]
ECE(LR.H1.exp = h1, LR.H2.exp = h2)

h1 <- ink.rf1.fit$test$votes[41:50, 3]
h2 <- 1 - ink.rf1.fit$test$votes[41:50, 3]
ECE(LR.H1.exp = h1, LR.H2.exp = h2)

h1 <- ink.rf1.fit$test$votes[31:40, 4]
h2 <- 1 - ink.rf1.fit$test$votes[31:40, 4]
ECE(LR.H1.exp = h1, LR.H2.exp = h2)

h1 <- ink.rf1.fit$test$votes[11:20, 5]
h2 <- 1 - ink.rf1.fit$test$votes[41:50, 3]
ECE(LR.H1.exp = h1, LR.H2.exp = h2)



