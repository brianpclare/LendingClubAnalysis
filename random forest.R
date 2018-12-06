# random forest classifier

library(randomForest)
library(tidyverse)
library(data.table)
library(magrittr)


train <- read_csv("sample 100k full data.csv")
test <- read_csv("test 200k.csv")

data <- bind_rows(train, test) %>% select(-debt_to_income, -round_amt, -emp51)
  
data$approved %<>% as.factor()
data$state %<>% as.factor()
data$year %<>% as.factor()
data$Title %<>% as.factor()

train <- data[1:100000, ]
test <- data[100001:300000, ]
rm(data)

set.seed(89)
rf <- randomForest(data = train, approved ~ ., ntree = 300)

rf
rf$importance

write_rds(rf, "random forest")

# rf <- read_rds("random forest")

rf_preds <- predict(rf, test)
test_rf <- table(test$approved, rf_preds)

rf_acc <- (test_rf[1,1] + test_rf[2,2]) / (sum(test_rf))

rf_sens <- test_rf[2,2] / sum(test_rf[2 ,])
rf_spec <- test_rf[1,1] / sum(test_rf[1 ,])

plot(rf$err.rate[, 1])
