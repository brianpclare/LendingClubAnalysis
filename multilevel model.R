#MLM with the sampled data

library(arm)
library(lme4)
library(tidyverse)
library(data.table)
library(magrittr)

data <- fread("sample 100k full data.csv") %>% as.tibble()
data$round_amt %<>% as.factor()
data$emp51 %<>% as.factor()
data$state %<>% as.factor()
data$year %<>% as.factor()



mlm <- glmer(data = data, approved ~ log_dti +
               (log_dti|emp51),
             family = binomial(link = "logit"))
display(mlm)

write_rds(mlm, "mlm simple")

mlm <- read_rds("balanced mlm")



# mlm2 <- glmer(data = data, approved ~ log_dti*emp51*round_amt +
#                (log_dti|Title),
#              family = binomial(link = "logit"))
# display(mlm2)
# 
# write_rds(mlm2, "balanced mlm2")
# 
# mlm2 <- read_rds("balanced mlm2")


m4 <- glm(data = data, approved ~ log_dti * emp51,
              family = binomial(link = "logit"))
display(m4)
write_rds(m4, "glm model")
#####
m4 <- read_rds("glm model")
#####

census <- read_csv("census ref.csv")
census %<>% select(-State) %>% rename(state = abb)
census$state %<>% as.factor()

data %<>% left_join(census)

mlm5 <- glmer(data = data, approved ~ rescale(risk_score) + rescale(med_income) +
                (risk_score|emp51) + (risk_score|round_amt) + (risk_score|state),
              family = binomial(link = "logit"))
display(mlm5)

write_rds(mlm5, "mlm5")
#####
mlm5 <- read_rds("mlm5")
#####

#########################################

mlm_preds <- invlogit(predict(mlm, data, allow.new.levels = TRUE))

data$preds <- mlm_preds
data$preds <- ifelse(data$preds > 0.5, 1, 0)

table <- table(data$approved, data$preds)

acc <- (table[1,1] + table[2,2]) / (sum(table))

sens <- table[2,2] / sum(table[2 ,])
spec <- table[1,1] / sum(table[1 ,])

###########################################
#test set
test <- read_csv("test 200k.csv")
test$emp51 %<>% as.factor()
test$round_amt %<>% as.factor()
# test %<>% left_join(census)

test_preds <- invlogit(predict(mlm, test, allow.new.levels = TRUE))

test$preds <- ifelse(test_preds > 0.5, 1, 0)

test_table <- table(test$approved, test$preds)

test_acc <- (test_table[1,1] + test_table[2,2]) / (sum(test_table))

test_sens <- test_table[2,2] / sum(test_table[2 ,])
test_spec <- test_table[1,1] / sum(test_table[1 ,])

#############################################
# with the plain glm

glm_preds <- invlogit(predict(m4, data, allow.new.levels = TRUE))

data$preds2 <- glm_preds
data$preds2 <- ifelse(data$preds2 > 0.5, 1, 0)

table2 <- table(data$approved, data$preds2)

acc2 <- (table2[1,1] + table2[2,2]) / (sum(table2))

sens2 <- table2[2,2] / sum(table2[2, ])
spec2 <- table2[1,1] / sum(table2[1, ])

###########################################
#test set
test2 <- read_csv("test 200k.csv")
# test2 %<>% left_join(census)
test2$round_amt %<>% as.factor()
test2$emp51 %<>% as.factor()
test2 %<>% filter(Title != "education")

test_preds2 <- invlogit(predict(m4, test2, allow.new.levels = TRUE))

test2$preds <- ifelse(test_preds2 > 0.5, 1, 0)

test_table2 <- table(test2$approved, test2$preds)

test_acc2 <- (test_table2[1,1] + test_table2[2,2]) / (sum(test_table2))

test_sens2 <- test_table2[2,2] / sum(test_table2[2, ])
test_spec2 <- test_table2[1,1] / sum(test_table2[1, ])
