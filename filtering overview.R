# filtering explanation


# library(arm)
# library(lme4)
library(data.table)
library(tidyverse)
library(magrittr)
library(lubridate)

rejects <- fread("full rejects.csv") %>% 
  filter(`Policy Code` == 0) %>% dplyr::select(-`Policy Code`) %>% 
  filter(!is.na(`Amount Requested`)) %>% filter(`Amount Requested` > 0) %>% 
  dplyr::select(`Amount Requested`, `Application Date`)

rejects$month <- month(ymd(rejects$`Application Date`), label = TRUE)



data <- fread("full accepted.csv") %>% 
  filter(policy_code == 1) %>%
  dplyr::select(amount = loan_amnt, date = issue_d) %>%
  filter(!is.na(amount)) %>% filter(amount > 0)
data %<>% separate(date, into = c("month", "year"), sep = "-", remove = TRUE) %>% 
  dplyr::select(-year)
data$month %<>% factor(., levels = levels(rejects$month), ordered= TRUE)


ggplot(data) + geom_bar(aes(x = month))
ggplot(data = rejects) + geom_bar(aes(x = month))

ggplot(data, aes(x = month, y = amount)) +
  stat_summary(fun.y = "median", geom = "col")

ggplot(data = rejects, aes(x = month, y = `Amount Requested`)) +
  stat_summary(fun.y = "median", geom = "col")

  
  
write_rds(data, "approved with months")
write_rds(rejects, "rejected with months")



colnames(rejects) <- colnames(data)

data$approved <- 1
rejects$approved <- 0

full_data <- bind_rows(data, rejects)
full_data$approved %<>% as.factor()
rm(data, rejects)


full_data %<>% filter(amount <= 40000) %>% filter(amount >= 1000)
full_data$emp_length %<>% str_extract("[[:digit:]]{1,2}") %>% as.integer()

full_data$round_amt <- ifelse((full_data$amount %% 5000 == 0), 1, 0)
full_data$emp51 <- ifelse((full_data$emp_length == 5 | full_data$emp_length == 1), 1, 0)
full_data$emp51 <- ifelse(is.na(full_data$emp_length), 0, full_data$emp51)
full_data %<>% filter(!is.na(emp_length))


full_data %<>% select(-risk_score)


write_csv(full_data, "full data 2015 without risk scores.csv")




