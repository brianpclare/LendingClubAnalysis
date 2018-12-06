# risk score analysis

library(data.table)
library(tidyverse)
library(magrittr)
library(lubridate)

rejects <- fread("full rejects.csv") %>% 
  filter(`Policy Code` == 0) %>% select(-`Policy Code`) %>%
  filter(!is.na(`Amount Requested`)) %>% filter(`Amount Requested` > 0) %>%
  dplyr::select(`Amount Requested`, `Loan Title`, State, 
                `Employment Length`, Risk_Score, `Debt-To-Income Ratio`, 
                `Application Date`)
rejects$`Debt-To-Income Ratio` %<>% str_replace("%", "") %>% as.numeric()
rejects$year <- year(ymd(rejects$`Application Date`))
rejects$month <- month(ymd(rejects$`Application Date`), label = TRUE)


rej_fico <- rejects %>% filter(year < 2013 | (year == 2013 & month < "Nov")) %>% 
  filter(Risk_Score > 0)
rej_vantage <- rejects %>% filter(year > 2013 | (year == 2013 & month >= "Nov")) %>% 
  filter(Risk_Score > 0)

summary(rej_fico$Risk_Score)
summary(rej_vantage$Risk_Score)

write_rds(rej_fico, "rej_fico")
write_rds(rej_vantage, "rej_vantage")

ggplot(data = rej_fico) + geom_histogram(aes(x = Risk_Score), binwidth = 30)
ggplot(data = rej_vantage) + geom_histogram(aes(x = Risk_Score), binwidth = 30)

###############################################

data <- fread("full accepted.csv") %>% 
  filter(policy_code == 1) %>%
  dplyr::select(amount = loan_amnt, Title = purpose, state = addr_state, 
                emp_length, risk_score = fico_range_high, debt_to_income = dti, date = issue_d) %>% 
  filter(!is.na(amount)) %>% filter(amount > 0)
data %<>% separate(date, into = c("month", "year"), sep = "-", remove = TRUE)

data %<>% dplyr::select(risk_score, year)
write_rds(data, "app_fico")

