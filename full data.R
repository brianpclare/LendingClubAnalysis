# library(arm)
# library(lme4)
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
rejects %<>% select(-`Application Date`) %>% filter(year >= 2015)


data <- fread("full accepted.csv") %>% 
  filter(policy_code == 1) %>%
  dplyr::select(amount = loan_amnt, Title = purpose, state = addr_state, 
         emp_length, risk_score = fico_range_high, debt_to_income = dti, date = issue_d) %>% 
  filter(!is.na(amount)) %>% filter(amount > 0)
data %<>% separate(date, into = c("month", "year"), sep = "-", remove = TRUE) %>% 
  select(-month) %>% filter(year >= 2015) %>% select(amount, Title, state, emp_length, risk_score,
                                                     debt_to_income, year)
data$year %<>% as.numeric()


#######
# data <- fread("accepted_2007_to_2017.csv") %>% 
#   select(amount = loan_amnt, Title = purpose, state = addr_state, 
#          emp_length, risk_score = fico_range_high, debt_to_income = dti,
#          date = issue_d, income = annual_inc, job_title = emp_title) %>% 
#   filter(!is.na(amount)) %>% filter(amount > 0)
# data %<>% separate(date, into = c("month", "year"), sep = "-", remove = TRUE) %>% 
#   select(-month)
# data$year %<>% as.numeric()
# 
# income_by_year <- data %>% group_by(year, job_title) %>% summarize(num = n(), avg_income = mean(income, na.rm = TRUE),
#                   max_income = max(income, na.rm = TRUE), millionaires = sum(income > 1000000, na.rm = TRUE))
# 
# data %<>% filter(income < 1000000) %>% filter(job_title != "") %>% filter(income > 0)
# data$job_title %<>% str_to_lower()
# income_by_year <- data %>% group_by(job_title) %>% 
#   summarize(num = n(), avg_income = mean(income, na.rm = TRUE),
#             max_income = max(income, na.rm = TRUE), 
#             millionaires = sum(income > 1000000, na.rm = TRUE))
# 
# jobs_by_year <- data %>% group_by(year) %>% summarize(loans = n(), 
#                                                       jobs = n_distinct(job_title))
#######





colnames(rejects) <- colnames(data)

data$approved <- 1
rejects$approved <- 0

full_data <- bind_rows(data, rejects)
full_data$approved %<>% as.factor()
rm(data, rejects)

# full_data$zip_code %<>% str_extract("[[:digit:]]{3}") %>% as.factor()
full_data %<>% filter(amount <= 40000) %>% filter(amount >= 1000)
full_data$emp_length %<>% str_extract("[[:digit:]]{1,2}") %>% as.integer()

full_data$round_amt <- ifelse((full_data$amount %% 5000 == 0), 1, 0)
full_data$emp51 <- ifelse((full_data$emp_length == 5 | full_data$emp_length == 1), 1, 0)
full_data$emp51 <- ifelse(is.na(full_data$emp_length), 0, full_data$emp51)
full_data %<>% filter(!is.na(emp_length))


full_data %<>% select(-risk_score)
#
approved <- full_data %>% filter(approved == "1")
rejected <- full_data %>% filter(approved == "0")
# 
# ggplot(data = approved) + geom_bar(aes(x = year, fill = as.factor(riskNA)))
# ggplot(data = rejected) + geom_bar(aes(x = year, fill = as.factor(riskNA)))

write_csv(full_data, "full data 2015 without risk scores.csv")

# full_data %<>% filter(!is.na(risk_score))
# full_data$high_FICO <- ifelse(full_data$risk_score > 660, 1, 0)
# 
# write_csv(full_data, "full data 2015 with risk scores.csv")

################################


