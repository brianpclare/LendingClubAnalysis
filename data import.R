library(tidyverse)
library(data.table)
library(magrittr)
library(lubridate)

rejects <- fread("full rejects.csv")
rejects$`Application Date` %<>% ymd()
rejects$year <- year(rejects$`Application Date`)
by_year <- rejects %>% group_by(year) %>% summarize(num = n(), avg = mean(`Amount Requested`),
                  med = median(`Amount Requested`), med_risk = median(Risk_Score, na.rm = TRUE))

# rejects14  <- rejects %>%  filter(year(`Application Date`) == 2014)
# rejects14$`Loan Title` %<>% str_to_lower() %>% str_replace("[[:punct:]]", " ")
# by_title14 <- rejects14 %>% group_by(`Loan Title`) %>% summarize (n = n())
# rm(rejects14)

# ggplot(data = rejects %>% filter(`Amount Requested` < 50000)) + geom_histogram(aes(x = `Amount Requested`, fill = as.factor(year(`Application Date`))))


later <- rejects %>% filter(year >= 2015)
later$`Loan Title` %<>% str_to_lower() %>% str_replace("[[:punct:]]", " ")
by_title <- later %>% group_by(`Loan Title`) %>% summarize (n = n()) %>% filter(n > 1)


rejects$`Loan Title` <- ifelse(str_detect(rejects$`Loan Title`, "auto") | str_detect(rejects$`Loan Title`, "car"),
                     "automotive", rejects$`Loan Title`)
rejects$`Loan Title` <- ifelse(str_detect(rejects$`Loan Title`, "consol") | str_detect(rejects$`Loan Title`, "debt"),
                     "debt consolidation", rejects$`Loan Title`)

rej_by_title <- rejects %>% group_by(`Loan Title`) %>% summarize(num = n(), 
                                                                 avg_amt = mean(`Amount Requested`))
rej_by_title$result <- "Rejected"
rej_by_title %<>% filter(num > 50) %>% rename(title = `Loan Title`) %>% filter(title != "")

###############################

data <- fread("accepted_2007_to_2017.csv")
data %<>% select(-member_id, -url) %>% filter(!is.na(funded_amnt))
data$title %<>% str_to_lower() %>% str_replace("[[:punct:]]", " ")
data$title <- ifelse(str_detect(data$title, "auto") | str_detect(data$title, "car"),
                     "automotive", data$title)
data$title <- ifelse(str_detect(data$title, "consol") | str_detect(data$title, "debt"),
                     "debt consolidation", data$title)

################################

acc_by_title <- data %>% group_by(title) %>% summarize(num = n(), avg_amt = mean(loan_amnt))
acc_by_title$result <- "Approved"
acc_by_title %<>% filter(num > 50) %>% filter(title != "")


by_title <- bind_rows(acc_by_title, rej_by_title)
