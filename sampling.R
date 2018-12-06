# title cleanup

library(data.table)
library(tidyverse)
library(magrittr)

full_data <- fread("full data 2015 without risk scores.csv")

full_data$Title %<>% str_to_lower() %>% str_replace("[[:punct:]]", " ")
full_data$Title <- ifelse(full_data$Title == "", "other", full_data$Title)
full_data$Title <- ifelse(str_detect(full_data$Title, "auto") |  str_detect(full_data$Title, "car") & !str_detect(full_data$Title, "card"),
                          "automotive", full_data$Title)
full_data$Title <- ifelse(str_detect(full_data$Title, "business"),
                          "business", full_data$Title)
full_data$Title <- ifelse(str_detect(full_data$Title, "moving"),
                          "moving", full_data$Title)
full_data$Title <- ifelse(str_detect(full_data$Title, "medical"),
                          "medical", full_data$Title)
full_data$Title <- ifelse(str_detect(full_data$Title, "consolid"),
                          "debt consolidation", full_data$Title)
full_data$Title <- ifelse(str_detect(full_data$Title, "credit card"),
                          "credit card refinancing", full_data$Title)
full_data$Title <- ifelse(str_detect(full_data$Title, "green") | str_detect(full_data$Title, "renewable"),
                          "green loan", full_data$Title)
full_data$Title <- ifelse(str_detect(full_data$Title, "learning") | str_detect(full_data$Title, "education"),
                          "education", full_data$Title)
full_data$Title <- ifelse(str_detect(full_data$Title, "house"),
                          "home buying", full_data$Title)


titles <- full_data %>% group_by(Title) %>% summarize(count = n(), mean_amt = mean(amount))

title_list <- titles$Title[titles$count > 1]

full_data$Title <- ifelse(full_data$Title %in% title_list, full_data$Title, "other")

# titles_final <- full_data %>% group_by(Title) %>% summarize(count = n(), mean_amt = mean(amount))

# summary(full_data$debt_to_income)
# bad_dti <- full_data %>% filter(debt_to_income < 0)
# no_dti <- full_data %>% filter(is.na(debt_to_income))
# 
# ggplot(data = full_data) + geom_bar(aes(x = year, fill = as.factor(debt_to_income > 0)))

full_data %<>% filter(debt_to_income > 0)
# summary(full_data$debt_to_income)
# ggplot(full_data) + geom_histogram(aes(x = log(debt_to_income + 0.01), fill = as.factor(approved)))
# summary( (full_data$debt_to_income[full_data$approved == 1] + 0.01) %>% log() )

full_data %<>% mutate(log_dti = log(debt_to_income + 0.01))

write_csv(full_data, "FINAL DATA.csv")

######################################
full_data <- fread("full data 2015 without risk scores.csv")

accepted <- full_data %>% filter(approved == 1)
declined <- full_data %>% filter(approved == 0)


set.seed(14)
sample_accepted <- accepted %>% sample_n(50000)
sample_declined <- declined %>% sample_n(50000)
rm(accepted, declined)

sample_data <- bind_rows(sample_accepted, sample_declined)

write_csv(sample_data, "sample 100k full data.csv")

test_data <- full_data %>% sample_n(200000)
write_csv(test_data, "test 200k.csv")
