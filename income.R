library(tidyverse)
library(magrittr)

income <- read_csv("income_database.csv") %>% select(State_ab, Zip_Code, Mean, Median, Stdev, num_HH = sum_w)

income$Zip_Code %<>% str_extract("[[:digit:]]{3}") %>% as.factor()

income %<>% filter(num_HH > 0)

by_zip <- income %>% group_by(Zip_Code) %>% summarize(zip_mean = weighted.mean(Mean, num_HH), 
                                                      num_HH = sum(num_HH), State = max(State_ab))
rm(income)

zip_join <- by_zip %>% select(zip_code = Zip_Code, Mean_Income = zip_mean)

census <- read_csv("census ref.csv")
