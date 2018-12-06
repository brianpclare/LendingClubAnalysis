library(tidyverse)
library(data.table)
library(magrittr)


rej_q3 <- fread("RejectStats_2018Q3.csv")
  
rej_older <- fread("rejected_2007_to_2017.csv")
  
rejects <- bind_rows(rej_older, rej_q3)
write_csv(rejects, "full rejects.csv")

rm(rej_q3, rej_older, rejects)

################

app_q3 <- fread("LoanStats_2018Q3.csv")
app_older <- fread("accepted_2007_to_2017.csv")

app_q3$int_rate %<>% str_replace("%", "") %>% as.numeric()
app_q3$revol_util %<>% str_replace("%", "") %>% as.numeric()

accepted <- bind_rows(app_older, app_q3)


write_csv(accepted, "full accepted.csv")

#################
