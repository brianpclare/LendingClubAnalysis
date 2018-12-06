library(tidyverse)
library(magrittr)

census <- read_csv("acs2015_census_tract_data.csv", 
                   col_types = cols(.default = col_number(), CensusTract = col_character(), State = col_character(),
                                    County = col_character()))
  


# census$zip <- "0"

# for(i in 1:length(census$zip)){
#   t <- census$CensusTract[i]
#   zip <- zip_crossref$zip[zip_crossref$tract == t]
#   census$zip[i] <- zip
# }

# z <- zip_crossref %>% group_by(tract) %>% summarize(count = n())
# zip_crossref %>% filter(tract == "11001006202") %>% select(zip) %>% unique()
# 
# zip_list <- tibble(zip_crossref$zip %>% unique())
# colnames(zip_list) <- c("zip")

census_states <- census %>% group_by(State) %>% 
  summarize(population = sum(TotalPop), white = weighted.mean(White, TotalPop, na.rm = TRUE), black = weighted.mean(Black, TotalPop, na.rm = TRUE),
            hispanic = weighted.mean(Hispanic, TotalPop, na.rm = TRUE), asian = weighted.mean(Asian, TotalPop, na.rm = TRUE),
            native = weighted.mean(Native, TotalPop, na.rm = TRUE), pacific = weighted.mean(Pacific, TotalPop, na.rm = TRUE),
            med_income = weighted.mean(Income, TotalPop, na.rm = TRUE), poverty_rate = weighted.mean(Poverty, TotalPop, na.rm = TRUE),
            prof_jobs = weighted.mean(Professional, TotalPop, na.rm = TRUE), service_jobs = weighted.mean(Service, TotalPop, na.rm = TRUE),
            office_jobs = weighted.mean(Office, TotalPop, na.rm = TRUE), construction_jobs = weighted.mean(Construction, TotalPop, na.rm = TRUE),
            prod_jobs = weighted.mean(Production, TotalPop, na.rm = TRUE), unemployment = weighted.mean(Unemployment, TotalPop, na.rm = TRUE))

states <- tibble(state.name, state.abb)
DC <- c("District of Columbia", "DC")
PR <- c("Puerto Rico", "PR")
states[51, ] <- DC
states[52, ] <- PR
states %<>% arrange(state.name)

census_states$abb <- states$state.abb
rm(states)
census_states <- census_states %>% select(State, abb, everything())
write_csv(census_states, "census ref.csv")

census_states <- read_csv("census ref.csv")
skinny_census <- census_states %>% select(state = abb, population, white, med_income, poverty_rate)
write_csv(skinny_census, "skinny census states.csv")

  
#############################################

zip_crossref <- readxl::read_xlsx("ZIP_TRACT_092018.xlsx") %>% select(zip, tract)
# zip_crossref$zip %<>% str_extract("[[:digit:]]{3}")
zip_crossref %<>% rename(CensusTract = tract)

census <- inner_join(census, zip_crossref) %>% filter(!is.na(zip))
census$zip %<>% str_extract("[[:digit:]]{3}")


census_zips <- census %>% group_by(zip) %>% 
  summarize(population = sum(TotalPop), white = weighted.mean(White, TotalPop, na.rm = TRUE), black = weighted.mean(Black, TotalPop, na.rm = TRUE),
            hispanic = weighted.mean(Hispanic, TotalPop, na.rm = TRUE), asian = weighted.mean(Asian, TotalPop, na.rm = TRUE),
            native = weighted.mean(Native, TotalPop, na.rm = TRUE), pacific = weighted.mean(Pacific, TotalPop, na.rm = TRUE),
            med_income = weighted.mean(Income, TotalPop, na.rm = TRUE), poverty_rate = weighted.mean(Poverty, TotalPop, na.rm = TRUE),
            prof_jobs = weighted.mean(Professional, TotalPop, na.rm = TRUE), service_jobs = weighted.mean(Service, TotalPop, na.rm = TRUE),
            office_jobs = weighted.mean(Office, TotalPop, na.rm = TRUE), construction_jobs = weighted.mean(Construction, TotalPop, na.rm = TRUE),
            prod_jobs = weighted.mean(Production, TotalPop, na.rm = TRUE), unemployment = weighted.mean(Unemployment, TotalPop, na.rm = TRUE))

write_csv(census_zips, "census zip reference.csv")
