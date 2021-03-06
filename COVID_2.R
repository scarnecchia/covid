rm(list = ls(all = TRUE))
setwd("D:/Github/COVID")
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
gc()

library(rjson)
library(jsonlite)
library(RCurl)
library(tidyverse)
library(ggthemes)
library(lubridate)
library(usmap)
library(directlabels)
#library(knitr)
#library(kableExtra)
library(censusapi)

# Sys.setenv(CENSUS_KEY="") # Set your Census API Key in .REnviron here
Sys.getenv("CENSUS_KEY") # Get your Census API Key from .REnviron here

# Daily Data
state_url = URLencode("https://covidtracking.com/api/states/daily")
us_url = URLencode("https://covidtracking.com/api/us/daily")

# FUNCTIONS
# Fetches daily data from API
fetch_data = function(url, endpoint){
  full_url = paste0(url, endpoint)
  base_url = URLencode(full_url)
  df = fromJSON(base_url)
  
  return(df)
}

# Function for calculating days_since_n, where n is number and sort_group is event
std_date_to_n <- function(df, n, sort_group){
  sort_group = enquo(sort_group)
  df <- df %>% filter(death >= n) %>% 
    group_by(!!sort_group) %>% 
    mutate('days_since_n' = row_number()-1)
  
  return(df)
}

# Fetch and Clean_up Data from NYTimes for earlier data. Annoying to clean.

# ny_times = read.csv("covid-19-data/us-states.csv", fileEncoding="UTF-8-BOM") %>% 
#   select(date,fips,cases,deaths) %>% 
#   rename(positive = cases, death = deaths) %>% 
#   add_column(negative=NA, .before = 4) %>%
#   add_column(hospitalized=NA, .before = 5) %>% 
#   add_column(state=NA, .before = 2) %>% 
#   select(-fips,everything()) %>% 
#   mutate(fips = sprintf("%02d",fips)) %>% 
#   mutate(state = as.character(state))
# 
#
# fips = read.csv("states.csv", header=TRUE)
# state = fips$abbr
# fips = fips$fips
# 
# lookup = setNames(state, fips)
# ny_times <- transform(ny_times, state=lookup[fips], stringsAsFactors=FALSE) %>% 
#   mutate(date = as.Date(ymd(date))) %>% 
#   filter(date < "2020-03-04")
# write.csv(nytimes, "nytimes_covid.csv")

ny_times = read.csv(file="nytimes_covid.csv") %>%
  select(-X) %>%
  mutate(date = as.Date(ymd(date))) %>%
  mutate(state = as.character(state)) %>%
  mutate(negative = as.integer(negative)) %>%
  mutate(hospitalized = as.integer(hospitalized)) %>%
  mutate(fips = as.character(fips)) %>%
  add_column(total=NA, .before = 7)

state_pop = getCensus(name = "2019/pep/population", vars=c("STATE","POP"), region="state:*") # Get State Population Data

cum_state = fetch_data(state_url, "") %>% 
  mutate(date = ymd(date)) %>% 
  select(date,state,positive,negative,hospitalized,death,total,fips) %>%
  bind_rows(ny_times) %>%
  #distinct(date, .keep_all = TRUE) %>% 
  group_by(state) %>% 
  arrange(positive, .by_group = TRUE) %>% 
  std_date_to_n(1, state) %>% 
  filter(state %in% c("NY", "CA", "TX", "MA", "VT", "CT","RI","NH", "ME")) %>% 
  add_column(population=NA) %>% 
  add_column(per_capita = NA) %>% 
  add_column(per_test = NA) %>% 
  add_column(cfr = NA) 

cum_state$population = state_pop$POP[match(cum_state$fips, state_pop$state)]

cum_state = cum_state %>% 
  ungroup() %>% 
  drop_na(population) %>%
  mutate(population = as.integer(population)) %>% 
  mutate(per_capita = (death / population)*100000) %>% # Deaths Per 100K
  mutate(per_test = (total / population)*100000) %>% # Tests Per 100K
  mutate(cfr = (death / positive)*100) %>% # % Deaths from Positive Cases
  group_by(state) %>% 
  #mutate(average_cfr = cumsum(cfr) / seq_along(cfr)) %>% 
  group_by(date, state)

# Plots

## Case Fatality Rate
g_cfr <- ggplot(cum_state, aes(x=days_since_n, y=cfr, colour=state)) +
  geom_path() + scale_y_continuous(breaks = extended_range_breaks()(cum_state$cfr)) +
  ggtitle("Case Fatality Rate") + xlab("Days since First Death") + ylab("Percent of Known Positive Cases Resulting in Death")
g_cfr + geom_rangeframe() + theme_tufte(base_size = 11, base_family = "serif", ticks = TRUE) +
geom_dl(aes(label = state), method = list(dl.trans(x = x + .2), "last.points", fontsize="8", fontfamily="serif"))

g_pcd <- ggplot(cum_state, aes(x=days_since_n, y=per_capita, colour=state)) +
  geom_path() + scale_y_continuous(breaks = extended_range_breaks()(cum_state$per_capita)) +
  ggtitle("COVID-19 Deaths Per 100000 Residents") + xlab("Days since First Death") + ylab("Deaths Per 100000 Residents")
g_pcd + geom_rangeframe() + theme_tufte(base_size = 11, base_family = "serif", ticks = TRUE) +
  geom_dl(aes(label = state), method = list(dl.trans(x = x + .2), "last.points", fontsize="8", fontfamily="serif"))

g_pct <- ggplot(cum_state, aes(x=days_since_n, y=per_test, colour=state)) +
  geom_path() + scale_y_continuous(breaks = extended_range_breaks()(cum_state$per_test)) +
  ggtitle("COVID-19 Tests Per 100000 Residents") + xlab("Days since First Death") + ylab("Tests Per 100000 Residents")
g_pct + geom_rangeframe() + theme_tufte(base_size = 11, base_family = "serif", ticks = TRUE) +
  geom_dl(aes(label = state), method = list(dl.trans(x = x + .2), "last.points", fontsize="8", fontfamily="serif"))

## US
us_pop = getCensus(name = "2019/pep/population", vars=c("NAME","POP"), region="us:*")

cum_us = fetch_data(us_url, "") %>% 
  arrange(positive) %>% 
  std_date_to_n(1) %>% 
  select(date,positive,negative,hospitalized,death,total,days_since_n) %>% 
  mutate(date = ymd(date)) %>% 
  group_by(date) %>% 
  add_column(state="United States", .before = 2) %>% 
  add_column(population=NA) %>% 
  add_column(per_capita = NA) %>% 
  add_column(per_test = NA) %>% 
  add_column(cfr = NA)

cum_us$population = us_pop$POP[match(cum_us$state, us_pop$NAME)]

cum_us = cum_us %>% 
  ungroup() %>% 
  drop_na(population) %>%
  mutate(population = as.integer(population)) %>% 
  mutate(per_capita = (death / population)*100000) %>% 
  mutate(per_test = (total / population)*100000) %>%
  mutate(cfr = (death / positive)*100) %>% 
  mutate(state = gsub("United States", "US", state)) %>% 
  group_by(date)

us_cfr <- ggplot(cum_us, aes(x=days_since_n, y=cfr, colour=state)) +
  geom_path() + scale_y_continuous(breaks = extended_range_breaks()(cum_us$cfr)) +
  ggtitle("Number of COVID-19 Deaths") + xlab("Days since First Death") + ylab("Percent of Known Positive Cases Resulting in Death")
us_cfr + geom_rangeframe() + theme_tufte(base_size = 11, base_family = "serif", ticks = TRUE) +
  geom_dl(aes(label = state), method = list(dl.trans(x = x + .2), "last.points", fontsize="8", fontfamily="serif"))

us_pcd <- ggplot(cum_us, aes(x=days_since_n, y=per_capita, colour=state)) +
  geom_path() + scale_y_continuous(breaks = extended_range_breaks()(cum_us$per_capita)) +
  ggtitle("COVID-19 Deaths Per 100000 Residents") + xlab("Days since First Death") + ylab("Deaths Per 100000 Residents")
us_pcd + geom_rangeframe() + theme_tufte(base_size = 11, base_family = "serif", ticks = TRUE) +
  geom_dl(aes(label = state), method = list(dl.trans(x = x + .2), "last.points", fontsize="8", fontfamily="serif"))

us_pct <- ggplot(cum_us, aes(x=days_since_n, y=per_test, colour=state)) +
  geom_path() + scale_y_continuous(breaks = extended_range_breaks()(cum_us$per_test)) +
  ggtitle("COVID-19 Tests Per 100000 Residents") + xlab("Days since First Death") + ylab("Tests Per 100000 Residents")
us_pct + geom_rangeframe() + theme_tufte(base_size = 11, base_family = "serif", ticks = TRUE) +
  geom_dl(aes(label = state), method = list(dl.trans(x = x + .2), "last.points", fontsize="8", fontfamily="serif"))

g_us <- ggplot(cum_us, aes(x=days_since_n)) +
  geom_line(aes(y=death), colour = "darkred") +
  geom_line(aes(y=total), colour = "blue")
g_us + scale_y_continuous(trans='log2') + geom_rangeframe() + theme_tufte(base_size = 11, base_family = "serif", ticks = TRUE)

# US + States
cum_all = bind_rows(cum_state, cum_us)

g_all <- ggplot(cum_all, aes(x=days_since_n, y=death, colour=state)) +
  geom_path() + #scale_y_continuous(breaks = extended_range_breaks()) +
  ggtitle("Number of COVID-19 Deaths Since First Death") + xlab("Days since First Death") + ylab("Deaths")
g_all + geom_rangeframe() + theme_tufte(base_size = 11, base_family = "serif", ticks = TRUE) +
  geom_dl(aes(label = state), method = list(dl.trans(x = x + .2), "last.points", fontsize=8, fontfamily="serif"))

all_cfr <- ggplot(cum_all, aes(x=days_since_n, y=cfr, colour=state)) +
  geom_path() + scale_y_continuous(breaks = extended_range_breaks()(cum_all$cfr)) +
  ggtitle("Number of COVID-19 Deaths") + xlab("Days since First Death") + ylab("Percent of Known Positive Cases Resulting in Death")
all_cfr + geom_rangeframe() + theme_tufte(base_size = 11, base_family = "serif", ticks = TRUE) +
  geom_dl(aes(label = state), method = list(dl.trans(x = x + .2), "last.points", fontsize="8", fontfamily="serif"))

all_pcd <- ggplot(cum_all, aes(x=days_since_n, y=per_capita, colour=state)) +
  geom_path() + scale_y_continuous(breaks = extended_range_breaks()(cum_all$per_capita)) +
  ggtitle("COVID-19 Deaths Per 100000 Residents") + xlab("Days since First Death") + ylab("Deaths Per 100000 Residents")
all_pcd + geom_rangeframe() + theme_tufte(base_size = 11, base_family = "serif", ticks = TRUE) +
  geom_dl(aes(label = state), method = list(dl.trans(x = x + .2), "last.points", fontsize="8", fontfamily="serif"))

all_pct <- ggplot(cum_all, aes(x=days_since_n, y=per_test, colour=state)) +
  geom_path() + scale_y_continuous(breaks = extended_range_breaks()(cum_all$per_test)) +
  ggtitle("COVID-19 Tests Per 100000 Residents") + xlab("Days since First Death") + ylab("Tests Per 100000 Residents")
all_pct + geom_rangeframe() + theme_tufte(base_size = 11, base_family = "serif", ticks = TRUE) +
  geom_dl(aes(label = state), method = list(dl.trans(x = x + .2), "last.points", fontsize="8", fontfamily="serif"))