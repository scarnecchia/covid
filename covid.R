setwd("D:/Github/COVID")
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")

library(tidyverse)
library(tibble)
library(lubridate)
library(fs)

path = ("COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/*.csv")
#pattern =("*.csv")

data_list = lapply(Sys.glob(path), read.csv, fileEncoding="UTF-8-BOM")

#data_list = list.files(path, pattern = "csv$", full.names = TRUE)

directory_names = Sys.glob(path)
directory_names = path_file(directory_names)
directory_names = path_ext_remove(directory_names)
directory_names = as.Date(mdy(directory_names))

## Bind Date to Data
data_list = mapply(cbind, data_list, "Date"=directory_names, simplify = F)

data_list <- lapply(data_list, function(x) setNames(x, gsub("(\\w+)\\.(\\w+)", "\\1_\\2", names(x))))
data_list <- lapply(data_list, function(x) x %>% select(Province_State, Country_Region, Confirmed, Deaths, Recovered, Date))

data = bind_rows(data_list, .id = "column_label") 

## Bind Date to Data

death_df = data %>% 
  select(Province_State, Country_Region, Date, Confirmed, Deaths, Recovered) %>% 
  filter(Country_Region == "US") %>% 
  mutate(Province_State = gsub("^\\D+,\\s(\\D+)","\\1",Province_State)) %>% 
  mutate(Province_State = gsub("\\s\\(\\D+\\)","",Province_State)) %>% 
  mutate(Province_State = gsub("(\\w+)\\.(\\w+)\\.", "\\1\\2",Province_State)) %>%
  mutate(Province_State = gsub("Grand Princess Cruise Ship", "Grand Princess",Province_State)) %>% 
  mutate(Province_State = gsub("Chicago", "IL",Province_State)) %>% 
  mutate(Province_State = gsub("District of Columbia", "DC",Province_State)) %>% 
  mutate(Province_State = gsub("Puerto Rico", "PR",Province_State)) %>% 
  mutate(Province_State = gsub("Guam", "GU",Province_State)) %>% 
  mutate(Province_State = gsub("Virgin Islands", "VI",Province_State)) %>% 
  mutate(Province_State = gsub("United States VI", "VI",Province_State)) %>% 
  mutate(Province_State = gsub("American Samoa", "AS",Province_State)) %>% 
  mutate(Province_State = gsub("Northern Mariana Islands", "MP",Province_State)) %>% 
  filter(Province_State != "Recovered") %>% 
  filter(Province_State != "Wuhan Evacuee") %>% 
  filter(Province_State != "US") %>% 
  filter(Province_State != "Unassigned Location") %>%
  filter(Province_State != "Diamond Princess") %>%
  filter(Province_State != "Grand Princess") %>%
  mutate(State = setNames(state.abb, state.name)[Province_State]) %>% 
  mutate(State = coalesce(State,Province_State)) %>% 
  select(-Province_State) %>% 
  rename(Province_State = State) %>% 
  select(Province_State, everything())

write.csv(death_df, file="covid_us_daily_report.csv")

# us_time_series_deaths = death_df %>% 
#   select(Date, State, Deaths) %>% 
#   group_by(State, Date, Deaths) %>% 
#   mutate(row = row_number()) %>% 
#   pivot_wider(id_cols = "State", names_from = "Date", values_from = "Deaths") %>% 
#   select(-row)
# 
# g <- ggplot(death_df, aes(x=Date, y=Deaths, colour=State)) +
#   geom_path(na.rm=TRUE) + scale_y_log10()
# g
# 
# which(duplicated(death_df))

## Growth in Tracking DB size
# jhu_df = dir_info("COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/", glob = "*.csv") %>%
#   select(path, size) %>% 
#   mutate(path = path_file(path)) %>% 
#   mutate(path = path_ext_remove(path)) %>% 
#   rename(date = path) %>% 
#   mutate(date = mdy(date)) %>% 
#   mutate(date = ymd(date)) %>% 
#   mutate(size = str_remove_all(size, "[\\D]")) %>% 
#   mutate(size = as.numeric(size)) %>% 
#   arrange(size)
#   
# g <- ggplot(jhu_df, aes(x=as.Date(date), y=size)) +
#   geom_point() + scale_y_log10()
# g