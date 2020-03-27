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
directory_names = mdy(directory_names)

## Bind Date to Data
data_list = mapply(cbind, data_list, "date"=directory_names, simplify = F)

data = bind_rows(data_list, .id = "column_label") 

## Bind Date to Data


death_df = data %>% 
  select(Province_State, Country_Region, date, Confirmed, Deaths, Recovered) %>% 
  filter(Country_Region == "US")

## Growth in Tracking DB size
jhu_df = dir_info("COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/", glob = "*.csv") %>%
  select(path, size) %>% 
  mutate(path = path_file(path)) %>% 
  mutate(path = path_ext_remove(path)) %>% 
  rename(date = path) %>% 
  mutate(date = mdy(date)) %>% 
  mutate(date = ymd(date)) %>% 
  mutate(size = str_remove_all(size, "[\\D]")) %>% 
  mutate(size = as.numeric(size)) %>% 
  arrange(size)
  
g <- ggplot(jhu_df, aes(x=as.Date(date), y=size)) +
  geom_point() + scale_y_log10()
g



+ scale_y_continuous(labels = comma, breaks = seq(from = 0, to = 400000000, by = 50000000)) +
  ggtitle("Growth of the Sentinel Distributed Database") + xlab("Date") + ylab("PatIDs") + scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_excel_new() + scale_colour_excel_new()
g + theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
  ggsave(sprintf("sdd_growth_OY%s.png", current_oy), height = 7 , width = 16, dpi="retina")