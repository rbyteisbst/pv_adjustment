#main script

#load packages
library(tidyverse)
library(readxl)
library(lubridate)
library(broom)

#pull data

#iso
#available here: https://www.iso-ne.com/static-assets/documents/2017/02/2017_smd_hourly.xlsx
smd_hourly <- read_excel("~/R/pv_adjustment/data/2017_smd_hourly.xlsx", sheet = "ISO NE CA") %>% 
  transmute(date = as.Date(Date), hour = as.integer(Hr_End),iso_load = RT_Demand)

#previously downloaded vwac
vt_net   <- read_csv("~/R/pv_adjustment/data/S_Vermont_Billing.csv") %>% 
  transmute(date = Date,hour = hour(Time),vt_net_load = `Forecast (MWh/h)`) %>% 
  filter(!is.na(vt_net_load))

vt_gross <- read_csv("~/R/pv_adjustment/data/S_Vermont_Billing (1).csv") %>% 
  transmute(date = Date,hour = hour(Time),vt_gross_load = `Forecast (MWh/h)`) %>% 
  filter(!is.na(vt_gross_load))

vt_pv    <- read_csv("~/R/pv_adjustment/data/S_Vermont_Billing (2).csv") %>% 
  transmute(date = Date,hour = hour(Time),vt_pv = `Forecast (MWh/h)`) %>% 
  filter(!is.na(vt_pv))

#combine
combo <- smd_hourly %>% 
  inner_join(vt_net) %>% 
  inner_join(vt_gross) %>% 
  inner_join(vt_pv)

#current
glance(lm(iso_load ~ vt_net_load,data = combo))

#suggested
glance(lm(iso_load ~ vt_gross_load + vt_pv,data = combo))
tidy(lm(iso_load ~ vt_gross_load + vt_pv,data = combo))

