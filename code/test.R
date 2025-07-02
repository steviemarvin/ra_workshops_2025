# load libraries
library(tidyverse)
library(epiextractr)
library(realtalk) #cpi_u_annual

# pull 2024 cpi for base year 
cpi_base <- filter(cpi_u_rs_extended_annual, year == 2024) %>% 
  pull(cpi_u_rs) 

# load cps basic 1979:2024
org_raw <- load_org(1979:2024, year, month, age, cow1, lfstat, wage, orgwgt)


# calculating annual average wages
annual_wages <- org_raw %>% 
  # filter: age restriction, no self employed, remove na wage values
  filter(age >= 16, lfstat %in% c(1,2), cow1 %in% c(1:5), !is.na(wage)) %>% 
  # left join cpi index from 1979:2024
  left_join(cpi_u_rs_extended_annual) %>% 
  # calculate real hourly wage
  mutate(real_wage = wage * (cpi_u_rs / cpi_base)) %>% 
  # calculate average annual real hourly wage
  summarize(real_av_wage = weighted.mean(real_wage, w = orgwgt, na.rm = TRUE),
            .by = year)

