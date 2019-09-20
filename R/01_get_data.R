# Get data used in the McKell model --------------------------------------------

# Wage measures: AAWI, AWOTE, AENA
# SG minimum (for large employers?)
# Unemployment rate
# NAIRU
# Inflation expectations
# Non-farm GDP deflator

# Set up -----------------------------------------------------------------------

# Load packages
library(tidyverse)
library(lubridate)
library(readabs)
library(readxl)
library(OECD)


# Load and tidy AAWI -----------------------------------------------------------

aawi_url <- "https://www.ag.gov.au/industrial-relations/industrial-relations-publications/Documents/historical_approved.csv"

aawi_file_loc <- "data/AAWI/aawi.csv"

download.file(aawi_url, aawi_file_loc)

aawi <- read_csv("data/AAWI/aawi.csv", na = "*")

aawi <- aawi %>%
  rename(industry = 1,
         measure = 2)

aawi <- aawi %>%
  mutate(industry = if_else(industry == "", NA_character_, industry)) %>%
  fill(industry) %>%
  filter(!is.na(measure))

aawi <- aawi %>%
  filter(industry == "Total",
         grepl("AAWI", measure)) %>%
  gather(key = quarter, value = aawi, -industry, -measure) %>%
  select(-industry, -measure) %>%
  separate(quarter, into = c("quarter", "year"), sep = "-") %>%
  mutate(year = if_else(year >= 91, paste0("19", year), paste0("20", year)),
         year = parse_number(year),
         quarter = parse_number(quarter))


# Load and tidy AWOTE ----------------------------------------------------------

# Note: recent AWOTE (2012-present) is biannual. Prior to that was quarterly.
# We need to read in three separate spreadsheets - pre-1994, 1994-2012, 2012-present.

awote_pre_94 <- read_abs_local(filenames = "awote_pre_94.xls", path = "data/ABS/6302.0")
awote_pre_12 <- read_abs_local(filenames = "awote_pre_12.xls", path = "data/ABS/6302.0")
awote_recent <- read_abs("6302.0", 2)

tidy_awote <- function(df) {
  df %>%
    separate_series(remove_nas = TRUE, remove_totals = TRUE) %>%
    filter(series_2 == "Persons",
           series_3 == "Full Time",
           series_4 == "Adult",
           series_5 == "Ordinary time earnings") %>% 
    mutate(release_year = case_when(table_no == "awote_pre_94" ~ 2007,
                                    table_no == "awote_pre_12" ~ 2012,
                                    TRUE ~ 2019)) %>%
    select(date, awote = value, release_year) 
}

awote <- map_dfr(list(awote_pre_12, awote_pre_94, awote_recent),
    tidy_awote)  %>%
  group_by(date) %>%
  filter(release_year == max(release_year)) %>%
  ungroup() %>%
  select(-release_year) %>%
  arrange(date)

rm(awote_pre_12, awote_pre_94, awote_recent)

years <- crossing(year = c(1970:2019),
         quarter = c(1:4))

awote <- awote %>% 
  mutate(quarter = lubridate::quarter(date),
         year = lubridate::year(date)) %>%
  right_join(years) %>% 
  select(-date) 

# Load and tidy AENA------------------------------------------------------------

lfs <- read_abs("6202.0",c(1,19)) %>% 
      filter(series %in% c("Monthly hours worked in all jobs ;  Persons ;",
                           "Employed total ;  Persons ;"),
             lubridate::month(date) %in% seq(1, 12, 3),
             series_type=="Seasonally Adjusted") %>%
      mutate(quarter = lubridate::quarter(date),
             year = lubridate::year(date),
             series= case_when(series == "Monthly hours worked in all jobs ;  Persons ;" ~ "hours_worked_monthly",
                               series == "Employed total ;  Persons ;" ~ "employed_persons"),
             value=value*1000) %>%
      select(series,value,quarter,year) %>%
      spread(series,value)


aena <- read_abs("5206.0", table=c(7, 24))

aena_objects <- c("Average compensation per employee: Current prices ;",
                  "Average non-farm compensation per employee: Current prices ;",
                  "Compensation of employees ;",
                  "Compensation of employees - Employers' social contributions ;",
                  "Compensation of employees - Wages and salaries ;")


aena <- aena %>%
  filter(series %in% aena_objects,
         !is.na(value),
         series_type == "Seasonally Adjusted") %>%
  mutate(year = lubridate::year(date),
         quarter = lubridate::quarter(date),
         series = case_when(series == aena_objects[1] ~ "aena",
                            series == aena_objects[2] ~ "aena_non_farm" ,
                            series == aena_objects[3] ~ "coe" ,
                            series == aena_objects[4] ~ "coe_social",
                            series == aena_objects[5] ~ "coe_wages")) %>%
  select(year, quarter, series, value) %>% 
  spread(series, value) %>% 
  mutate(employees = coe / aena)

# Load and tidy unemployment rate ----------------------------------------------

unemp <- read_abs("6202.0", 1)

unemp <- unemp %>%
  separate_series() %>%
  filter(series_1 == "Unemployment rate",
         series_2 == "Persons",
         series_type == "Seasonally Adjusted") %>%
  mutate(year = lubridate::year(date),
         quarter = lubridate::quarter(date)) %>%
  group_by(year, quarter) %>% 
  mutate(unemp = mean(value)) %>% 
  filter(lubridate::month(date) %in% seq(1,12,3)) %>% 
  select(year, quarter, unemp, unemp_point = value)

  
  
# Load and tidy NAIRU ----------------------------------------------------------

# Use OECD Economic Outlook version
nairu_file_loc <- "data/OECD/nairu.csv"
  
nairu <- OECD::get_dataset("EO", "AUS.NAIRU.A") 

write_csv(nairu, nairu_file_loc)

nairu <- read_csv(nairu_file_loc)

nairu <- nairu %>%
  select(year = obsTime, nairu = obsValue)

nairu <- crossing(year = unique(nairu$year),
         quarter = c(1:4)) %>%
  right_join(nairu, by = "year")


# Load and tidy inflation expectations -----------------------------------------

  # Getting: Average annual inflation rate implied by the difference between 
  # 10-year nominal bond yield and 10-year inflation indexed bond yield; 
  # End-quarter observation

inf_exp_url <- "https://www.rba.gov.au/statistics/tables/xls/g03hist.xls?v=2019-09-13-14-40-56"
inf_exp_file_loc <- "data/RBA/inf_exp.xls"


download.file(inf_exp_url, inf_exp_file_loc)


inf_exp <- read_excel(inf_exp_file_loc, sheet = "Data", skip = 1)

inf_exp <- inf_exp %>%
  select(inf_exp = `Break-even 10-year inflation rate`,
         date = Title) %>%
  filter(row_number() >= 10,
         !is.na(inf_exp)) %>%
  mutate(date = as.Date(parse_number(date), origin = "1899-12-30"),
         year = lubridate::year(date),
         quarter = lubridate::quarter(date),
         inf_exp =as.numeric(inf_exp)) %>%
  select(-date)



# Load and tidy non-farm GDP deflator ------------------------------------------

nf_gdp <- read_abs(series_id = "A2302591K")

nf_gdp <- nf_gdp %>%
  mutate(year = lubridate::year(date),
         quarter = lubridate::quarter(date)) %>%
  select(nf_gdp = value, quarter, year)


# Get WPI for RBA comparison
# Table 3b. Total Hourly Rates of Pay Excluding Bonuses: Private Sector by State, Original (Quarterly Index Numbers) 

wpi_series <- c("Percentage Change from Previous Quarter ;  Total hourly rates of pay excluding bonuses ;  Australia ;  Private ;  All industries ;",
                "Percentage Change from Corresponding Quarter of Previous Year ;  Total hourly rates of pay excluding bonuses ;  Australia ;  Private ;  All industries ;")

wpi_raw <- read_abs("6345.0", "3b")

wpi_private <- wpi_raw %>% 
  filter(series %in% wpi_series) %>% 
  mutate(year = lubridate::year(date),
         quarter = lubridate::quarter(date),
         series = case_when(
           series == wpi_series[1] ~ "wpi_private_d1",
           series == wpi_series[2] ~ "wpi_private_d4"
         )) %>%
  select(year, quarter, series, value) %>% 
  pivot_wider(id_cols = c(year, quarter),
              names_from = series,
              values_from = value)

# Load and tidy SG rates -------------------------------------------------------

# These rates have been sourced from s 20 of the original Superannuation 
# Guarantee (Administration) Act: https://www.legislation.gov.au/Details/C2004C07278

sg_raw <- tibble::tribble(
             ~date, ~sg_small,       ~sg,  # Default 'sg' is for large businesses       
        "1-Jul-80",         0,         0,       
        "1-Jul-92",         3,         4,  
        "1-Jan-93",         3,         5,  
        "1-Jul-93",         3,         5,  
        "1-Jul-94",         4,         5,  
        "1-Jul-95",         5,         6,  
        "1-Jul-96",         6,         6,  
        "1-Jul-98",         7,         7,  
        "1-Jul-00",         8,         8,  
        "1-Jul-02",         9,         9,  
        "1-Jul-13",      9.25,      9.25,
        "1-Jul-14",       9.5,       9.5
        )


sg <- sg_raw %>%
  mutate(date = lubridate::dmy(date),
         year = lubridate::year(date),
         quarter = lubridate::quarter(date)) %>%
  select(-date)

sg <- crossing(year = c(1980:2019),
         quarter = c(1:4)) %>%
  left_join(sg, by = c("year", "quarter")) %>%
  fill(sg_small, sg) %>%
  filter(!is.na(sg))



# Save data --------------------------------------------------------------------
if (!dir.exists("data/Cleaned")) dir.create("data/Cleaned")

save(aawi, aena, awote, inf_exp, nairu, nf_gdp, sg, unemp, lfs, wpi_private,
     file = "data/Cleaned/indiv_tables.Rda")





