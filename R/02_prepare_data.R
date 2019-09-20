# Prepare data retrieved in 01_get_data.R for regression  ----------------------


# Set up -----------------------------------------------------------------------

# Load packages
library(tidyverse)
library(lubridate)

# Load data generated in 01_get_data.R
load("data/Cleaned/indiv_tables.Rda")

# percent function
pc <- function(x) 100 * ( (x) - 1 )

# Combine data and generate derived variables ----------------------------------
time_data <- crossing(year = c(1980:2019),
                      quarter = c(1:4))

super_data <- reduce(list(time_data,
                          awote,
                          aena,
                          aawi,
                          inf_exp,
                          nairu,
                          nf_gdp,
                          unemp,
                          sg,
                          wpi_private),
       left_join, by = c("year", "quarter"))


super_data <- super_data %>%
  mutate(
    # AWOTE variants (the default awote is interpolated)
    awote_unfilled = awote,
      awote_unfilled_d1 = pc(awote_unfilled / lag(awote_unfilled, 1)),
      awote_unfilled_d4 = pc(awote_unfilled / lag(awote_unfilled, 4)),
    # Since 2012, AWOTE has been only twice yearly rather than quarterly;
    # Use linear interpolation to fill in the missing quarters
    awote = if_else(is.na(awote_unfilled),
                         (lead(awote_unfilled) + lag(awote_unfilled)) / 2,
                         awote_unfilled),
      awote_d1 = pc(awote / lag(awote)),
      awote_d4 = pc(awote / lag(awote, 4)),
    
    awote_prior_filled = if_else(is.na(awote_unfilled),
                                     lag(awote_unfilled),
                                     awote_unfilled),
      awote_prior_filled_d1 = pc(awote_prior_filled / lag(awote_prior_filled)),
      awote_prior_filled_d4 = pc(awote_prior_filled / lag(awote_prior_filled, 4)),
    
    # AENA
    aena = (coe_social + coe_wages) / employees,
      aena_d1 = pc(aena / lag(aena, 1)),
      aena_d4 = pc(aena / lag(aena, 4)),
    aena_social = coe_social / employees,
      aena_social_d1 = pc(aena_social / lag(aena_social, 1)),
      aena_social_d4 = pc(aena_social / lag(aena_social, 4)),
    aena_wages  = coe_wages / employees,
      aena_wages_d1 = pc(aena_wages / lag(aena_wages, 1)),
      aena_wages_d4 = pc(aena_wages / lag(aena_wages, 4)),
    awote_aena_mean_d1 = (awote_d1 + aena_wages_d1) / 2,
    awote_aena_mean_d4 = (awote_d4 + aena_wages_d4) / 2,
    
    # SG changes
    sg_small_d1 = sg_small - lag(sg_small, 1),
    sg_small_d4 = sg_small - lag(sg_small, 4),
    sg_d1 = sg - lag(sg, 1),
    sg_d2 = sg - lag(sg, 2),
    sg_d3 = sg - lag(sg, 3),
    sg_d4 = sg - lag(sg, 4),
    
    sg_other = if_else(year == 1993 & quarter <= 2, 4, sg),
    sg_other_d1 = sg_other - lag(sg_other, 1),
    sg_other_d2 = sg_other - lag(sg_other, 2),
    sg_other_d3 = sg_other - lag(sg_other, 3),
    sg_other_d4 = sg_other - lag(sg_other, 4),

    sg_lead1lag3 = lead(sg),
    sg_lead1lag3_d4 = sg_lead1lag3 - lag(sg_lead1lag3, 4),
  
    sg_lead2lag2 = lead(sg, 2),
    sg_lead2lag2_d4 = sg_lead2lag2 - lag(sg_lead2lag2, 4),
    
    sg_other_lead1lag3 = lead(sg_other),
    sg_other_lead1lag3_d4 = sg_other_lead1lag3 - lag(sg_other_lead1lag3, 4),
    
    sg_other_lead2lag2 = lead(sg_other, 2),
    sg_other_lead2lag2_d4 = sg_other_lead2lag2 - lag(sg_other_lead2lag2, 4),
    
    sg_max_d1 = if_else(sg_small_d1 > sg_d1, 
                        sg_small_d1, sg_d1),
    sg_max_d4 = if_else(sg_small_d4 > sg_d4, 
                        sg_small_d4, sg_d4),
    
    sg_early_d1 = if_else(year == 1992, 4, sg_d1),
    sg_early_d4 = if_else(year == 1992, 4, sg_d4),
    
    sg_with_award1 = if_else(year < 1992 | (year == 1992 & quarter <= 2),
                             1, sg),
    sg_with_award1_d1 = sg_with_award1 - lag(sg_with_award1, 1),
    sg_with_award1_d4 = sg_with_award1 - lag(sg_with_award1, 4),
    
    sg_with_award2 = if_else(year < 1992 | (year == 1992 & quarter <= 2),
                             2, sg),
    sg_with_award2_d1 = sg_with_award2 - lag(sg_with_award2, 1),
    sg_with_award2_d4 = sg_with_award2 - lag(sg_with_award2, 4),
    
    sg_with_award3 = if_else(year < 1992 | (year == 1992 & quarter <= 2),
                             3, sg),
    sg_with_award3_d1 = sg_with_award3 - lag(sg_with_award3, 1),
    sg_with_award3_d4 = sg_with_award3 - lag(sg_with_award3, 4),
    
    super_increase_d4 = sg_d4 > 0,
    super_introduction = (year == 1992 & quarter >= 3) | (year == 1993 & quarter <= 2),
    
    # NAIRU gap
    nairu_gap = unemp - nairu,
    nairu_gap5 = unemp - 5, # gap with constant NAIRU = 5% 

    # Unemployment (note that default is quarter-average unemployment)
    unemp_d1 = unemp - lag(unemp, 1),
    unemp_d4 = unemp - lag(unemp, 4),
    
    # GDP deflator
    nf_gdp_d4 = pc(nf_gdp / lag(nf_gdp, 4)),
    nf_gdp_d8 = pc(nf_gdp / lag(nf_gdp, 8))) %>%
  
  # Generate lagged variables
  mutate_at(vars(-quarter, -year), 
            .funs = list(lag1 = ~lag(., 1),
                         lag2 = ~lag(., 2),
                         lag3 = ~lag(., 3),
                         lag4 = ~lag(., 4),
                         lag8 = ~lag(., 8))) %>%
  # Filter to the same years used in McKell's paper
  filter(year >= 1992,
         year < 2017) 

# Save for use in regression ----------------------------------------------------
save(super_data,
     file = "data/Cleaned/super_data.Rda")



