# Run regression analysis with data generated in 02_prepare_data.R  ------------


# Set up -----------------------------------------------------------------------

# Load packages
library(tidyverse)
library(jtools) # nice presentation tables


# Load data generated in 02_prepare_data.R
load("data/Cleaned/super_data.Rda")


# Model 1: Recreation of McKell models -----------------------------------------

# AWOTE
mck_awote <- awote_d4 ~    
  sg_d4  +  
  nairu_gap_lag4 +     
  unemp_d4_lag4 + 
  inf_exp_lag4 +         
  nf_gdp_d8_lag4

mck_awote_model <- lm(mck_awote, 
                      data = super_data)

# AENA (wages)
mck_aena_wages <- aena_wages_d4 ~    
  sg_d4  +  
  nairu_gap_lag4 +     
  unemp_d4_lag4 + 
  inf_exp_lag4 +         
  nf_gdp_d8_lag4

mck_aena_wages_model <- lm(mck_aena_wages, 
                           data = super_data)

# AENA (social)
mck_aena_social <- aena_social_d4 ~    
  sg_d4  +  
  nairu_gap_lag4 +     
  unemp_d4_lag4 + 
  inf_exp_lag4 +         
  nf_gdp_d8_lag4

mck_aena_social_model <- lm(mck_aena_social, 
                            data = super_data)

# Results table
model1 <- export_summs(mck_awote_model, mck_aena_wages_model, mck_aena_social_model,
             error_format = "(p = {p.value})",
             stars = c('***' = 0.01, '**' = 0.05, '*' = 0.10),
             error_pos = "right",
             model.names = c("AWOTE",
                             "AENA\n(wages)",
                             "AENA\n(social)"),
             coefs =  c("sg_d4",
                        "nairu_gap_lag4",
                        "unemp_d4_lag4",
                        "inf_exp_lag4",
                        "nf_gdp_d8_lag4"))

model1



# Model 2: McKell model with pre-SG award --------------------------------------

# AWOTE
mck_awote_with_award2 <- awote_d4 ~    
  sg_with_award2_d4  +  
  nairu_gap_lag4 +     
  unemp_d4_lag4 + 
  inf_exp_lag4 +         
  nf_gdp_d8_lag4

mck_awote_model_with_award2 <- lm(mck_awote_with_award2, 
                                  data = super_data)

# AENA (wages)
mck_aena_wages_with_award2 <- aena_wages_d4 ~    
  sg_with_award2_d4  +  
  nairu_gap_lag4 +     
  unemp_d4_lag4 + 
  inf_exp_lag4 +         
  nf_gdp_d8_lag4

mck_aena_wages_model_with_award2 <- lm(mck_aena_wages_with_award2, 
                                       data = super_data)

# AENA (social)
mck_aena_social_with_award2 <- aena_social_d4 ~    
  sg_with_award2_d4  +  
  nairu_gap_lag4 +     
  unemp_d4_lag4 + 
  inf_exp_lag4 +         
  nf_gdp_d8_lag4

mck_aena_social_model_with_award2 <- lm(mck_aena_social_with_award2, 
                                        data = super_data)

# Results table
model2 <- export_summs(mck_awote_model_with_award2, 
             mck_aena_wages_model_with_award2,
             mck_aena_social_model_with_award2,
             error_format = "(p = {p.value})",
             stars = c('***' = 0.01, '**' = 0.05, '*' = 0.10),
             error_pos = "right",
             model.names = c("AWOTE (with pre-SG rate)",
                             "AENA (wages) (with pre-SG rate)",
                             "AENA (social) (with pre-SG rate)"),
             coefs =  c("sg_with_award2_d4",
                        "nairu_gap_lag4",
                        "unemp_d4_lag4",
                        "inf_exp_lag4",
                        "nf_gdp_d8_lag4"))

model2


# Model 3: McKell model with lagged SG one-quarter change ----------------------

# AWOTE
mck_awote_d1_lag2 <- awote_d4 ~    
  sg_d1_lag2 +
  nairu_gap_lag4 +
  unemp_d4_lag4 +
  inf_exp_lag4 +
  nf_gdp_d8_lag4

mck_awote_d1_lag2_model <- lm(mck_awote_d1_lag2, 
                              data = super_data)

# AENA (wages)
mck_aena_wages_d1_lag2 <- aena_wages_d4 ~
  sg_d1_lag2 +
  nairu_gap_lag4 +
  unemp_d4_lag4 +
  inf_exp_lag4 +
  nf_gdp_d8_lag4

mck_aena_wages_d1_lag2_model <- lm(mck_aena_wages_d1_lag2, 
                                   data = super_data)


# AENA (wages)
mck_aena_social_d1_lag2 <- aena_social_d4 ~
  sg_d1_lag2 +
  nairu_gap_lag4 +
  unemp_d4_lag4 +
  inf_exp_lag4 +
  nf_gdp_d8_lag4

mck_aena_social_d1_lag2_model <- lm(mck_aena_social_d1_lag2, 
                                   data = super_data)



# Results table
model3 <- export_summs(mck_awote_d1_lag2_model, 
             mck_aena_wages_d1_lag2_model, 
             mck_aena_social_d1_lag2_model,
             error_format = "(p = {p.value})",
             stars = c('***' = 0.01, '**' = 0.05, '*' = 0.10),
             error_pos = "right",
             model.names = c("AWOTE",
                             "AENA (wages)",
                             "AENA (social)"))

model3



# Model 4: The previous model with independent variable lag --------------------------


# AWOTE
rba_awote <- awote_d4 ~    
  sg_with_award2_d1_lag2 + 
  nairu_gap_lag4 +
  unemp_d4_lag4 +
  inf_exp_lag4 +
  nf_gdp_d8_lag4 + 
  awote_d4_lag1

rba_awote_model <- lm(rba_awote, super_data)


# AENA (wages)
rba_aena_wages <- aena_wages_d4 ~    
  sg_with_award2_d1_lag2 + 
  nairu_gap_lag4 +
  unemp_d4_lag4 +
  inf_exp_lag4 +
  nf_gdp_d8_lag4 + 
  aena_wages_d4_lag1

rba_aena_wages_model <- lm(rba_aena_wages, super_data)


# AENA (social)
rba_aena_social <- aena_social_d4 ~    
  sg_with_award2_d1_lag2 + 
  nairu_gap_lag4 +
  unemp_d4_lag4 +
  inf_exp_lag4 +
  nf_gdp_d8_lag4 + 
  aena_social_d4_lag1

rba_aena_social_model <- lm(rba_aena_social, super_data)


# Results table
model4 <- export_summs(rba_awote_model,
             rba_aena_wages_model, 
             rba_aena_social_model, 
             error_format = "(p = {p.value})",
             stars = c('***' = 0.01, '**' = 0.05, '*' = 0.10),
             error_pos = "right",
             model.names = c("AWOTE",
                             "AENA (wages)",
                             "AENA (social)"))

model4




