# *******************************************************************************************************
# Program Name:                 adae.R
# Project:                      TRA-025
# Purpose:                      Create ADaM Dataset of ADAE
# Original Author:              Binod Jung Bogati (linkedin.com/in/bjungbogati)
# Copyright:                    © 2025. Unauthorized distribution or reuse prohibited.
# Date Created:                 8/12/2025
# Parameters:                   NA
#
# Input:                        ae (SDTM), adsl (must already exist - run adsl.R first)
# Output:                       adae
# Modifications:
#   Date        By             Changes
# **********  *************  *************************************
#
# ********************************************************************************************************/

library(tidyverse)
library(haven)

get_data("adae")
get_data("adsl_table")

# ae comes from operational.rda (loaded by adsl.R); adsl comes from having
# sourced adsl.R first. SAS analogy: %include adsl.sas; before adae.sas so
# WORK.adsl already exists when this program runs.
adae <- ae %>%
  mutate(ASTDT = as.Date(AESTDTC), AENDT = as.Date(AEENDTC)) %>%
  left_join(
    adsl %>% select(USUBJID, TRTP, TRTA, TRTSDT, TRTEDT),
    by = "USUBJID"
  ) %>%
  mutate(
    TRTEMFL = if_else(!is.na(ASTDT) & !is.na(TRTSDT) & ASTDT >= TRTSDT, "Y", NA_character_)
  )

adae
