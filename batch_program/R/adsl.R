# *******************************************************************************************************
# Program Name:                 adsl.R
# Project:                      TRA-025
# Purpose:                      Create ADaM Dataset of ADSL
# Original Author:              Binod Jung Bogati (linkedin.com/in/bjungbogati)
# Copyright:                    © 2025. Unauthorized distribution or reuse prohibited.
# Date Created:                 8/12/2025
# Parameters:                   NA
#
# Input:                        dm ds ex (SDTM, from sdtm/sdtm.rda)
# Output:                       adsl
# Modifications:
#   Date        By             Changes
# **********  *************  *************************************
#
# ********************************************************************************************************/

library(tidyverse)
library(haven)

# ADaM programs read SDTM (already-mapped, final domains) as input - not
# raw source data. operational.rda holds those SDTM domains: dm, ds, ex, ae, vs.
load("sdtm/sdtm.rda")

## treatment start/end dates - earliest/latest EX record per subject
## SAS analogy: proc sql summary join, min(EXSTDTC)/max(EXENDTC) group by usubjid
## screen failures never dosed, so some subjects have no usable EX date -
## min()/max() on an all-NA group returns +-Inf, which we convert back to
## NA so TRTSDT/TRTEDT stay a proper (missing) Date instead of -Inf/Inf
ex_dt <- ex %>%
  mutate(EXSTDT = as.Date(EXSTDTC), EXENDT = as.Date(EXENDTC)) %>%
  group_by(USUBJID) %>%
  summarise(
    TRTSDT = suppressWarnings(min(EXSTDT, na.rm = TRUE)),
    TRTEDT = suppressWarnings(max(EXENDT, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(across(c(TRTSDT, TRTEDT), ~ if_else(is.infinite(.x), as.Date(NA), .x)))

## disposition status - from the single DISPOSITION EVENT record per subject
disp <- ds %>%
  filter(DSCAT == "DISPOSITION EVENT") %>%
  mutate(
    EOSSTT  = if_else(DSDECOD == "COMPLETED", "COMPLETED", "DISCONTINUED"),
    DCSREAS = if_else(DSDECOD == "COMPLETED", NA_character_, DSDECOD)
  ) %>%
  select(USUBJID, EOSSTT, DCSREAS)

## ADSL - one record per subject
adsl <- dm %>%
  select(
    STUDYID, USUBJID, SUBJID, SITEID, AGE, AGEU, SEX, RACE, ETHNIC,
    ARMCD, ARM, ACTARMCD, ACTARM, COUNTRY
  ) %>%
  rename(TRT01P = ARM, TRT01A = ACTARM) %>%
  left_join(ex_dt, by = "USUBJID") %>%
  left_join(disp, by = "USUBJID") %>%
  mutate(SAFFL = if_else(USUBJID %in% ex$USUBJID, "Y", "N"))

adsl
