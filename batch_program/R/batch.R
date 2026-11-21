# *******************************************************************************************************
# Program Name:                 batch.R
# Project:                      TRA-025
# Purpose:                      Batch-run ADaM programs and export for submission
# Original Author:              Binod Jung Bogati (linkedin.com/in/bjungbogati)
# Copyright:                    © 2025. Unauthorized distribution or reuse prohibited.
# Date Created:                 8/12/2025
# Parameters:                   NA
#
# Input:                        sdtm/sdtm.rda (SDTM), R/adsl.R, R/adae.R
# Output:                       adam/adsl.xpt, adam/adae.xpt
# Modifications:
#   Date        By             Changes
# **********  *************  *************************************
#
# ********************************************************************************************************/

library(tidyverse)
library(purrr)
library(haven)

## batch run ADaM programs ---------------------------------------------------
# SAS analogy: a production "run all" driver that %includes each program in
# dependency order - same idea as a batch .sas driver calling adsl.sas then
# adae.sas in sequence. We only submit ADaM programs (not SDTM), so this
# folder only drives ADaM creation - SDTM is read in as already-final data
# from sdtm/sdtm.rda.

source("R/mapper.R") # export_adam() helper, needed before the batch export step below

# order matters: adae.R expects `adsl` to already exist in the session
adam_programs <- c("R/adsl.R", "R/adae.R")
walk(adam_programs, source)

## batch export ---------------------------------------------------------------
# every ADaM dataset built above, written to adam/<name>.xpt in one pass -
# same idea as a macro loop of PROC COPY / xport steps, one per dataset.
adam_datasets <- list(adsl = adsl, adae = adae)
iwalk(adam_datasets, ~ export_adam(.x, name = .y, output_path = "adam"))
