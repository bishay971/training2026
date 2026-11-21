# *******************************************************************************************************
# Program Name:                 adsl.R
# Project:                      CDISC
# Purpose:                      Create ADSL
# Original Author:              Binod Jung Bogati (linkedin.com/in/bjungbogati)
# Copyright:                    © 2025. Unauthorized distribution or reuse prohibited.
# Date Created:                 8/12/2025
# Parameters:                   NA
#
# Input:                        SDTM datasets (dm, suppdm, ex, ds, ae)
#                               adam/adam_specs.xlsx
# Output:                       adsl.xpt
# Modifications:
#   Date        By             Changes
# **********  *************  *************************************
#   2026-05-22  BJB            Fix spec path, undefined dm_suppdm, xportr 0.5.0 API
# ********************************************************************************************************/



  library(metacore)
  library(metatools)
  library(tidyverse)
  library(admiral)
  library(haven)
  library(xportr)

# FIX: was missing — load SDTM data before use
rsetup::get_data("sdtm_demo")

# FIX: dm_suppdm was undefined — define it here
dm_suppdm <- combine_supp(dm, suppdm)

## ----r metacore, warning=FALSE, results='hold'--------------------------------

# FIX: path was "adam/adsl_specs.xlsx" — file does not exist, only adam_specs.xlsx exists
# suppressWarnings/Messages: spec file has empty optional columns — harmless metacore warnings
adsl_spec <- spec_to_metacore(path = "adam/adam_specs.xlsx", where_sep_sheet = FALSE) |>
    select_dataset("ADSL")

## ----r demographics-----------------------------------------------------------
adsl_preds <- build_from_derived(
  adsl_spec,
  ds_list = list("dm" = dm_suppdm, "suppdm" = dm_suppdm),
  predecessor_only = FALSE,
  keep = FALSE
)

## ----r grouping_option_1 (derive_vars_cat)------------------------------------
# Option 1: admiral derive_vars_cat with lookup table
agegr1_lookup <- exprs(
  ~condition,            ~AGEGR1, ~AGEGR1N,
  AGE < 18,                "<18",       1L,
  between(AGE, 18, 64),  "18-64",       2L,
  !is.na(AGE),             ">64",       3L
)

adsl_cat <- derive_vars_cat(
  dataset = adsl_preds,
  definition = agegr1_lookup
)

## ----r------------------------------------------------------------------------
get_control_term(adsl_spec, variable = AGEGR1)

## ----r grouping_option_2 (metatools create_cat_var)---------------------------
# Option 2: metatools spec-driven categorical derivation (used in downstream chain)
adsl_ct <- adsl_preds |>
  create_cat_var(
    adsl_spec,
    ref_var = AGE,
    grp_var = AGEGR1,
    num_grp_var = AGEGR1N
  )

## ----r grouping_option_3 (manual mutate)--------------------------------------
# Option 3: manual case_when functions
format_agegr1 <- function(age) {
  case_when(
    age < 18              ~ "<18",
    between(age, 18, 64)  ~ "18-64",
    age > 64              ~ ">64"
  )
}

format_agegr1n <- function(age) {
  case_when(
    age < 18              ~ 1L,
    between(age, 18, 64)  ~ 2L,
    age > 64              ~ 3L
  )
}

adsl_cust <- adsl_preds |>
  mutate(
    AGEGR1  = format_agegr1(AGE),
    AGEGR1N = format_agegr1n(AGE)
  )

## ----r codelist---------------------------------------------------------------
# Continue pipeline from Option 2 (adsl_ct); add RACEN from codelist
adsl_ct <- adsl_ct |>
  create_var_from_codelist(
    metacore  = adsl_spec,
    input_var = RACE,
    out_var   = RACEN
  )

## ----r exposure---------------------------------------------------------------
ex_ext <- ex |>
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST"
  ) |>
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  )

adsl_raw <- adsl_ct |>
  # Treatment Start Datetime
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  ) |>
  # Treatment End Datetime
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  ) |>
  derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM)) |>
  derive_vars_dtm_to_tm(source_vars = exprs(TRTSDTM)) |>
  mutate(TRTSTM = as.character(TRTSTM)) |>
  derive_var_trtdurd() |>
  # Safety Population Flag
  derive_var_merged_exist_flag(
    dataset_add = ex,
    by_vars = exprs(STUDYID, USUBJID),
    new_var = SAFFL,
    false_value = "N",
    missing_value = "N",
    condition = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO")))
  )

## ----r treatment_char---------------------------------------------------------
adsl <- adsl_raw |>
  mutate(
    TRT01P = if_else(ARM %in% c("Screen Failure", "Not Assigned", "Not Treated"),
                     "No Treatment", ARM),
    TRT01A = if_else(ACTARM %in% c("Screen Failure", "Not Assigned", "Not Treated"),
                     "No Treatment", ACTARM)
  )

## ----r treatment_num----------------------------------------------------------
adsl <- adsl |>
  create_var_from_codelist(adsl_spec, input_var = TRT01P, out_var = TRT01PN) |>
  create_var_from_codelist(adsl_spec, input_var = TRT01A, out_var = TRT01AN)

## ----r disposition------------------------------------------------------------
ds_ext <- derive_vars_dt(
  ds,
  dtc = DSSTDTC,
  new_vars_prefix = "DSST"
)

adsl <- adsl |>
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(EOSDT = DSSTDT),
    filter_add = DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE"
  )

## ----r eosstt-----------------------------------------------------------------
format_eosstt <- function(x) {
  case_when(
    x %in% c("COMPLETED")     ~ "COMPLETED",
    x %in% c("SCREEN FAILURE") ~ NA_character_,
    TRUE                        ~ "DISCONTINUED"
  )
}

adsl <- adsl |>
  derive_vars_merged(
    dataset_add = ds,
    by_vars = exprs(STUDYID, USUBJID),
    filter_add = DSCAT == "DISPOSITION EVENT",
    new_vars = exprs(EOSSTT = format_eosstt(DSDECOD)),
    missing_values = exprs(EOSSTT = "ONGOING")
  )

## ----r death_date-------------------------------------------------------------
adsl <- adsl |>
  derive_vars_dt(
    new_vars_prefix = "DTH",
    dtc = DTHDTC,
    highest_imputation = "M",
    date_imputation = "first"
  )

## ----r milestone_dates--------------------------------------------------------
adsl <- adsl |>
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(RANDDT = DSSTDT),
    filter_add = DSDECOD == "RANDOMIZED"
  ) |>
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(SCRFDT = DSSTDT),
    filter_add = DSCAT == "DISPOSITION EVENT" & DSDECOD == "SCREEN FAILURE"
  ) |>
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(FRVDT = DSSTDT),
    filter_add = DSCAT == "OTHER EVENT" & DSDECOD == "FINAL RETRIEVAL VISIT"
  )

## ----r death_duration---------------------------------------------------------
adsl <- adsl |>
  derive_vars_duration(
    new_var = DTHADY,
    start_date = TRTSDT,
    end_date = DTHDT
  ) |>
  derive_vars_duration(
    new_var = LDDTHELD,
    start_date = TRTEDT,
    end_date = DTHDT,
    add_one = FALSE
  )

## ----r randfl-----------------------------------------------------------------
adsl <- adsl |>
  mutate(RANDFL = if_else(!is.na(RANDDT), "Y", NA_character_))

## ----r death_cause------------------------------------------------------------
adsl <- adsl |>
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
      event(
        dataset_name = "ae",
        condition = AEOUT == "FATAL",
        set_values_to = exprs(DTHCAUS = AEDECOD, DTHDOM = "AE")
      ),
      event(
        dataset_name = "ds",
        condition = DSDECOD == "DEATH" & grepl("DEATH DUE TO", DSTERM),
        set_values_to = exprs(DTHCAUS = DSTERM, DTHDOM = "DS")
      )
    ),
    source_datasets = list(ae = ae, ds = ds),
    tmp_event_nr_var = event_nr,
    order = exprs(event_nr),
    mode = "first",
    new_vars = exprs(DTHCAUS, DTHDOM)
  )

## ----r grouping---------------------------------------------------------------
region1_lookup <- exprs(
  ~condition,                            ~REGION1,         ~REGION1N,
  COUNTRY %in% c("CAN", "USA"),   "North America",                1L,
  !is.na(COUNTRY),            "Rest of the World",                2L,
  is.na(COUNTRY),                       "Missing",                3L
)

racegr1_lookup <- exprs(
  ~condition,             ~RACEGR1, ~RACEGR1N,
  RACE %in% c("WHITE"),    "White",         1L,
  RACE != "WHITE",     "Non-white",         2L,
  is.na(RACE),          "Missing",         3L
)

dthcgr1_lookup <- exprs(
  ~condition,                                                                                ~DTHCGR1, ~DTHCGR1N,
  DTHDOM == "AE",                                                                     "ADVERSE EVENT",        1L,
  !is.na(DTHDOM) & str_detect(DTHCAUS, "(PROGRESSIVE DISEASE|DISEASE RELAPSE)"), "PROGRESSIVE DISEASE",     2L,
  !is.na(DTHDOM) & !is.na(DTHCAUS),                                                         "OTHER",        3L,
  is.na(DTHDOM),                                                                       NA_character_, NA_integer_
)

adsl <- adsl |>
  derive_vars_cat(definition = region1_lookup) |>
  derive_vars_cat(definition = racegr1_lookup) |>
  derive_vars_cat(definition = dthcgr1_lookup)

## ----r export-----------------------------------------------------------------
# FIX: xportr 0.5.0 removed named 'metacore' param from individual functions
#      Use xportr_metadata() to set metadata once for the whole chain


adsl |>
  check_variables(adsl_spec) |>
  check_ct_data(adsl_spec, na_acceptable = TRUE) |>
  order_cols(adsl_spec) |>
  sort_by_key(adsl_spec) |>
  xportr_metadata(adsl_spec, domain = "ADSL") |>
  xportr_type() |>
  xportr_length() |>
  xportr_label() |>
  xportr_df_label() |>
  xportr_write("adam/adsl.xpt")

