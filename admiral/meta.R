# *******************************************************************************************************
# Program Name:                 meta.R
# Project:                      CDISC
# Purpose:                      Create Meta of ADAM
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
#   2026-05-22  BJB            Fix variable name bugs, broken pipeline, missing derivations
#                              Update API for metacore 0.3.0, xportr 0.5.0
# ********************************************************************************************************/



  library(metacore)
  library(metatools)
  library(tidyverse)
  library(admiral)
  library(haven)
  library(xportr)

rsetup::get_data("sdtm_demo")

# FIX: was ae <- convert_blanks_to_na(dm) — wrong LHS
dm <- convert_blanks_to_na(dm)

dm_supp_combine <- combine_supp(dm, suppdm)

adsl_spec <- spec_to_metacore(path = "adam/adam_specs.xlsx", where_sep_sheet = FALSE) |>
    select_dataset("ADSL")

# Build predecessor variables from DM/SUPPDM
adsl_preds <- build_from_derived(
  adsl_spec,
  ds_list = list("dm" = dm_supp_combine, "suppdm" = dm_supp_combine),
  predecessor_only = FALSE,
  keep = FALSE
)

# NOTE: check_variables removed here — adsl_preds holds predecessor vars only;
# all derived vars added downstream. Run check on final dataset instead.

# --- Age grouping ---

# FIX: added is.na(AGE) guard; changed AGE > 64 to !is.na(AGE) to avoid NA bleed-through
agegr1_lookup <- exprs(
  ~condition,            ~AGEGR1, ~AGEGR1N,
  AGE < 18,                "<18",       1L,
  between(AGE, 18, 64),  "18-64",       2L,
  !is.na(AGE),             ">64",       3L
)

adsl_cat1 <- derive_vars_cat(
  dataset = adsl_preds,
  definition = agegr1_lookup
)

# --- Race numeric (codelist-driven) ---

# FIX: was adsl_preds — must chain from adsl_cat1 to preserve AGEGR1/AGEGR1N
adsl_cat2 <- adsl_cat1 |>
  create_var_from_codelist(
    metacore = adsl_spec,
    input_var = RACE,
    out_var = RACEN
  )

# --- Exposure datetime derivation ---

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

# --- Treatment start / end datetimes, dates, times, duration ---

adsl_dtm <- adsl_cat2 |>
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = !is.na(EXSTDTM),
    new_vars = exprs(
      TRTSDTM = EXSTDTM,
      TRTSTMF = EXSTTMF     # FIX: was TRTSDTMF — spec variable is TRTSTMF
    ),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  ) |>
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = !is.na(EXENDTM),
    new_vars = exprs(
      TRTEDTM = EXENDTM,
      TRTETMF = EXENTMF     # FIX: was TRTEDTMF — spec variable is TRTETMF
    ),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  ) |>
  derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM)) |>
  derive_vars_dtm_to_tm(source_vars = exprs(TRTSDTM)) |>   # spec has TRTSTM only, not TRTETM
  mutate(TRTSTM = as.character(TRTSTM)) |>
  derive_var_trtdurd()

# --- Safety population flag ---

adsl_fl <- adsl_dtm |>
  derive_var_merged_exist_flag(
    dataset_add = ex,
    by_vars = exprs(STUDYID, USUBJID),
    new_var = SAFFL,
    condition = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))),
    false_value = "N",
    missing_value = "N"
  )

# --- Treatment character variables ---

# FIX: was TRTP01P / TRTP01A — spec variables are TRT01P / TRT01A
adsl_trt <- adsl_fl |>
  mutate(
    TRT01P = if_else(ARM %in% c("Screen Failure", "Not Assigned", "Not Treated"),
                     "No Treatment", ARM),
    TRT01A = if_else(ACTARM %in% c("Screen Failure", "Not Assigned", "Not Treated"),
                     "No Treatment", ACTARM)
  )

# FIX: was missing — spec requires TRT01PN and TRT01AN (integer, codelist ADSL_TRTN)
adsl_trt <- adsl_trt |>
  create_var_from_codelist(metacore = adsl_spec, input_var = TRT01P, out_var = TRT01PN) |>
  create_var_from_codelist(metacore = adsl_spec, input_var = TRT01A, out_var = TRT01AN)

# --- Disposition date ---

ds_ext <- derive_vars_dt(
  ds,
  dtc = DSSTDTC,
  new_vars_prefix = "DSST"
)

# --- End of study date ---

adsl_eos <- adsl_trt |>
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(EOSDT = DSSTDT),
    filter_add = DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE"
  )

# --- End of study status ---

format_eosstt <- function(x) {
  case_when(
    x == "COMPLETED"     ~ "COMPLETED",
    x == "SCREEN FAILURE" ~ NA_character_,
    TRUE                  ~ "DISCONTINUED"
  )
}

adsl_eosstat <- adsl_eos |>
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = exprs(STUDYID, USUBJID),
    filter_add = DSCAT == "DISPOSITION EVENT",
    new_vars = exprs(EOSSTT = format_eosstt(DSDECOD)),
    missing_values = exprs(EOSSTT = "ONGOING")
  )

# --- Randomization, screen failure, and final retrieval visit dates ---
# FIX: was missing — spec requires RANDDT, SCRFDT, FRVDT

adsl_dates <- adsl_eosstat |>
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
  ) |>
  # FIX: was missing — spec requires RANDFL (Derived, NY_YN codelist)
  mutate(RANDFL = if_else(!is.na(RANDDT), "Y", NA_character_))

# --- Death date ---
# FIX: was new_vars_prefix = "DSST" — creates DSSTDT (corrupts EOS date)
#      Correct prefix is "DTH" to create DTHDT + DTHDTF (auto-created by admiral)

adsl_dth <- derive_vars_dt(
  adsl_dates,
  dtc = DTHDTC,
  highest_imputation = "M",
  date_imputation = "first",
  new_vars_prefix = "DTH"
)

# --- Death cause and domain ---
# FIX: was missing — spec requires DTHCAUS, DTHDOM (both Derived)

adsl_dth <- adsl_dth |>
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

# --- Death duration variables ---
# FIX: was missing — spec requires DTHADY, LDDTHELD (both Derived)

adsl_dth <- adsl_dth |>
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

# --- Grouping variables ---
# FIX: was missing — spec requires REGION1/N, RACEGR1/N, DTHCGR1/N (all Derived)

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

adsl_final <- adsl_dth |>
  derive_vars_cat(definition = region1_lookup) |>
  derive_vars_cat(definition = racegr1_lookup) |>
  derive_vars_cat(definition = dthcgr1_lookup)

# --- Export ---
# FIX: was missing — add spec-driven validation + xportr export
# FIX: xportr 0.5.0 removed named 'metacore' param — use xportr_metadata() chain


adsl_final |>
  check_variables(adsl_spec) |>
  check_ct_data(adsl_spec, na_acceptable = TRUE) |>
  order_cols(adsl_spec) |>
  sort_by_key(adsl_spec) |>
  xportr_metadata(adsl_spec, domain = "ADSL") |>
  xportr_type() |>
  xportr_length() |>
  xportr_label() |>
  xportr_df_label() |>
  xportr_write("adam/preadsl.xpt")
