# *******************************************************************************************************
# Program Name:                 meta.R
# Project:                      CDISC
# Purpose:                      Create Meta of ADAM
# Original Author:              Binod Jung Bogati (linkedin.com/in/bjungbogati)
# Copyright:                    © 2025. Unauthorized distribution or reuse prohibited.
# Date Created:                 8/12/2025
# Parameters:                   NA
#
# Input:
# Output:
# Modifications:
#   Date        By             Changes
# **********  *************  *************************************
#
# ********************************************************************************************************/

library(metacore)
library(metatools)
library(tidyverse)
library(admiral)
library(haven)
library(xportr)

rsetup::get_data("sdtm_demo")

# dm1 <- convert_blanks_to_na(dm)

dm_supp <- combine_supp(dm, suppdm)

adsl_spec <- spec_to_metacore(path = "adam/adam_specs.xlsx") |>
  select_dataset("ADSL")

adsl_preds <- build_from_derived(
  adsl_spec,
  ds_list = list(
    "dm" = dm_supp,
    "suppdm" = dm_supp
  ),
  predecessor_only = FALSE,
  keep = FALSE
)


# age group

agegr1_lookup <- exprs(
  ~condition, ~AGEGR1, ~AGEGR1N,
  AGE < 18, "<18", 1,
  between(AGE, 18, 64), "18-64", 2,
  AGE > 64, ">64", 3,
  is.na(AGE), "Missing", 4
)

adsl_cat1 <- derive_vars_cat(
  dataset = adsl_preds,
  definition = agegr1_lookup
)

# codelist

adsl_cat2 <- adsl_cat1 |>
  create_var_from_codelist(
    metacore = adsl_spec,
    input_var = RACE,
    out_var = RACEN
  )

# exposure
ex_ext <- ex |>
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST",
    time_imputation = "last"
  ) |>
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  )


# treatment

adsl_dtm <- adsl_cat2 |>
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = !is.na(EXDOSE),
    new_vars = exprs(
      TRTSDTM = EXSTDTM,
      TRTSTMF = EXSTTMF
    ),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  ) |>
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = !is.na(EXDOSE),
    new_vars = exprs(
      TRTEDTM = EXENDTM,
      TRTETMF = EXENTMF
    ),
    order = exprs(EXENDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  ) |>
  derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM)) |>
  derive_vars_dtm_to_tm(source_vars = exprs(TRTSDTM)) |>   # spec has TRTSTM only, not TRTETM
  mutate(TRTSTM = as.character(TRTSTM)) |>
  derive_var_trtdurd()

# safety population flag

adsl_fl <- adsl_dtm |>
  derive_var_merged_exist_flag(
    dataset_add = ex,
    by_vars = exprs(STUDYID, USUBJID),
    new_var = SAFFL,
    condition = EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO")),
    false_value = "N",
    missing_value = "N"
  )

# Treatment variables

adsl_trt <- adsl_fl |>
  mutate(
    TRT01P = if_else(
      ARM %in% c("Screen Failure", "Not Assigned", "Not Treated"),
      "No Treatment", ARM
    ),
    TRT01A = if_else(
      ACTARM %in% c("Screen Failure", "Not Assigned", "Not Treated"),
      "No Treatment", ACTARM
    )
  ) |>
  create_var_from_codelist(adsl_spec,
                           input_var = TRT01P,
                           out_var = TRT01PN
  ) |>
  create_var_from_codelist(adsl_spec,
                           input_var = TRT01A,
                           out_var = TRT01AN
  )

# disposition

ds_ext <- derive_vars_dt(
  ds,
  dtc = DSSTDTC,
  new_vars_prefix = "DSST"
)

# end of study date

adsl_eos <- adsl_trt |>
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(EOSDT = DSSTDT),
    filter_add = DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE"
  )

format_eosstt <- function(x) {
  case_when(
    x == "COMPLETED" ~ "COMPLETED",
    x == "SCREEN FAILURE" ~ NA_character_,
    TRUE ~ "DISCONTINUED"
  )
}

# end of study status
adsl_eosstat <- adsl_eos |>
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(EOSSTT = format_eosstt(DSDECOD)),
    filter_add = DSCAT == "DISPOSITION EVENT",
    missing_values = exprs(EOSSTT = "ONGOING")
  )


# randomization, screen failure

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
  mutate(
    RANDFL = if_else(
      is.na(RANDDT), NA, "Y"
    )
  )

# death dates

adsl_dth <- adsl_dates |>
  derive_vars_dt(
    dtc = DTHDTC,
    highest_imputation = "M",
    date_imputation = "first",
    new_vars_prefix = "DTH"
  )

# death cause

adsl_dth <- adsl_dth |>
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
      event(
        dataset_name = "ae",
        condition = AEOUT == "FATAL",
        set_values_to = exprs(
          DTHCAUS = AEDECOD,
          DTHDOM = "AE"
        )
      ),
      event(
        dataset_name = "ds",
        condition = DSDECOD == "DEATH",
        set_values_to = exprs(
          DTHCAUS = DSTERM,
          DTHDOM = "DS"
        )
      )
    ),
    source_datasets = list(
      ae = ae, ds = ds
    ),
    tmp_event_nr_var = event_nr,
    order = exprs(event_nr),
    mode = "first",
    new_vars = exprs(DTHCAUS, DTHDOM)
  )

adsl_dth <- adsl_dth |>
  derive_vars_duration(
    new_var = DTHADY,
    start_date = TRTSDT,
    end_date = DTHDT
  ) |>
  derive_vars_duration(
    new_var = LDDTHELD,
    start_date = TRTEDT,
    end_date = DTHDT
  )

# grouping


# issues
dthcgr1_lookup <- exprs(
  ~condition, ~DTHCGR1, ~DTHCGR1N,
  DTHDOM == "AE", "ADVERSE EVENT", 1,
  !is.na(DTHDOM) & str_detect(DTHCAUS, "(PROGRESSIVE DISEASE|DISEASE RELAPSE)"), "PROGRESSIVE DISEASE", 2,
  !is.na(DTHDOM) & !is.na(DTHCAUS), "OTHER", 3,
  is.na(DTHDOM), NA_character_, NA
)

racegr1_lookup <- exprs(
  ~condition, ~RACEGR1, ~RACEGR1N,
  RACE == "WHITE", "White", 1,
  RACE != "WHITE", "Non-white", 2,
  is.na(RACE), "Missing", 3
)

region1_lookup <- exprs(
  ~condition, ~REGION1, ~REGION1N,
  COUNTRY %in% c("CAN", "USA"), "North America", 1,
  !is.na(COUNTRY), "Rest of the World", 2,
  is.na(COUNTRY), "Missing", 3
)

adsl_draft <- adsl_dth |>
  derive_vars_cat(definition = dthcgr1_lookup) |>
  derive_vars_cat(definition = racegr1_lookup) |> 
  derive_vars_cat(definition = region1_lookup)

adsl_draft |>
  check_variables(adsl_spec) |>
  check_ct_data(adsl_spec,
                na_acceptable = TRUE
  ) |>
  order_cols(adsl_spec) |>
  sort_by_key(adsl_spec) |>
  xportr_metadata(
    adsl_spec,
    domain = "ADSL"
  ) |>
  xportr_type(verbose = "message") |>  # fix line 108
  xportr_length() |>
  xportr_label() |>
  xportr_df_label() |>
  xportr_write("adsl.xpt")


# read the xpt dataset
preadsl <- read_xpt("adsl.xpt")
