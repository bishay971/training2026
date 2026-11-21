# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2026. Unauthorized distribution or reuse prohibited.

library(tidyverse)
library(sdtm.oak)
library(labelled)
library(diffdf)

rsetup::get_data("cm")

var_drops <- c(
  "SITE", "SUBJECTID", "PROJECTID", "RECORDPOSITION", "TARGETDAYS", "PROJECT",
  "STUDYID", "SITEGROUP", "SITENUMBER", "SITEID", "INSTANCENAME",
  "INSTANCEREPEATNUMBER", "INSTANCEID", "PAGEREPEATNUMBER", "FOLDER",
  "FOLDERID", "FOLDERSEQ", "DATAPAGEID", "RECORDDATE", "RECORDID",
  "MINCREATED", "MAXUPDATED", "ENVIRONMENTNAME", "STUDYENVSITENUMBER", "SAVETS"
)

raw_cm <- raw_cm |>
  select(-all_of(var_drops), -starts_with("Z_"))

# Study-specific CT spec: maps raw collected values to SDTM terms
study_ct <- tibble(
  codelist_code       = c("C71113",          "C71113",     "C66729", "C66729",       "C71620", "C71620",   "C66728"),
  term_code           = c("C25473",           "C64499",     "C38288", "C38306",       "C28254", "C48542",   "C53279"),
  CodedData           = c("QD",               "ONCE",       "ORAL",   "INTRAVENOUS",  "mL",     "TABLET",   "ONGOING"),
  term_value          = c("QD",               "ONCE",       "ORAL",   "INTRAVENOUS",  "mL",     "TABLET",   "ONGOING"),
  collected_value     = c("QD = 1x per day",  "X1 = once",  "Oral",   "Intravenous",  "mL",     "Tablet",   "Yes"),
  term_preferred_term = c("Daily",            "Once",       "Oral",   "Intravenous",  "mL",     "Tablet",   "Ongoing"),
  term_synonyms       = c("QD = 1x per day",  "X1 = once",  "Oral",   "Intravenous",  "mL",     "Tablet",   "Yes"),
  raw_codelist        = c("FREQ_CV1",         "FREQ_CV1",   "ROUTE",  "ROUTE",        "UNIT",   "UNIT",     "ENRF")
)

# SUPP variable spec — 'Label' and 'Origin' column names required by generate_sdtm_supp
supp_qual_info <- tibble(
  QNAM   = c("CMDICVER",                              "ATC",               "ATCCD"),
  Label  = c("Medical Coding Dictionary & Version",  "ATC Level 1 Term",  "ATC Level 1 Code"),
  Origin = c("eDT",                                   "eDT",               "eDT")
)

# Prep: OAK ID vars + derived helper columns
# CMROUTE_RAW needed because assign_ct(raw_var == tgt_var) would coalesce raw value first
cm_raw <- generate_oak_id_vars(raw_cm, pat_var = "SUBJECT", raw_src = "raw_cm") |>
  filter(CMTRT != "") |>
  mutate(
    USUBJID     = paste("TRA-025", SUBJECT, sep = "-"),
    CMROUTE_RAW = CMROUTE,
    CMDICVER    = paste(CMTRT_CODERDICTNAME, CMTRT_CODERDICTVERSION),
    ATC         = CMTRT_ATC,
    ATCCD       = CMTRT_ATC_CODE
  )


# STUDYID = "TRA-025"
# DOMAIN = "CM"

# Build CM SDTM using sdtm.oak assign/hardcode functions
cm_data <-
  hardcode_no_ct(tgt_val = "TRA-025", raw_dat = cm_raw, raw_var = "CMTRT",    tgt_var = "STUDYID") |>
  hardcode_no_ct(tgt_dat = _,         raw_dat = cm_raw, raw_var = "CMTRT",    tgt_var = "DOMAIN",   tgt_val = "CM") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = cm_raw, raw_var = "USUBJID",  tgt_var = "USUBJID") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = cm_raw, raw_var = "CMTRT",    tgt_var = "CMTRT") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = cm_raw, raw_var = "FOLDERNAME", tgt_var = "CMCAT") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = cm_raw, raw_var = "CMINDIC",  tgt_var = "CMINDC") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = cm_raw, raw_var = "CMDOSE",   tgt_var = "CMDOSE") |>
  assign_ct(     tgt_dat = _,         raw_dat = cm_raw, raw_var = "CMUNIT",      tgt_var = "CMDOSU",   ct_spec = study_ct, ct_clst = "C71620") |>
  assign_ct(     tgt_dat = _,         raw_dat = cm_raw, raw_var = "CMFREQ",      tgt_var = "CMDOSFRQ", ct_spec = study_ct, ct_clst = "C71113") |>
  assign_ct(     tgt_dat = _,         raw_dat = cm_raw, raw_var = "CMROUTE_RAW", tgt_var = "CMROUTE",  ct_spec = study_ct, ct_clst = "C66729") |>
  assign_ct(     tgt_dat = _,         raw_dat = cm_raw, raw_var = "CMONGO",      tgt_var = "CMENRF",   ct_spec = study_ct, ct_clst = "C66728") |>
  assign_datetime(tgt_dat = _,        raw_dat = cm_raw, raw_var = "CMSTDT_RAW",  tgt_var = "CMSTDTC",  raw_fmt = "d m y", raw_unk = c("UN", "UNK")) |>
  assign_datetime(tgt_dat = _,        raw_dat = cm_raw, raw_var = "CMENDT_RAW",  tgt_var = "CMENDTC",  raw_fmt = "d m y", raw_unk = c("UN", "UNK")) |>
  # SUPP variables
  assign_no_ct(  tgt_dat = _,         raw_dat = cm_raw, raw_var = "CMDICVER", tgt_var = "CMDICVER") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = cm_raw, raw_var = "ATC",      tgt_var = "ATC") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = cm_raw, raw_var = "ATCCD",    tgt_var = "ATCCD") |>
  mutate(CMDOSE = as.numeric(CMDOSE))

# Preserve ISO 8601 character dates — derive_study_day converts them to Date in-place
cm_data <- cm_data |>
  mutate(
    CMSTDTC_CHAR = as.character(CMSTDTC),
    CMENDTC_CHAR = as.character(CMENDTC)
  )

# Study day derivation — uses dm domain for RFSTDTC reference
cm_data <- derive_study_day(
  sdtm_in = cm_data, 
  dm_domain = dm |> select(USUBJID, RFSTDTC),
  tgdt = "CMSTDTC", 
  refdt = "RFSTDTC", 
  study_day_var = "CMSTDY"
) |>
  derive_study_day(dm |> select(USUBJID, RFSTDTC), "CMENDTC", "RFSTDTC", "CMENDY")

# Restore character dates (derive_study_day converts to Date)
# Truncate "yyyy---dd" (UNK month) → "yyyy" to match SDTM partial date convention
cm_data <- cm_data |>
  mutate(
    CMSTDTC = str_replace(CMSTDTC_CHAR, "^(\\d{4})---\\d{2}$", "\\1"),
    CMENDTC = replace_na(CMENDTC_CHAR, ""),
    CMSTDY  = as.numeric(CMSTDY),
    CMENDY  = as.numeric(CMENDY)
  ) |>
  select(-CMSTDTC_CHAR, -CMENDTC_CHAR)

# EPOCH: needs RFSTDTC / RFENDTC from DM
cm_data <- cm_data |>
  left_join(dm |> select(USUBJID, RFSTDTC, RFENDTC), by = "USUBJID") |>
  mutate(
    EPOCH = case_when(
      !is.na(as.Date(CMSTDTC)) & as.Date(CMSTDTC) < as.Date(RFSTDTC) ~ "SCREENING",
      !is.na(as.Date(CMSTDTC)) & as.Date(CMSTDTC) > as.Date(RFSTDTC) & as.Date(CMENDTC) < as.Date(RFENDTC) ~ "TREATMENT",
      !is.na(as.Date(CMSTDTC)) & as.Date(CMSTDTC) > as.Date(RFENDTC) ~ "FOLLOW-UP",
      is.na(as.Date(CMSTDTC)) ~ "SCREENING"
    )
  )

# Sequence number
cm_data <- cm_data |>
  # arrange(STUDYID, USUBJID, CMTRT, CMSTDTC) |>
  derive_seq(tgt_var = "CMSEQ", rec_vars = c("STUDYID", "USUBJID", "CMTRT", "CMSTDTC")) |>
  mutate(CMSEQ = as.numeric(CMSEQ))

# Split into CM + SUPPCM using sdtm.oak
result <- generate_sdtm_supp(
  cm_data |> select(
    STUDYID, DOMAIN, USUBJID, CMSEQ, CMTRT, CMCAT, CMDOSE, CMDOSU,
    CMDOSFRQ, CMROUTE, EPOCH, CMSTDTC, CMENDTC, CMSTDY, CMENDY, CMENRF,
    CMDICVER, ATC, ATCCD
  ),
  idvar         = NULL,
  supp_qual_info = supp_qual_info,
  qnam_var      = "QNAM",
  label_var     = "Label",
  orig_var      = "Origin"
)

cm     <- result$CM
suppcm <- result$SUPPCM |>
  mutate(
    IDVARVAL = as.character(IDVARVAL),
    QEVAL    = replace_na(QEVAL, "")
  ) |>
  arrange(STUDYID, USUBJID, IDVAR, IDVARVAL, QNAM)

# Variable labels
variable_labels <- c(
  STUDYID  = "Study Identifier",
  DOMAIN   = "Domain Abbreviation",
  USUBJID  = "Unique Subject Identifier",
  CMSEQ    = "Sequence Number",
  CMTRT    = "Reported Name of Drug, Med, or Therapy",
  CMCAT    = "Category for Medication",
  CMDOSE   = "Dose",
  CMDOSU   = "Dose Units",
  CMDOSFRQ = "Dose Frequency per Interval",
  CMROUTE  = "Route of Administration",
  EPOCH    = "Epoch",
  CMSTDTC  = "Start Date/Time of Medication",
  CMENDTC  = "End Date/Time of Medication",
  CMSTDY   = "Study Day of Start of Medication",
  CMENDY   = "Study Day of End of Medication",
  CMENRF   = "End Relative to Reference Period"
)

cm <- cm |> set_variable_labels(.labels = variable_labels)

# Compare against validated datasets
diffdf(base = cm, compare = v_cm)

diffdf(base = suppcm, compare = v_suppcm)
