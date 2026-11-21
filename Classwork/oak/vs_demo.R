# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2026. Unauthorized distribution or reuse prohibited.

library(tidyverse)
library(sdtm.oak)

rsetup::get_data("vs")

# Study CT: raw pivot column names → VSTESTCD
# term_synonyms (semicolon-separated) cover all three raw sources
vs_testcd_ct <- tibble(
  codelist_code       = rep("C66741", 7),
  term_code           = c("C49676",       "C49678",          "C25298",                  "C25299",                   "C49677",      "C81255",  "C60816"),
  CodedData           = c("PULSE",        "RESP",            "SYSBP",                   "DIABP",                    "TEMP",        "WEIGHT",  "HEIGHT"),
  term_value          = c("PULSE",        "RESP",            "SYSBP",                   "DIABP",                    "TEMP",        "WEIGHT",  "HEIGHT"),
  collected_value     = c("VSHR",         "VSRESP",          "VSSYS",                   "VSDIA",                    "VSTEMP",      "VS2WGT",  "VS2HGT"),
  term_preferred_term = c("Pulse Rate",   "Respiration Rate","Systolic Blood Pressure", "Diastolic Blood Pressure", "Temperature", "Weight",  "Height"),
  term_synonyms       = c("VSHR; VS1HR",  "VSRESP; VS1RESP", "VSSYS; VS1SYS",           "VSDIA; VS1DIA",            "VSTEMP; VS1TEMP","VS2WGT","VS2HGT"),
  raw_codelist        = rep("TESTCD",     7)
)

# Lookup: raw col name → VSTEST label / VSORRESU unit (pre-derive in mutate)
raw_to_vstest <- c(
  VSHR="Pulse Rate",  VS1HR="Pulse Rate",  VS2WGT="Weight",     VS2HGT="Height",
  VSRESP="Respiration Rate", VS1RESP="Respiration Rate",
  VSSYS="Systolic Blood Pressure",  VS1SYS="Systolic Blood Pressure",
  VSDIA="Diastolic Blood Pressure", VS1DIA="Diastolic Blood Pressure",
  VSTEMP="Temperature", VS1TEMP="Temperature"
)
raw_to_vsorresu <- c(
  VSHR="bpm",   VS1HR="bpm",   VS2WGT="kg",  VS2HGT="cm",
  VSRESP="breaths/min", VS1RESP="breaths/min",
  VSSYS="mmHg", VS1SYS="mmHg", VSDIA="mmHg", VS1DIA="mmHg",
  VSTEMP="C",   VS1TEMP="C"
)
visitn_fmt <- c(
  Screening=1, "Day 1"=2, "Day 2"=3, "Day 3"=4, "Day 4"=5, "Day 5"=6, "Week 6"=7
)

# ---- Source 1: raw_vs (Screening + Week 6, no timepoints) ----
vs_raw1 <- raw_vs |>
  pivot_longer(cols = c(VSHR, VSRESP, VSSYS, VSDIA, VSTEMP),
               names_to = "VSTESTCD_RAW", values_to = "VSORRES") |>
  generate_oak_id_vars(pat_var = "SUBJECT", raw_src = "raw_vs") |>
  mutate(
    USUBJID      = paste0("TRA-025-", SUBJECT),
    VSTEST_RAW   = raw_to_vstest[VSTESTCD_RAW]   |> unname(),
    VSORRESU_RAW = raw_to_vsorresu[VSTESTCD_RAW] |> unname(),
    VSDTC_RAW    = if_else(VSTM != "", paste0(VSDT, "T", VSTM), as.character(VSDT)),
    VISIT_RAW    = str_to_title(FOLDERNAME),
    VISITNUM_RAW = visitn_fmt[VISIT_RAW] |> as.numeric()
  )

vs1 <-
  hardcode_no_ct(tgt_val = "TRA-025", raw_dat = vs_raw1, raw_var = "VSTESTCD_RAW", tgt_var = "STUDYID") |>
  hardcode_no_ct(tgt_dat = _,         raw_dat = vs_raw1, raw_var = "VSTESTCD_RAW", tgt_var = "DOMAIN",   tgt_val = "VS") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = vs_raw1, raw_var = "USUBJID",      tgt_var = "USUBJID") |>
  assign_ct(     tgt_dat = _,         raw_dat = vs_raw1, raw_var = "VSTESTCD_RAW", tgt_var = "VSTESTCD", ct_spec = vs_testcd_ct, ct_clst = "C66741") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = vs_raw1, raw_var = "VSTEST_RAW",   tgt_var = "VSTEST") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = vs_raw1, raw_var = "VSORRES",      tgt_var = "VSORRES") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = vs_raw1, raw_var = "VSORRESU_RAW", tgt_var = "VSORRESU") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = vs_raw1, raw_var = "VSDTC_RAW",    tgt_var = "VSDTC") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = vs_raw1, raw_var = "VISIT_RAW",    tgt_var = "VISIT") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = vs_raw1, raw_var = "VISITNUM_RAW", tgt_var = "VISITNUM") |>
  # VSSTAT: "NOT DONE" when VSORRES is missing
  hardcode_no_ct(tgt_dat = _,
                 raw_dat  = condition_add(vs_raw1, is.na(VSORRES) | VSORRES == ""),
                 raw_var  = "VSTESTCD_RAW", tgt_var = "VSSTAT", tgt_val = "NOT DONE")

# ---- Source 2: raw_vs1 (Day 1–5, with timepoints) ----
vs_raw2 <- raw_vs1 |>
  pivot_longer(cols = c(VS1HR, VS1RESP, VS1SYS, VS1DIA, VS1TEMP),
               names_to = "VSTESTCD_RAW", values_to = "VSORRES") |>
  generate_oak_id_vars(pat_var = "SUBJECT", raw_src = "raw_vs1") |>
  mutate(
    USUBJID      = paste0("TRA-025-", SUBJECT),
    VSTEST_RAW   = raw_to_vstest[VSTESTCD_RAW]   |> unname(),
    VSORRESU_RAW = raw_to_vsorresu[VSTESTCD_RAW] |> unname(),
    VSDTC_RAW    = if_else(VS1TM != "", paste0(VS1DT, "T", VS1TM), as.character(VS1DT)),
    VISIT_RAW    = FOLDERNAME,
    VISITNUM_RAW = visitn_fmt[FOLDERNAME] |> as.numeric(),
    VSTPT_RAW    = VS1TPT,
    VSTPTNUM_RAW = as.numeric(VS1TPT_STD)
  )

vs2 <-
  hardcode_no_ct(tgt_val = "TRA-025", raw_dat = vs_raw2, raw_var = "VSTESTCD_RAW", tgt_var = "STUDYID") |>
  hardcode_no_ct(tgt_dat = _,         raw_dat = vs_raw2, raw_var = "VSTESTCD_RAW", tgt_var = "DOMAIN",   tgt_val = "VS") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = vs_raw2, raw_var = "USUBJID",      tgt_var = "USUBJID") |>
  assign_ct(     tgt_dat = _,         raw_dat = vs_raw2, raw_var = "VSTESTCD_RAW", tgt_var = "VSTESTCD", ct_spec = vs_testcd_ct, ct_clst = "C66741") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = vs_raw2, raw_var = "VSTEST_RAW",   tgt_var = "VSTEST") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = vs_raw2, raw_var = "VSORRES",      tgt_var = "VSORRES") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = vs_raw2, raw_var = "VSORRESU_RAW", tgt_var = "VSORRESU") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = vs_raw2, raw_var = "VSDTC_RAW",    tgt_var = "VSDTC") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = vs_raw2, raw_var = "VISIT_RAW",    tgt_var = "VISIT") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = vs_raw2, raw_var = "VISITNUM_RAW", tgt_var = "VISITNUM") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = vs_raw2, raw_var = "VSTPT_RAW",    tgt_var = "VSTPT") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = vs_raw2, raw_var = "VSTPTNUM_RAW", tgt_var = "VSTPTNUM") |>
  hardcode_no_ct(tgt_dat = _,
                 raw_dat  = condition_add(vs_raw2, is.na(VSORRES) | VSORRES == ""),
                 raw_var  = "VSTESTCD_RAW", tgt_var = "VSSTAT", tgt_val = "NOT DONE")

# ---- Source 3: raw_vs2 (Weight + Height, Screening only) ----
vs_raw3 <- raw_vs2 |>
  pivot_longer(cols = c(VS2WGT, VS2HGT),
               names_to = "VSTESTCD_RAW", values_to = "VSORRES") |>
  generate_oak_id_vars(pat_var = "SUBJECT", raw_src = "raw_vs2") |>
  mutate(
    USUBJID      = paste0("TRA-025-", SUBJECT),
    VSTEST_RAW   = raw_to_vstest[VSTESTCD_RAW]   |> unname(),
    VSORRESU_RAW = raw_to_vsorresu[VSTESTCD_RAW] |> unname(),
    VSDTC_RAW    = as.character(VS2DT),
    VISIT_RAW    = FOLDERNAME,
    VISITNUM_RAW = visitn_fmt[FOLDERNAME] |> as.numeric()
  )

vs3 <-
  hardcode_no_ct(tgt_val = "TRA-025", raw_dat = vs_raw3, raw_var = "VSTESTCD_RAW", tgt_var = "STUDYID") |>
  hardcode_no_ct(tgt_dat = _,         raw_dat = vs_raw3, raw_var = "VSTESTCD_RAW", tgt_var = "DOMAIN",   tgt_val = "VS") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = vs_raw3, raw_var = "USUBJID",      tgt_var = "USUBJID") |>
  assign_ct(     tgt_dat = _,         raw_dat = vs_raw3, raw_var = "VSTESTCD_RAW", tgt_var = "VSTESTCD", ct_spec = vs_testcd_ct, ct_clst = "C66741") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = vs_raw3, raw_var = "VSTEST_RAW",   tgt_var = "VSTEST") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = vs_raw3, raw_var = "VSORRES",      tgt_var = "VSORRES") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = vs_raw3, raw_var = "VSORRESU_RAW", tgt_var = "VSORRESU") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = vs_raw3, raw_var = "VSDTC_RAW",    tgt_var = "VSDTC") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = vs_raw3, raw_var = "VISIT_RAW",    tgt_var = "VISIT") |>
  assign_no_ct(  tgt_dat = _,         raw_dat = vs_raw3, raw_var = "VISITNUM_RAW", tgt_var = "VISITNUM") |>
  hardcode_no_ct(tgt_dat = _,
                 raw_dat  = condition_add(vs_raw3, is.na(VSORRES) | VSORRES == ""),
                 raw_var  = "VSTESTCD_RAW", tgt_var = "VSSTAT", tgt_val = "NOT DONE")

# ---- Combine all sources ----
vs_data <- bind_rows(vs1, vs2, vs3)

# ---- Baseline flag (last obs <= first exposure date per subject+test) ----
vs_data <- derive_blfl(
  sdtm_in   = vs_data,
  dm_domain = dm,
  tgt_var   = "VSBLFL",
  ref_var   = "RFXSTDTC"
)

# ---- Study day ----
# Preserve ISO 8601 strings — derive_study_day converts VSDTC to Date in-place
vs_data <- vs_data |> mutate(VSDTC_CHAR = as.character(VSDTC))

vs_data <- derive_study_day(
  sdtm_in      = vs_data,
  dm_domain    = dm,
  tgdt         = "VSDTC",
  refdt        = "RFXSTDTC",
  study_day_var = "VSDY"
) |>
  mutate(VSDTC = VSDTC_CHAR) |>
  select(-VSDTC_CHAR)

# ---- Sequence number ----
vs_data <- vs_data |>
  derive_seq(tgt_var = "VSSEQ", rec_vars = c("STUDYID", "USUBJID", "VSTESTCD", "VSDTC"))

# ---- Final VS dataset ----
vs <- vs_data |>
  select(
    STUDYID, DOMAIN, USUBJID, VSSEQ,
    VSTESTCD, VSTEST,
    VSORRES, VSORRESU,
    VSTPT, VSTPTNUM,
    VSSTAT, VSBLFL,
    VISIT, VISITNUM,
    VSDTC, VSDY
  )

