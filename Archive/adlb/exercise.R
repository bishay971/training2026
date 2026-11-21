# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2025. Unauthorized distribution or reuse prohibited.

# load(url("https://tinyurl.com/3bc8dcjv"))

library(tidyverse)
library(admiral)
rsetup::get_data("adlb")

# Exercise 1: Write a program to merge LB and ADSL

adlb1 <- lb |>
  left_join(adsl, by = c("STUDYID", "USUBJID"))


# Exercise 2: Derive dates and sequence variable:
# ADT, ADY, ASEQ

adlb2 <- adlb1 |>
  mutate(
    ADT = as.Date(LBDTC)
  )

# Exercise 3: Create Parameter: PARAM, PARAMCD, PARAMN, Parameter Category: PARCAT1, PARCAT1N,

# admiral

# adlb2 <- read_xpt("/sdtm/lb.xpt")

adlb3 <- convert_blanks_to_na(adlb2)

param_lookup <- tribble(
  ~LBTESTCD, ~PARAMCD, ~PARAM, ~PARAMN,
  "ALP", "ALP", "Alkaline Phosphatase (mg/dL)", 1,
  "ALT", "ALT", "Alanine Aminotransferase (mg/dL)", 2,
  "AST", "AST", "Aspartate Aminotransferase (mmol/L)", 3,
  "BILI", "BILI", "Bilirubin (mg/dL)", 4,
  "CREAT", "CREAT", "Creatinine (mg/dL)", 5,
  "PH", "PH", "pH", 6,
  "SODIUM", "SODIUM", "Sodium (mmol/L)", 7,
  "WBC", "WBC", "Leukocytes (10^9/L)", 8
)

adlb3 <- adlb3 |>
  derive_vars_merged_lookup(
    dataset_add = param_lookup,
    by_vars = exprs(LBTESTCD),
    new_vars = exprs(PARAMCD, PARAM, PARAMN)
  )

# Exercise 4: Derive Results - Analysis Value: AVAL, AVALC, and ANRLO, ANRHI, ANRIND.

adlb4 <- adlb3 |>
  mutate(
    AVAL = LBSTRESN,
    AVALC = LBSTRESC,
    ANRLO = as.numeric(LBORNRLO),
    ANRHI = as.numeric(LBORNRHI),
    ANRIND = LBNRIND
  )

# Exercise 5: Derive Timing Variables - Analysis Visit: AVISIT

adlb5 <- adlb4 |>
  mutate(
    AVISIT = VISIT,
    AVISITN = VISITNUM
  )

# Exercise 6: Derive Timing Flag Variables (ONTRTFL)

adlb6 <- adlb5 |>
  mutate(
    ONTRTFL = if_else(TRTSDT <= ADT & ADT <= (TRTEDT + 30), "Y", "")
  )

# Exercise 7: Derive Baseline (ABLFL, BASE, BNRIND)

adlb7 <- adlb6 |>
  mutate(
    ABLFL = LBBLFL
  ) |>
  group_by(USUBJID, PARAMCD) |>
  mutate(
    BASE = if_else(ABLFL == "Y", AVAL, NA),
    BNRIND = if_else(ABLFL == "Y", ANRIND, NA)
  ) |>
  fill(BASE, .direction = "down") |>
  fill(BNRIND, .direction = "down") |>
  ungroup()

# Exercise 8: Analysis Variable flag (ANLxxFL)


# Exercise 9: Derive Change from Baseline (CHG) and Percent Change from Baseline (PCHG),


# Exercise 10: Creating the Lab Grade

grade_lookup <- tribble(
  ~PARAMCD, ~ATOXDSCL, ~ATOXDSCH,
  "ALP", NA_character_, "Alkaline Phosphatase increased",
  "ALT", NA_character_, "Alanine Aminotransferase increased",
  "AST", NA_character_, "Aspartate Aminotransferase increased",
  "BILI", NA_character_, "Blood bilirubin increased",
  "CREAT", NA_character_, "Creatinine increased",
  "PH", "Acidosis", "Alkalosis",
  "SODIUM", "Hyponatremia", "Hypertremia",
  "WBC", "White blood cell decreased", "Leukocytosis"
)


adlb10 <- adlb7 |>
  derive_vars_merged(
    dataset_add = grade_lookup,
    by_vars = exprs(PARAMCD)
  )

adlb11 <- adlb10 |>
  derive_var_atoxgr_dir(
    new_var = ATOXGRL,
    tox_description_var = ATOXDSCL,
    meta_criteria = atoxgr_criteria_ctcv5,
    criteria_direction = "L",
    low_indicator = "LOW",
    get_unit_expr = extract_unit(PARAM)
  ) |>
  derive_var_atoxgr_dir(
    new_var = ATOXGRH,
    tox_description_var = ATOXDSCH,
    meta_criteria = atoxgr_criteria_ctcv5,
    criteria_direction = "H",
    high_indicator = "HIGH",
    get_unit_expr = extract_unit(PARAM)
  )

# WBC

# Grade 1   	Grade 2   	Grade 3   	Grade 4   	Grade 5   
# <LLN - 3000/mm3; <LLN - 3.0 x 10e9 /L	<3000 - 2000/mm3; <3.0 - 2.0 x 10e9 /L	<2000 - 1000/mm3; <2.0 - 1.0 x 10e9 /L	<1000/mm3; <1.0 x 10e9 /L	-
#
# ATOXGRL = case_when(
#   ATOXDSCL == "White blood cell decreased" & !is.na(AVAL) & AVAL < 1.0 & PARAMUNIT == "10^9/L" ~ "Grade 4",
#   AVAL >= 1.0 & PARAMUNIT == "10^9/L" & AVAL < 3.0 ~ "Grade 3",
#   AVAL >= ANRLO ~ "Grade 0",
#   TRUE ~ NA
# )

adlb12 <- adlb11 |>
  group_by(USUBJID, PARAMCD) |>
  mutate(
    BTOXGRH = if_else(ABLFL == "Y", ATOXGRH, NA),
    BTOXGRL = if_else(ABLFL == "Y", ATOXGRL, NA)
  ) |>
  fill(BTOXGRL, .direction = "updown") |>
  fill(BTOXGRH, .direction = "updown") |>
  ungroup()

adlb13 <- adlb12 |>
  mutate(
    ATOXGRLN = ATOXGRL,
    ATOXGRHN = ATOXGRH,
    BTOXGRLN = BTOXGRL,
    BTOXGRHN = BTOXGRH,
    ATOXGRH = if_else(
      is.na(ATOXGRH), NA, paste0("Grade ", ATOXGRH)
    ),
    ATOXGRL = if_else(
      is.na(ATOXGRL), NA, paste0("Grade ", ATOXGRL)
    ),
    BTOXGRH = if_else(
      is.na(BTOXGRH), NA, paste0("Grade ", BTOXGRH)
    ),
    BTOXGRL = if_else(
      is.na(BTOXGRL), NA, paste0("Grade ", BTOXGRL)
    )
  ) |> 
  mutate(
    WTOXGRL = if_else(is.na(ABLFL), "Y", NA), 
    WTOXGRH = if_else(is.na(ABLFL), "Y", NA),
  )


# WTOXGRL - ONTRTFL == "Y", ANLO1FL == "Y", max(ATOXGRLN)
# 4, 3, 2, 1, 0

# ANLO1FL : post-baseline flag






