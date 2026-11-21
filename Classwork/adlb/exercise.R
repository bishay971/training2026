# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2025. Unauthorized distribution or reuse prohibited.

# load(url("https://tinyurl.com/3bc8dcjv"))


library(tidyverse)
library(admiral)

rsetup::get_data("adlb")

# Exercise 1: Write a program to merge LB and ADSL

ex1 <- lb |> 
  left_join(adsl, by = "USUBJID")


# Exercise 2: Derive dates and sequence variable: 
# ADT, ADY, ASEQ

ex2 <- ex1 |> 
  mutate(
    ADT = as.Date(LBDTC), 
    ADY = LBDY,
    ASEQ = LBSEQ
  )


# Exercise 3: Create Parameter: PARAM, PARAMCD, PARAMN, Parameter Category: PARCAT1, PARCAT1N, 

ex3 <- ex2 |> 
  mutate(
    PARAM  = if_else(LBSTRESU != "", paste0( LBTEST , " (",  LBSTRESU, ")"), LBTEST),
    PARAMCD = LBTESTCD, 
    PARAMN = as.factor(PARAMCD) |> as.numeric(), 
    PARCAT1 = LBCAT, 
    PARCAT1N = as.factor(PARCAT1) |> as.numeric()
  )


# Exercise 4: Derive Results - Analysis Value: AVAL, AVALC, and ANRLO, ANRHI, ANRIND.

ex4 <- ex3 |> 
  mutate(
    AVAL = LBSTRESN, 
    AVALC = LBSTRESC, 
    ANRLO = as.numeric(LBSTNRLO), 
    ANRHI = as.numeric(LBSTNRHI), 
    ANRIND = LBNRIND
  )



# Exercise 5: Derive Timing Variables - Analysis Visit: AVISIT

ex5 <- ex4 |> 
  mutate(
    AVISIT = VISIT, 
    AVISITN = VISITNUM
  )

# Exercise 6: Derive Timing Flag Variables (ONTRTFL)

ex6 <- ex5 |> 
  mutate(
    ONTRTFL = if_else(
      TRTSDT <= ADT & ADT <= (TRTEDT + 30), "Y", ""
    )
  )




# Exercise 7: Derive Baseline (ABLFL, BASE, BNRIND)


ex7 <- ex6 |> 
  mutate(
    ABLFL = LBBLFL
  ) |> 
  group_by(USUBJID, PARAMCD) |> 
  mutate(
    BASE = if_else(ABLFL == "Y", AVAL, NA), 
    BNRIND = if_else(ABLFL == "Y", ANRIND, NA), 
  ) |> 
  fill(BASE, .direction = "down") |> 
  fill(BNRIND, .direction = "down") |> 
  ungroup()

# data ex7;
#   set ex6;
#   by USUBJID PARAMCD;
#   retain base;
#   if first.PARAMCD then base = .;
#   if ABLFL = "Y" then base = AVAL;
# run;





# Exercise 8: Analysis Variable flag (ANLxxFL) 

ex8 <- ex7 |> 
  mutate(
    ANL01FL = if_else(
      ADT >= TRTSDT & ABLFL != "Y", "Y", ""
    )
  )


# Exercise 9: Derive Change from Baseline (CHG) and Percent Change from Baseline (PCHG), 

ex9 <- ex8 |> 
  mutate(
    CHG = AVAL - BASE,
    PCHG = (CHG/BASE) * 100
  )

# Exercise 10: Creating the Lab Grade

ex10 <- convert_blanks_to_na(ex9) |> 
  select(-c(PARAM, PARAMCD, PARAMN))

param_lookup <- tribble(		
  ~LBTESTCD, ~PARAMCD, ~PARAM, ~PARAMN,		
  "ALP", "ALP", "Alkaline Phosphatase (mg/dL)", 1,		
  "ALT", "ALT", "Alanine Aminotransferase (mg/dL)", 2,		
  "AST", "AST", "Aspartate Aminotransferase (mmol/L)", 3,		
  "BILI", "BILI","Bilirubin (mg/dL)", 4,		
  "CREAT", "CREAT", "Creatinine (mg/dL)", 5,		
  "PH", "PH", "pH", 6,		
  "SODIUM", "SODIUM", "Sodium (mmol/L)", 7,		
  "WBC", "WBC", "Leukocytes (10^9/L)", 8		
)		

ex11 <- ex10 |> 
  derive_vars_merged_lookup(
    dataset_add = param_lookup, 
    by_vars = exprs(LBTESTCD),
    new_vars = exprs(PARAMCD, PARAM, PARAMN)
  )

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

ex12 <- ex11 |> 
  derive_vars_merged(
    dataset_add = grade_lookup, 
    by_vars = exprs(PARAMCD)
  )

ex13 <- ex12 |> 
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

# ATOXGRL = case_when(
#   PARAMCD == "WBC" & PARAMUNIT == "10^9/L" & AVAL < 1.0  ~ "Grade 4",
#   PARAMCD == "WBC" & PARAMUNIT == "10^9/L" & AVAL < 2.0 & AVAL >= 1.0 ~ "Grade 3",
#   PARAMCD == "WBC" & PARAMUNIT == "10^9/L" & AVAL < 3.0 & AVAL >= 2.0 ~ "Grade 2",
#   PARAMCD == "WBC" & PARAMUNIT == "10^9/L" & AVAL >= 3.0 & AVAL < ANRLO ~ "Grade 1",
#   PARAMCD == "WBC" & PARAMUNIT == "10^9/L" & AVAL >= ANRLO ~ "Grade 0",
#   TRUE ~ NA
# )

ex14 <- ex13 |> 
  group_by(USUBJID, PARAMCD) |> 
  mutate(
    BTOXGRL = if_else(ABLFL == "Y", ATOXGRL, NA), 
    BTOXGRH = if_else(ABLFL == "Y", ATOXGRH, NA), 
  ) |> 
  fill(BTOXGRL, .direction = "down") |> 
  fill(BTOXGRH, .direction = "down") |> 
  ungroup()

ex15 <- ex14 |> 
  mutate(
    ATOXGRLN = ATOXGRL, 
    ATOXGRLH = ATOXGRH, 
    BTOXGRLN = BTOXGRL, 
    BTOXGRLH = BTOXGRH, 
    
    ATOXGRH = if_else(is.na(ATOXGRH), NA, paste("Grade ", ATOXGRH)),
    ATOXGRL = if_else(is.na(ATOXGRL), NA, paste("Grade ", ATOXGRL)),
    BTOXGRH = if_else(is.na(BTOXGRH), NA, paste("Grade ", BTOXGRH)),
    BTOXGRL = if_else(is.na(BTOXGRL), NA, paste("Grade ", BTOXGRL)), 
    
    WTOXGRL = if_else(is.na(ABLFL), "Y", NA),
    WTOXGRH = if_else(is.na(ABLFL), "Y", NA)
  )



# WTOXGRL -> ONTRTFL == "Y", ANL01FL == "Y", max(ATOXGRLN)
# WTOXGRH -> ONTRTFL == "Y", ANL01FL == "Y", max(ATOXGRHN)








