# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2026. Unauthorized distribution or reuse prohibited.

library(tidyverse)

rsetup::get_data("lb")

# Exercise 1: Transpose required values in respective columns 
# (test results/units/high/low/flags, + high, - low and 0 normal). 

# 5 mins for exercise 1 

lb1 <- raw_lb1 |> 
  mutate(
    LBTESTCD = "WBC", 
    LBORRES = as.character(WBC), 
    LBORRESU = WBC_UN, 
    LBORNRLO = WBC_LOW, 
    LBORNRHI = WBC_HIGH, 
    LBNRIND = case_when(
      WBC_FLAG == "+" ~ "HIGH",
      WBC_FLAG == "0" ~ "NORMAL",
      WBC_FLAG == "-" ~ "LOW"
    ), 
    LBDTC = as.character(LBDT), 
    LBCAT = "Hematology"
  ) |> 
  select(
    SUBJECT, LBCAT, LBTESTCD, LBORRES, LBORRESU, LBORNRLO, LBORNRHI, LBNRIND, LBDTC, INSTANCE
  )

lb2_spec <- data.frame(
  .name = names(raw_lb2)[21:56],
  .value = c("LBORRES", "LBSTRESC", "LBORRESU", "LBORNRLO", "LBORNRHI", "LBNRIND"),
  LBTESTCD = c("CREAT", "SODIUM", "AST", 'ALT', "BILI", "ALP") |> rep(each = 6)
)

lb2 <- raw_lb2 |> 
  pivot_longer_spec(
    lb2_spec
  ) |> 
  mutate(
    LBDTC = as.character(LBDT),
    LBORRES = as.character(LBORRES),
    LBNRIND = case_when(
      LBNRIND == "+" ~ "HIGH",
      LBNRIND == "0" ~ "NORMAL",
      LBNRIND == "-" ~ "LOW"
    ), 
    LBCAT = "Chemistry"
  ) |> 
  select(
    SUBJECT, LBCAT, LBTESTCD, LBORRES, LBSTRESC, LBORRESU, LBORNRLO, LBORNRHI, LBNRIND, LBDTC, INSTANCE
  ) 

?pivot_longer_spec


lb3_spec <- data.frame(
  .name = names(raw_lb3)[21:25],
  .value = c("LBORRES", "LBSTRESC", "LBORNRLO", "LBORNRHI", "LBNRIND"),
  LBTESTCD = c("UPH") |> rep(each = 5)
)

lb3 <- raw_lb3 |> 
  pivot_longer_spec(
    lb3_spec
  ) |> 
  mutate(
    LBDTC = as.character(LBDT),
    LBORRES = as.character(LBORRES),
    LBNRIND = case_when(
      LBNRIND == "+" ~ "HIGH",
      LBNRIND == "0" ~ "NORMAL",
      LBNRIND == "-" ~ "LOW"
    ), 
    LBCAT = "Urinalysis"
  ) |> 
  select(
    SUBJECT, LBCAT, LBTESTCD, LBORRES, LBSTRESC,  LBORNRLO, LBORNRHI, LBNRIND, LBDTC, INSTANCE
  )

lb_data <- bind_rows(
  lb1, lb2, lb3
)

# raw_lb2 |> 
#   pivot_longer(
#     cols = c(CREAT, SODIUM, AST, ALT, ALP, BILI), 
#     names_to = "LBTESTCD", 
#     values_to = "LBORRES"
#   ) |> 
#   # select(SUBJECT, LBTESTCD, LBORRES) |>
#   View()
# 
# raw_lb2 |> 
#   pivot_longer(
#     cols = c(CREAT_UN, SODIUM_UN, AST_UN, ALT_UN, ALP_UN, BILI_UN), 
#     names_to = "LBTESTCD", 
#     values_to = "LORRESU"
#   ) |> 
#   # select(SUBJECT, LBTESTCD, LORRESU) |>
#   View()


# Excercise 2: For Lab test (LBTEST/LBTESTCD) and Lab category (LBCAT), change it as per LOINC LB Mapping document. 

lbcat_fmt <- c(
  "Hematology (Lab Admin)" ~ "Hematology", 
  "Chemistry (Lab Admin)" ~ "Chemistry"
)


test_fmt <- c(
  "WBC" = "Leukocytes", 
  "CREAT" = "Creatinine", 
  "SODIUM" = "Sodium", 
  "AST" = "Aspartate Aminotransferase", 
  "ALT"
)
# testcd_fmt <- c(
#   "WBCD" = "WBC"
# )

lbex2 <- lb_data |> 
  mutate(
    LBTEST = test_fmt[LBTESTCD]
    # LBCAT = case_when(
    #   LBTESTCD == "WBC" ~ "Hematology"
    # )
  )

# Exercise 3: Convert the original values/units (LBORRES/LBORRESU) into standard units (LBSTRESN /LBSTRESU)

#TESTNAME	Original Unit	Conversion factor	Standard Unit
# Leukocytes	10^3/mm^3	1	10^9/L
# Leukocytes	10^3/uL	1	10^9/L
# Leukocytes	K/mm^3	1	10^9/L
# Leukocytes	K/uL	1	10^9/L
# Sodium	mEq/L	1	mmol/L
# Erythrocytes	mil/uL	1	10^12/L
# Erythrocytes	10^6/mm^3	1	10^12/L

lbex3 <- lbex2 |> 
  mutate(
    LBSTRESN = case_when(
      LBORRES != "" & LBTESTCD == "WBC" & LBORRESU %in% c("10^3/mm^3", "10^3/mm^3", "K/mm^3", "K/uL") ~ as.numeric(LBORRES) * 1, 
      LBORRES != "" & LBTESTCD == "SODIUM" & LBORRESU %in% c("mEq/L") ~ as.numeric(LBORRES) * 1,
      TRUE ~ as.numeric(LBORRES)
    ), 
    LBSTRESU = case_when(
      LBORRES != "" & LBTESTCD == "WBC" & LBORRESU %in% c("10^3/mm^3", "10^3/mm^3", "K/mm^3", "K/uL") ~ "10^9/L", 
      LBORRES != "" & LBTESTCD == "SODIUM" & LBORRESU %in% c("mEq/L") ~ "mmol/L",
      TRUE ~ LBORRESU
    )
  )

# eDT - Central lab datasets 

# baseline flag -> check exercise of 05_vs folder; line number ~ 150.

visit_fmt <- c(
  "SCREENING" = "Screening",
  "Screening" = "Screening",
  "Day 1" = "Day 1",
  "Day 2" = "Day 2",
  "Day 3" =   "Day 3",
  "Day 4" =   "Day 4",
  "Day 5" =  "Day 5", 
  "WEEK 6" = "Week 6"
)

visitn_fmt <- c(
  "Screening" = 1,
  "Day 1" = 2,
  "Day 2" = 3,
  "Day 3" = 4,
  "Day 4" = 5,
  "Day 5" = 6,
  "Week 6" = 7
)

lbex4 <- lbex3 |> 
  mutate(
    USUBJID = paste0("TRA-025", "-", SUBJECT),
    VISIT = str_to_title(INSTANCE), 
    VISITNUM = if_else(VISIT == "Screening", 1, 7)
  )

lb_dm <- lbex4 |> 
  left_join(
    select(dm, USUBJID, RFXSTDTC),
    by = "USUBJID"
  )

#
baseline <- lb_dm |> 
  filter(
    LBDTC <= RFXSTDTC, 
    !is.na(LBORRES) & LBORRES != "", 
    !is.na(LBDTC) & LBDTC != ""
  ) |> 
  group_by(USUBJID, LBTESTCD) |>
  filter(VISITNUM == max(VISITNUM)) |> 
  mutate(LBBLFL = "Y") |> 
  ungroup() |> 
  select(USUBJID, LBTESTCD, VISITNUM, LBBLFL, LBDTC) 

lb_blfl <- lb_dm |> 
  left_join(
    baseline, by = c("USUBJID", "LBTESTCD", "VISITNUM", "LBDTC")
  )

# Sorting and --SEQ -> check exercise 4 of 05_vs folder; line number ~ 180. 

# Study day variables

# --SUPP

# Exercise 4: Map all the dataset to final lb dataset.
