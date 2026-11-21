# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2026. Unauthorized distribution or reuse prohibited.


library(tidyverse)

rsetup::get_data("lb")

# Exercise 1: Transpose required values in respective columns (test results/units/high/low/flags, + high, - low and 0 normal). 

lb1 <- raw_lb1 |> 
  mutate(
    LBTESTCD = "WBC",
    LBORRES = WBC_RAW, 
    LBORRESU = WBC_UN, 
    LBORNRHO = WBC_LOW, 
    LBORNRHI = WBC_HIGH, 
    LBNRIND = case_when(
      WBC_FLAG == "+" ~ "HIGH",
      WBC_FLAG == "-" ~ "LOW",
      WBC_FLAG == "0" ~ "NORMAL"
    ),
    LBDTC = as.character(LBDT), 
    LBCAT = "HEMATOLOGY"
  ) |> 
  select(
    SUBJECT, LBCAT, LBTESTCD, LBORRES, LBORRESU, LBORNRHO, LBORNRHI, LBNRIND, LBDTC
  )

# raw_lb2 |> 
#   pivot_longer(
#     cols = c(CREAT, SODIUM, AST, ALT, ALP, BILI),
#     names_to = "LBTESTCD", 
#     values_to = "LBORRES"
#   )

lb2_spec <- data.frame(
  .name = names(raw_lb2)[21:56], 
  .value = c("LBORRES", "LBORRESC", "LBORRESU", "LBORNRHO", "LBORNRHI", "LBNRIND"), 
  LBTESTCD = c("CREAT", "SODIUM", "AST", "ALT", "BILI", "ALP") |> rep(each = 6)
)

lb2 <- raw_lb2 |> 
  pivot_longer_spec(lb2_spec) |> 
  mutate(
    LBNRIND = case_when(
      LBNRIND == "+" ~ "HIGH",
      LBNRIND == "-" ~ "LOW",
      LBNRIND == "0" ~ "NORMAL"
    ),
    LBDTC = as.character(LBDT), 
    LBORRES = as.character(LBORRES), 
    LBCAT = "CHEMISTRY"
  ) |> 
  select(
    SUBJECT, LBCAT, LBTESTCD, LBORRES, LBORRESU, LBORNRHO, LBORNRHI, LBNRIND, LBDTC
  )


# lb3  

lb3_spec <- data.frame(
  .name = names(raw_lb3)[21:25], 
  .value = c("LBORRES", "LBORRESC", "LBORNRHO", "LBORNRHI", "LBNRIND"), 
  LBTESTCD = c("UPH") 
)

lb3 <- raw_lb3 |> 
  pivot_longer_spec(lb3_spec) |> 
  mutate(
    LBNRIND = case_when(
      LBNRIND == "+" ~ "HIGH",
      LBNRIND == "-" ~ "LOW",
      LBNRIND == "0" ~ "NORMAL"
    ),
    LBDTC = as.character(LBDT), 
    LBORRES = as.character(LBORRES), 
    LBCAT = "URINALYSIS"
  ) |> 
  select(
    SUBJECT, LBCAT, LBTESTCD, LBORRES, LBORNRHO, LBORNRHI, LBNRIND, LBDTC
  )

lb_data <- bind_rows(lb1, lb2, lb3)


# Excercise 2: For Lab test (LBTEST/LBTESTCD) and Lab category (LBCAT), change it as per LOINC LB Mapping document. 

test_fmt <- c(
  "WBC" = "Leukocytes", 
  "CREAT" = "Creatinine", 
  "SODIUM" = "Sodium", 
  "AST" = "Aspartate Aminotransferase", 
  "ALT" = "Alanine Aminotransferase", 
  "BILI" = "Total Bilirubin",
  "ALP" = "Alkaline Phosphatase", 
  "UPH" = "pH"
)

lbex2 <- lb_data |> 
  mutate(
    LBTEST = test_fmt[LBTESTCD]
  )


# Exercise 3: Convert the original values/units (LBORRES/LBORRESU) into standard units (LBSTRESN /LBSTRESU)

# Test Name	Current Unit	Conversion Factor	Target Unit
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
      LBORRES != "" & LBTEST == "Leukocytes" & 
        LBORRESU %in% c("10^3/mm^3", "10^3/uL", 
                        "K/mm^3", "K/uL") ~ as.numeric(LBORRES) * 1, 
      
      TRUE ~ as.numeric(LBORRES)
    ), 
    
    LBSTRESU = case_when(
      LBORRES != "" & LBTEST == "Leukocytes" & 
        LBORRESU %in% c("10^3/mm^3", "10^3/uL", 
                        "K/mm^3", "K/uL") ~ "10^9/L", 
      TRUE ~ LBORRESU
    )
    
    
    
    
    
    
    # Exercise 4: Map all the dataset to final lb dataset.
    