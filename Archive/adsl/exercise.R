# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2025. Unauthorized distribution or reuse prohibited.

library(tidyverse)
rsetup::get_data("adsl")


# Exercise 1: Write a program to merge: DM and SUPPDM

# Exercise 2: Write a program to generate Numeric Variables for Sex, Race, Ethnic (ASEX/ARACE/AETHNIC) variable.

sex_fmt <- c(
  "M" = "Male", 
  "F" = "Female"
)

race_fmt <- c(
  "NOT REPORTED" =   "Not reported",
  "WHITE" = "White",
  "BLACK OR AFRICAN AMERICAN"    =   "Black or African American",
  "ASIAN" = "Asian",
  "AMERICAN INDIAN OR ALASKA NATIVE" =   "American Indian or Alaska Native"
)

adsl2 <- dm |>
  mutate(
    ASEX = sex_fmt[SEX],
    ARACE = race_fmt[RACE], 
    ASEXN = factor(ASEX, levels = sex_fmt) |> as.numeric() # order based on above vector/format
    # AETHNIC
  )


# Exercise 3: Write a program to generate population flags (SAFFL, ITTFL, ENRFL)

adsl3 <- adsl2 |> 
  mutate(
    # RANDFL = if_else(),
    SAFFL = if_else(RFXSTDTC != "", "Y", "N"),
    ITTFL = if_else(SAFFL == "Y" 
                    # & RANDFL != ""
                    , "Y", "N"),
    ENRFL = if_else(RFICDTC != "", "Y", "N")
    # DWTHFL = if_else(DSDECOD == "DRUG WITHDRAWN", "Y", "N")
  ) 


# Exercise 4: Write a program to generate Treatment Variables for planned treatment (TRTP) and actual treatment (TRTA). (Similar to above Exercise 6 of DM)

adsl4 <- adsl3 |> 
  mutate(
    TRTA = ACTARM, 
    TRTP = ARM
  )

# Exercise 5: Write a program to generate Dates / time such as treatment start date / time (TRTSDT, TRTSDTM) and treatment end date / time (TRTEDT, TRTENTM). (In SAS: Keep dates in numeric date9 format and e8601dt for datetime format) 

adsl5 <- adsl4 |> 
  mutate(
    TRTSDT = as.Date(RFXSTDTC), 
    TRTEDT = as.Date(RFXENDTC),
    TRTSDTM = ymd_hm(RFXSTDTC), 
    TRTENTM = ymd_hm(RFXENDTC)
  )

adsl6 <- adsl5 |> 
  arrange(
    USUBJID
  )

# Exercise 6: Write a program to generate ADSL dataset.
