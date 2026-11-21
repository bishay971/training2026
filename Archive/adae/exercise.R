# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2026. Unauthorized distribution or reuse prohibited.

library(tidyverse)
rsetup::get_data("adae")

# Exercise 1: Write a program to merge AE and SUPPAE dataset.




# Exercise 2: Write a program to create flags related to study drug.
# AESEVFL, AESERFL, AEACNFL, AERELFL, TRTEMFL

adae2 <- ae |> 
  mutate(
    AESEVFL = if_else(AESEV == "SEVERE", "Y", ""), 
    AESERFL = if_else(AESER == "Yes", "Y", ""),, 
    AERELFL = if_else(AEREL == "RELATED", "Y", ""),, 
    AEACNFL = if_else(AEACN == "DRUG WITHDRAWN", "Y", ""),
  )


# Exercise 3: Write a program to create ASTDT, AENDT and TRTP, TRTSDT, TRTEDT from DM/ADSL.

dm1 <- dm |> 
  select(USUBJID, ACTARMCD, ACTARM, RFXSTDTC, RFXENDTC)

ae_dm <- left_join(adae2, dm1, by = "USUBJID") |> 
  mutate(
    TRTSDT = as.Date(RFXSTDTC), 
    TRTEDT = as.Date(RFXENDTC), 
    TRTA = ACTARM
  )

adae3 <- ae_dm |> 
  mutate(
    ASTDT = as.Date(AESTDTC), 
    AENDT = as.Date(AEENDTC)
  )

# TRTEMFL
# If ADSL.TRTSDT <= ASTDT<=(ADSL.TRTEDT +14) then 
# TRTEMFL="Y"

adae3 <- adae3 |> 
  mutate(
    TRTEMFL = if_else(
      TRTSDT <= ASTDT & (TRTEDT + 14) <= ASTDT, "Y", ""
    )
  )




# Exercise 4: Write a program to create ADAE dataset.
