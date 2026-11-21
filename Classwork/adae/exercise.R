# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2026. Unauthorized distribution or reuse prohibited.


library(tidyverse)
get_data("adae")

# Exercise 1: Write a program to merge AE and SUPPAE dataset.

supp <- suppae |> 
  pivot_wider(
    id_cols = c("USUBJID", "IDVARVAL"),
    names_from = QNAM,
    values_from = QVAL) |>  
  mutate (
    AESEQ=as.numeric(IDVARVAL)
  ) |> 
  select (-IDVARVAL)


comb_ae <- left_join(ae,supp, by=c("USUBJID", "AESEQ"))


# Exercise 2: Write a program to create flags related to study drug.
# AESEVFL, AESERFL, AEACNFL, AERELFL, TRTEMFL

ae2 <- comb_ae |> 
  mutate(
    AESEVFL = if_else(AESER == "Yes", "Y", "N"), 
    AESERFL = if_else(AESEV == "SEVERE", "Y", "N"), 
    AERELFL = if_else(AEREL == "RELATED", "Y", "N"), 
    AEACNFL = if_else(AEACN == "DRUG WITHDRAWN", "Y", "N"), 
    TRTEMFL = if_else(AETRTEM == "Y", "Y", "N") # SDTM.AE
  )


# Exercise 3: Write a program to create AESTDT, AEENDT and TRTP, TRTSDT, TRTEDT from DM/ADSL.

ae3 <- ae2 |> 
  left_join(
    select(dm, USUBJID, ARM, RFSTDTC, RFENDTC), by = "USUBJID"
  ) |> 
  mutate(
    AESTDT = as.Date(AESTDTC), 
    AEENDT = as.Date(AEENDTC), 
    TRTP = ARM, 
    TRTSDT = as.Date(RFSTDTC), 
    TRTEDT = as.Date(RFENDTC)
  )

# Exercise 4: Write a program to create ADAE dataset.
