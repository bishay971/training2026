# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2026. Unauthorized distribution or reuse prohibited.

library(tidyverse)
rsetup::get_data("adcm")

# Exercise 1: Write a program to merge CM and SUPPCM dataset. # 3 mins

supp <- suppcm |> 
  pivot_wider(
    id_cols = c("USUBJID", "IDVARVAL"), 
    names_from = QNAM, 
    values_from = QVAL
  ) |> 
  mutate(
    CMSEQ = as.numeric(IDVARVAL)
  ) |> 
  select(-IDVARVAL)

comb_cm <- left_join(
  cm, 
  supp, 
  by = c("USUBJID", "CMSEQ")
)

# Exercise 2: Write a program to merge above data with Treatment groups and Treatment start date (TRTSDT) and Treatment end date (TRTEDT) from DM/ADSL.

dm1 <- dm |> 
  select(USUBJID, ACTARMCD, ACTARM, RFXSTDTC, RFXENDTC)

cm_dm <- left_join(comb_cm, dm1, by = "USUBJID") |> 
  mutate(
    TRTSDT = as.Date(RFXSTDTC), 
    TRTEDT = as.Date(RFXENDTC), 
    TRTA = ACTARM
  )

# Exercise 3: In CM dataset, impute the partial dates in CMSTDTC and CMENDTC and create respective imputation flag.

adcm3 <- cm_dm |> 
  mutate(
    ASTDT = case_when(
      str_length(CMSTDTC) == 4 ~ paste0(CMSTDTC, "-01-01") |> as.Date(), 
      str_length(CMSTDTC) == 10 ~ as.Date(CMSTDTC)
    ), 
    ASTDTF = case_when(
      str_length(CMSTDTC) == 4 ~ "M", 
      str_length(CMSTDTC) == 10 ~ ""
    ), 
    AENDT = case_when(
      str_length(CMENDTC) == 7 ~ paste0(CMENDTC, "-01") |> as.Date(), 
      str_length(CMENDTC) == 10 ~ as.Date(CMENDTC)
    ), 
    AENDTF = case_when(
      str_length(CMENDTC) == 7 ~ "D", 
      str_length(CMENDTC) == 10 ~ ""
    )
  ) 

# Exercise 4: Create a PREFL (Prior Flag) and ONTRTFL (On Treatment Flag).

adcm4 <- adcm3 |> 
  mutate(
    PREFL = if_else(
      ASTDT < TRTSDT & CMENRF != "ONGOING", "Y", ""
    ), 
    ONTRTFL = if_else(
      CMENRF == "ONGOING", "Y", ""
    )
  )

adcm <- adcm4 |> 
  arrange(USUBJID, CMTRT, CMSTDTC)



# Exercise 5: Write a full program to generate ADCM dataset.
