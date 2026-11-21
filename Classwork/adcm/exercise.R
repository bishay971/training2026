# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2026. Unauthorized distribution or reuse prohibited.

library(tidyverse)

rsetup::get_data("adcm")

# Exercise 1: Write a program to merge CM and SUPPCM dataset.

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

comb_cm <- left_join(cm, supp, by = c("USUBJID", "CMSEQ"))


# Exercise 2: Write a program to merge above data with Treatment groups and Treatment start date (TRTSDT) and Treatment end date (TRTEDT) from DM/ADSL.

adcm <- comb_cm |>
  left_join(
    select(dm, USUBJID, RFSTDTC, RFENDTC, ARMCD),
    by = "USUBJID"
  ) |>
  mutate(
    TRTSDT = as.Date(RFSTDTC),
    TRTEDT = as.Date(RFENDTC)
  )

# Exercise 3: In CM dataset, impute the partial dates in CMSTDTC and CMENDTC and create respective imputation flag.

adcm3 <- adcm |>
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
      str_length(CMENDTC) == 4 ~ paste0(CMENDTC, "-01-01") |> as.Date(),
      str_length(CMENDTC) == 7 ~ paste0(CMENDTC, "-01") |> as.Date(),
      str_length(CMENDTC) == 10 ~ as.Date(CMENDTC)
    ),
    AENDTF = case_when(
      str_length(CMENDTC) == 4 ~ "M",
      str_length(CMENDTC) == 7 ~ "D",
      str_length(CMENDTC) == 10 ~ "",
      TRUE ~ ""
    )
  ) 


# Exercise 4: Create a PREFL (Prior Flag) and   ONTRTFL (On Treatment Flag).

adcm4 <- adcm3 |> 
  mutate(
    PREFL = if_else(ASTDT < TRTSDT & CMENRF == "", "Y", ""), 
    ONTRTFL = if_else((ASTDT > TRTSDT & AENDT < TRTEDT) | CMENRF == "ONGOING", "Y", "")
  )

# Exercise 5: Write a full program to generate ADCM dataset.





