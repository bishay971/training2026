# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2026. Unauthorized distribution or reuse prohibited.


library(tidyverse)
rsetup::get_data("ds")


# Exercise 1. Use the RAW_VISIT dataset to derive USUBJID, DSTERM, DSDECOD, DSCAT, EPOCH, DSSTDTC. 

ex1 <- raw_visit |> 
  filter(FOLDERNAME == "Screening") |> 
  mutate(
    USUBJID = paste0("TRA-025", "-", SUBJECT),
    DSTERM = "COMPLETED", 
    DSDECOD = "COMPLETED", 
    DSCAT = "DISPOSITION EVENT", 
    EPOCH = "SCRENNING",
    DSSTDTC = as.character(VISDT)
  ) |> 
  select(USUBJID, DSTERM, DSDECOD, DSCAT, EPOCH, DSSTDTC)


# 200 character per variable
# write_sas(ex1, "ex1.xpt", version = 5)

# Exercise 2. Use the RAW_IE dataset to derive USUBJID, DSTERM, DSDECOD, DSCAT, EPOCH, DSSTDTC. 

ex2 <- raw_ie |> 
  filter(FOLDERNAME == "SCREENING") |> 
  mutate(
    USUBJID = paste0("TRA-025", "-", SUBJECT),
    DSTERM = "INFORMED CONSENT OBTAINED", 
    DSDECOD = "INFORMED CONSENT OBTAINED", 
    DSCAT = "PROTOCOL MILESTONE", 
    EPOCH = "SCREENING",
    DSSTDTC = as.character(DSSTDT)
  ) |> 
  select(USUBJID, DSTERM, DSDECOD, DSCAT, EPOCH, DSSTDTC)

# Exercise 3. Use the RAW_RAND dataset to derive USUBJID, DSTERM, DSDECOD, DSCAT, EPOCH, DSSTDTC.

ex3 <- raw_rand |> 
  mutate(
    USUBJID = paste0("TRA-025", "-", SUBJECT),
    DSTERM = "RANDOMIZED", 
    DSDECOD = "RANDOMIZED", 
    DSCAT = "PROTOCOL MILESTONE", 
    EPOCH = "SCREENING",
    DSSTDTC = paste0(RNDT, "T", RNTM)
  ) |> 
  select(USUBJID, DSTERM, DSDECOD, DSCAT, EPOCH, DSSTDTC)


# Exercise 4. Use the RAW_EX dataset to derive USUBJID, DSTERM, DSDECOD, DSCAT, DSSCAT, EPOCH, DSSTDTC.

ex4 <- raw_ex |> 
  group_by(SUBJECT) |> 
  summarise(
    last_dt = paste0(EXDT, "T", EXENTM) |> max()
  ) |> 
  ungroup() |> 
  mutate(
    USUBJID = paste0("TRA-025", "-", SUBJECT),
    DSTERM = "COMPLETED", 
    DSDECOD = "COMPLETED", 
    DSCAT = "DISPOSITION EVENT", 
    DSSCAT = "STUDY PARTICIPATION", # End of treatment
    EPOCH = "TREATMENT",
    DSSTDTC = last_dt
  ) |> 
  select(USUBJID, DSTERM, DSDECOD, DSCAT, DSSCAT, EPOCH, DSSTDTC)


# Exercise 5. Use the RAW_DS dataset to derive USUBJID, DSTERM, DSDECOD, DSCAT, DSSCAT, EPOCH, DSSTDTC.

ex5 <- raw_ds |> 
  mutate(
    USUBJID = paste0("TRA-025", "-", SUBJECT),
    DSTERM = "COMPLETED", 
    DSDECOD = "COMPLETED", 
    DSCAT = "DISPOSITION EVENT", 
    DSSCAT = "STUDY PARTICIPATION", # End of treatment
    EPOCH = "FOLLOW-UP",
    DSSTDTC = as.character(DSENDT) 
  ) |> 
  select(USUBJID, DSTERM, DSDECOD, DSCAT, DSSCAT, EPOCH, DSSTDTC)

# Exercise 6. Append all the datasets to create ds dataset.

ds_data <- bind_rows(
  ex1, ex2, ex3, ex4, ex5
) |> 
  mutate(STUDYID = "TRA-025") |> 
  arrange(USUBJID, DSSTDTC, DSDECOD) |> 
  group_by(USUBJID) |> 
  mutate(DSSEQ = row_number()) |> 
  ungroup()





# --DY = (date portion of --DTC) - (date portion of RFSTDTC) + 1 if --DTC is on or after RFSTDTC
# --DY = (date portion of --DTC) - (date portion of RFSTDTC) if --DTC precedes RFSTDTC

cal_days <- function(date, ref_date) {
  case_when(
    as.Date(date) >= as.Date(ref_date) ~ as.numeric(as.Date(date) - as.Date(ref_date)) + 1,
    TRUE ~ as.numeric(as.Date(date) - as.Date(ref_date))
  )
}

ds <- ds_data |> 
  left_join(dm, by = c("STUDYID", "USUBJID")) |>
  mutate(
    DOMAIN = "DS",
    DSSTDY = cal_days(DSSTDTC, RFSTDTC)
  ) |> 
  select(
    STUDYID, 
    DOMAIN,
    USUBJID, 
    DSSEQ, 
    DSTERM, 
    DSDECOD, DSCAT, DSSCAT, EPOCH, DSSTDTC,
    DSSTDY
  )


