# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2026. Unauthorized distribution or reuse prohibited.

library(tidyverse)
rsetup::get_data("ds")


# Exercise 1. Use the RAW_VISIT dataset to derive USUBJID, DSTERM, DSDECOD, DSCAT, EPOCH, DSSTDTC. 

ex1 <- raw_visit |> 
  filter(FOLDERNAME == "Screening") |> 
  mutate(
    USUBJID = paste("TRA-025", SUBJECT, sep = "-"), 
    DSTERM = "COMPLETED", 
    DSDECOD = "COMPLETED", 
    DSCAT = "DISPOSITION EVENT", 
    EPOCH = "SCREENING", 
    DSSTDTC = as.character(VISDT)
  ) |> 
  select(USUBJID, DSTERM, DSDECOD, DSCAT, EPOCH, DSSTDTC)

# Exercise 2. Use the RAW_IE dataset to derive USUBJID, DSTERM, DSDECOD, DSCAT, EPOCH, DSSTDTC. 

ex2 <- raw_ie |> 
  # filter() |> 
  mutate(
    USUBJID = paste("TRA-025", SUBJECT, sep = "-"), 
    DSTERM = "INFORMED CONSENT OBTAINED", 
    DSDECOD = "INFORMED CONSENT OBTAINED", 
    DSCAT = "PROTOCOL MILESTONE", 
    DSSCAT = "STUDY PARTICIPATION",
    EPOCH = "SCREENING", 
    DSSTDTC = as.character(DSSTDT)
  ) |> 
  select(USUBJID, DSTERM, DSDECOD, DSCAT, DSSCAT, EPOCH, DSSTDTC)



# Exercise 3. Use the RAW_RAND dataset to derive USUBJID, DSTERM, DSDECOD, DSCAT, EPOCH, DSSTDTC. 

ex3 <- raw_rand |> 
  # filter() |> 
  mutate(
    USUBJID = paste("TRA-025", SUBJECT, sep = "-"), 
    DSTERM = "RANDOMIZED", 
    DSDECOD = "RANDOMIZED", 
    DSCAT = "PROTOCOL MILESTONE", 
    EPOCH = "SCREENING", 
    DSSTDTC = paste(RNDT, RNTM, sep = "T")
  ) |> 
  select(USUBJID, DSTERM, DSDECOD, DSCAT, EPOCH, DSSTDTC)

# Exercise 4. Use the RAW_EX dataset to derive USUBJID, DSTERM, DSDECOD, DSCAT, DSSCAT, EPOCH, DSSTDTC.

ex4 <- raw_ex |> 
  group_by(SUBJECT) |> 
  summarise(
    last_subj = paste0(EXDT, "T", EXENTM) |> max()
  ) |> 
  ungroup() |> 
  # filter() |> 
  mutate(
    USUBJID = paste("TRA-025", SUBJECT, sep = "-"), 
    DSTERM = "COMPLETED", 
    DSDECOD = "COMPLETED", 
    DSCAT = "DISPOSITION EVENT", 
    DSSCAT = "STUDY PARTICIPATION",
    EPOCH = "TREATMENT", 
    DSSTDTC = last_subj
  ) |> 
  select(USUBJID, DSTERM, DSDECOD, DSCAT, DSSCAT, EPOCH, DSSTDTC)



# Exercise 5. Use the RAW_DS dataset to derive USUBJID, DSTERM, DSDECOD, DSCAT, DSSCAT, EPOCH, DSSTDTC.

ex5 <- raw_ds |> 
  # filter() |> 
  mutate(
    USUBJID = paste("TRA-025", SUBJECT, sep = "-"), 
    DSTERM = "COMPLETED", 
    DSDECOD = "COMPLETED", 
    DSCAT = "DISPOSITION EVENT", 
    DSSCAT = "STUDY PARTICIPATION", 
    EPOCH = "FOLLOW-UP", 
    DSSTDTC = as.character(DSENDT)
  ) |> 
  select(USUBJID, DSTERM, DSDECOD, DSCAT, DSSCAT, EPOCH, DSSTDTC)

# Exercise 6. Append all the datasets to create ds dataset.

ds_data <- bind_rows(
  ex1, ex2, ex3, ex4, ex5
) |> 
  arrange(USUBJID, DSSTDTC, DSDECOD) |> 
  group_by(USUBJID) |> 
  mutate(DSSEQ = row_number()) |> 
  ungroup()



