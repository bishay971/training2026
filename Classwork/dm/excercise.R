# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2025. Unauthorized distribution or reuse prohibited.

# load

library(tidyverse)
rsetup::get_data("dm")

# Exercise 1: Write a program to get screening failures (SCFAIL) from raw folder Inclusion/Exclusion (ie) dataset.

# screenfailure
scfail <- raw_ie |>
  filter(IESFYN == "Yes") |>
  select(SUBJECT, IESFYN)

# informed consent signed

ics <- raw_ie |>
  mutate(
    RFICDTC = DSSTDT
  ) |>
  select(SUBJECT, RFICDTC)


# Exercise 2: Write a program to get first and last exposure date of each subject from raw Exposure (ex) dataset.

# exposure first and last date
exps <- raw_ex |>
  mutate(
    first_dt = paste0(EXDT, "T", EXSTTM),
    last_dt = paste0(EXDT, "T", EXENTM)
  ) |>
  # as.POSIXct(paste0(EXDT, EXSTTM))
  group_by(SUBJECT) |>
  summarise(
    RFXSTDTC = min(first_dt),
    RFXENDTC = max(last_dt)
  ) |>
  select(
    SUBJECT, RFXSTDTC, RFXENDTC
  )


# Exercise 3: Write a program to get death date using raw / Death details (DD) dataset.

# death
dd <- raw_dd |>
  select(subject, dthdt)

# Exercise 4: Write a program to get the End of participation date (RFPENDTC) from raw Disposition (DS) Dataset.

# end of study participation
eos <- raw_ds |>
  mutate(RFPENDTC = DSENDT) |> 
  select(SUBJECT, RFPENDTC)


# Exercise 5: Write a program to derive race information (as per SDTM CT) from raw demographic (DM) dataset.

# race
race <- raw_dm |>
  mutate(
    RACE = case_when(
      RACE1 == 1 ~ "AMERICAN INDIAN OR ALASKA NATIVE",
      RACE2 == 1 ~ "ASIAN",
      RACE3 == 1 ~ "BLACK OR AFRICAN AMERICAN",
      RACE4 == 1 ~ "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
      RACE5 == 1 ~ "WHITE",
      RACE6 == 1 ~ "NOT REPORTED"
    )
  ) |> 
  select(SUBJECT, RACE)

# Exercise 6: Write a program to create planned treatment arms (ARM/ARMCD) from randomization (rand ) dataset and
# Actual Treatment arms (ACTARM/ACTARMCD) from exposure (ex).

# randomization
rand <- raw_rand |>
  mutate(
    ARM = RNDOSE,
    ARMCD = if_else(RNDOSE == "Placebo", "PLAC", RNDOSE)
  ) |>
  select(SUBJECT, ARM, ARMCD)

# exposure
exp6 <- raw_ex |>
  mutate(
    ACTARM = EXTRT,
    ACTARMCD = if_else(EXTRT == "Placebo", "PLAC", EXTRT)
  ) |>
  select(SUBJECT, ACTARM, ACTARMCD)


# Exercise 7: Write a program to map information to DM and SUPPDM dataset (DM and SUPPDM). You can reuse the above programs to get desired output.

sex_fmt <- c(
  "Male" = "M",
  "Female" = "F"
)

ethnic_fmt <- c(
  "Unknown" = "UNKNOWN",
  "Not Hispanic or Latino" = "NOT HISPANIC OR LATINO", 
  "Hispanic or Latino" = "HISPANIC OR LATINO"
)

dm_data <- raw_dm |>
  
  select(SUBJECT, SEX, AGE, BRTHYR, ETHNIC, RPRES) |> 
  mutate(SEX = sex_fmt[SEX], 
         ETHNIC = ethnic_fmt[ETHNIC]
  ) |>
  left_join(scfail, by = "SUBJECT") |>
  left_join(ics, by = "SUBJECT") |>
  left_join(dd, by = c("SUBJECT" = "subject")) |>
  left_join(eos, by = "SUBJECT") |>
  left_join(exps, by = "SUBJECT") |>
  left_join(exp6, by = "SUBJECT") |>
  left_join(rand, by = "SUBJECT") |>
  left_join(race, by = "SUBJECT") |> 
  mutate(
    STUDYID = "TRA-025", 
    DOMAIN = "DM", 
    USUBJID = paste(STUDYID, SUBJECT, sep = "-"), 
    SUBJID = SUBJECT
  )

suppdm <- dm_data |> 
  mutate(
    RDOMAIN = "DM",
    IDVAR = ""
  ) |> 
  pivot_longer(
    cols = c(RPRES),
    names_to = "QNAM", 
    values_to = "QVAL"
  ) |> 
  mutate(
    QLABEL = case_when(
      QNAM == "RPRES" ~ "Is subject of childbearing potential?"
    ),
    QORIG = "CRF",
    QEVAL = "",
    IDVARVAL = ""
  ) |> 
  arrange(STUDYID,USUBJID, IDVAR, IDVARVAL, QNAM) |> 
  select(STUDYID, 
         RDOMAIN, 
         USUBJID, 
         IDVAR, 
         IDVARVAL, 
         QNAM, 
         QLABEL, 
         QVAL, 
         QORIG, 
         QEVAL) |> 
  filter(QVAL != "") |> 
  distinct() 

#













