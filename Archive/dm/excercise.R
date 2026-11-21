# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2025. Unauthorized distribution or reuse prohibited.

# load

library(tidyverse)
rsetup::get_data("dm")

# Exercise 1: Write a program to get screening failures (SCFAIL) from raw folder Inclusion/Exclusion (ie) dataset.

ie_data <- raw_ie |> filter(IESFYN == "Yes") |> select(SUBJECT, IESFYN, IESFDT)

ic_data <- raw_ie |> select(SUBJECT, DSSTDT)

# Exercise 2: Write a program to get first and last exposure date of each subject from raw Exposure (ex) dataset.
ex2 <- raw_ex |> 
  mutate(
    first_dt = paste0(EXDT, "T", EXSTTM),
    last_dt = paste0(EXDT, "T", EXENTM) 
  ) |> 
  group_by(SUBJECT) |> 
  summarise(
    RFXSTDTC = min(first_dt), 
    RFXENDTC = max(last_dt) 
  ) |> 
  ungroup() |> 
  select(SUBJECT, RFXSTDTC, RFXENDTC) 



# Exercise 3: Write a program to get death date using raw / Death details (DD) dataset.

dd_data <- raw_dd |> select(subject, dthdt)

# Exercise 4: Write a program to get the End of participation date (RFPENDTC) from raw Disposition (DS) Dataset. 

ds_data <- raw_ds |>
  mutate(
    RFPENDTC = DSENDT
  ) |>
  select(
    SUBJECT, RFPENDTC
  )

# Exercise 5: Write a program to derive race information (as per SDTM CT) from raw demographic (DM) dataset.

ex5 <- raw_dm |> 
  mutate(
    RACE = case_when(
      RACE1 == 1 ~ "American Indian or Alaska Native", 
      RACE2 == 1 ~ "Asian", 
      RACE3 == 1 ~ "Black or African American ", 
      RACE4 == 1 ~ "Native Hawaiian Other Pac Islander ", 
      RACE5 == 1 ~ "White ", 
      RACE6 == 1 ~ "Not Reported", 
    )
  ) |> 
  select(SUBJECT, RACE)

# Exercise 6: Write a program to create planned treatment arms (ARM/ARMCD) from randomization (rand ) dataset 
# and Actual Treatment arms (ACTARM/ACTARMCD) from exposure (ex).

# TA

rand_arm <- raw_rand |> 
  mutate(
    ARM = RNDOSE, 
    ARMCD = if_else(RNDOSE == "Placebo", "PLAC", RNDOSE)
  ) |> 
  select(SUBJECT, ARM, ARMCD)

ex_arm <- raw_ex |> 
  mutate(
    ACTARM = EXTRT, 
    ACTARMCD = if_else(EXTRT == "Placebo", "PLAC", EXTRT)
  ) |> 
  select(SUBJECT, ACTARM, ACTARMCD)



# dm data

# modifying keyboard shortcuts

# Tools > Modify keyboard shortcuts > filter upper or lower case >
# Apply
# Ctrl+shift+ 1, 2,3, 4



sex_fmt <- c(
  "Male" = "M", 
  "Female" = "F"
)

ethnic_fmt <- c(
  "Not Hispanic or Latino" = "NOT HISPANIC OR LATINO", 
  "Hispanic or Latino" = "HISPANIC OR LATINO",
  "Unknown" = "UNKNOWN"
)

# New_name = Old_name
# Old_name = New_name

dm_data <- raw_dm |> 
  # select(SUBJECT, SEX, ETHNIC, AGE, BRTHDTC = BRTHYR) |> # Ctrl+shift+m
  mutate(
    SEX = sex_fmt[SEX],
    ETHNIC = ethnic_fmt[ETHNIC]
  ) |> 
  left_join(ds_data, by = "SUBJECT") |> 
  left_join(ex2, by = "SUBJECT") |> 
  left_join(ex5, by = "SUBJECT") |> 
  left_join(rand_arm, by = "SUBJECT") |>
  left_join(ex_arm, by = "SUBJECT") |> 
  mutate(
    RFSTDTC = RFXSTDTC, 
    RFENDTC = RFXENDTC, 
    AGEU = "Years",
    STUDYID = "TRA-025", 
    USUBJID = paste0(STUDYID, "-", SUBJECT)
  ) |> 
  arrange(STUDYID, USUBJID)

# clean R console > Ctrl+L
















# Exercise 7: Write a program to map information to DM and SUPPDM dataset (DM and SUPPDM). You can reuse the above programs to get desired output.