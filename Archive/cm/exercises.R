# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2025. Unauthorized distribution or reuse prohibited.

# load

library(tidyverse)
rsetup::get_data("cm")

var_drops <- c(
  c("SITE", "SUBJECTID", "PROJECTID", "RECORDPOSITION", "TARGETDAYS", "PROJECT",
    "PROJECT", "STUDYID", "SITEGROUP", "SITENUMBER", "SITEID", "INSTANCENAME", 
    "INSTANCEREPEATNUMBER", "INSTANCEID", "PAGEREPEATNUMBER", "FOLDER", 
    "FOLDERID", "FOLDERSEQ", "DATAPAGEID", "RECORDDATE", "RECORDID", 
    "MINCREATED", "MAXUPDATED", "SITEID", "ENVIRONMENTNAME", 
    "STUDYENVSITENUMBER", "SAVETS")
)

raw_cm <- raw_cm |> 
  select(-var_drops, -starts_with("Z_")) 

# DD MMM YYYY ->> YYYY-MM-DD 2020-01-01



# Exercise 1: Write a program to map proper formats for CMSTDTC and CMENDTC from raw CM dataset. 

# names(raw_cm)
# raw_cm$CMSTDT_RAW |> unique()

# print labels
rsetup::print_lbl(raw_cm)
ex1 <- raw_cm |> 
  mutate(
    CMSTDTC = case_when(
      str_detect(CMSTDT_RAW, "UNK") ~ str_sub(CMSTDT_RAW, 8, 11),
      !is.na(dmy(CMSTDT_RAW)) ~ dmy(CMSTDT_RAW) |> as.character()
    ),
    # Replace UN with 01. then convert it to date dmy() then add str_sub(enddate, 1, 10)
    CMENDTC = case_when(
      str_detect(CMENDT_RAW, "UN") ~  str_replace(CMENDT_RAW , "UN", "01") |> 
        dmy() |> str_sub(1, 7),
      !is.na(dmy(CMENDT_RAW)) ~ dmy(CMENDT_RAW) |> as.character()
    )
  ) |> 
  select(SUBJECT, CMSTDT_RAW, CMSTDTC, CMENDTC, CMENDT_RAW ) 

# Exercise 2: Write a program to create Unique Subject Identifier variable and 
# Sequence variable. 

# concatenate studyid and subject to create USUBJID

ex2 <- raw_cm |> 
  mutate(
    USUBJID = paste("TRA-025", SUBJECT,  sep = "-"), 
  ) |> 
  group_by(USUBJID) |> 
  mutate(
    CMSEQ = row_number() # _N_
  ) |> 
  select(USUBJID, CMTRT, CMSEQ)


# Exercise 3: Write a program to calculate Study Day variable. (CMSTDTC, CMENDTC) 


# --DY = (date portion of --DTC) - (date portion of RFSTDTC) + 1 if --DTC is on or after RFSTDTC
# --DY = (date portion of --DTC) - (date portion of RFSTDTC) if --DTC precedes RFSTDTC

# keep SUBJECT in ex1 first

ex3 <- ex1 |> 
  mutate(
    USUBJID = paste("TRA-025", SUBJECT,  sep = "-"), 
  ) |> 
  left_join(
    dm, by = "USUBJID"
  ) |> 
  # date conversion from character dates
  mutate(
    CMSTDT = as.Date(CMSTDTC), 
    CMENDT = as.Date(CMENDTC),
    RFSTDT = as.Date(RFSTDTC),
    RFENDT = as.Date(RFENDTC)
  ) |> 
  mutate(
    CMSTDY = case_when(
      CMSTDT >= RFSTDT ~ as.numeric(CMSTDT - RFSTDT) + 1,
      TRUE ~ as.numeric(CMSTDT - RFSTDT)
    ),
    CMENDY = case_when(
      CMENDT >= RFSTDT ~ as.numeric(CMENDT - RFSTDT) + 1,
      TRUE ~ as.numeric(CMENDT - RFSTDT)
    )
  ) 



# Exercise 4: Take the concomitant medication data. 
# The sample source data also has ATC coded variables for the medication names.
# Plan the mapping of these into cm_mapping.xlsx excel file


# Exercise 5: Compare the mapping you proposed for exercise 4 with the mapping file given by Binod.


# 
# Exercise 6. After completing the comparison, create a single cm.R program to create CM and SUPPCM . 
# After that, validate the CM & SUPPCM datasets with provided dataset (upon request). 
# 
