# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2025. Unauthorized distribution or reuse prohibited.

# load

library(tidyverse)

rsetup::get_data("cm")

var_drops <- c("PROJECTID", "RECORDPOSITION", "TARGETDAYS", "PROJECT", "PROJECT", "STUDYID", 
               "SITEGROUP", "SITENUMBER", "SITEID", "INSTANCENAME", "INSTANCEREPEATNUMBER", 
               "INSTANCEID", "PAGEREPEATNUMBER", "FOLDER", "FOLDERID", "FOLDERSEQ", "DATAPAGEID", 
               "RECORDDATE", "RECORDID", "MINCREATED", "MAXUPDATED", "SITEID", "ENVIRONMENTNAME", 
               "STUDYENVSITENUMBER",  "SAVETS", "SUBJECTID", "SITE")

raw_cm1 <- raw_cm |> 
  select(
   -var_drops, -starts_with("Z_")
  ) 

# library(haven)
# raw_cm <- read_sas("path/file/raw_cm.sas7bdat")

# Exercise 1: Write a program to map proper formats for CMSTDTC and CMENDTC from raw CM dataset. 

raw_cm |>
  mutate(
    CMSTDTC = case_when(
      str_detect(CMSTDT_RAW, "UNK") ~ str_sub(CMSTDT_RAW, 8, 11),
      !is.na(dmy(CMSTDT_RAW)) ~ dmy(CMSTDT_RAW) |> as.character()
    ),
    CMENDTC = case_when(
      str_detect(CMENDT_RAW, "UN") ~ str_replace(CMENDT_RAW, "UN", "01") |> dmy() |> str_sub(1, 7),
      !is.na(dmy(CMENDT_RAW)) ~ dmy(CMENDT_RAW) |> as.character()
    )
  ) |>
  select(SUBJECT, CMSTDTC, CMSTDT_RAW, CMENDT_RAW, CMENDTC)

# that is your comment

# Exercise 2: Write a program to create Unique Subject Identifier variable and Sequence variable. 
# Concatenate STUDYID and SUBJECT with seperator "-"
# STUDYID = "TRA-025"

raw_cm |> 
  mutate(USUBJID= paste("TRA-025",SUBJECT, sep="-")
         )  |> 
  # arrange()
  group_by(USUBJID) |> 
  mutate( CMSEQ = row_number()) |> 
  select(USUBJID, CMSEQ)

# Exercise 3: Write a program to calculate Study Day variable. (CMSTDTC, CMENDTC) 

raw_cm |>
  mutate(
    CMSTDTC = case_when(
      str_detect(CMSTDT_RAW, "UNK") ~ str_sub(CMSTDT_RAW, 8, 11),
      !is.na(dmy(CMSTDT_RAW)) ~ dmy(CMSTDT_RAW) |> as.character()
    ),
    CMENDTC = case_when(
      str_detect(CMENDT_RAW, "UN") ~ str_replace(CMENDT_RAW, "UN", "01") |> dmy() |> str_sub(1, 7),
      !is.na(dmy(CMENDT_RAW)) ~ dmy(CMENDT_RAW) |> as.character()
    ),
    
    USUBJID= paste("TRA-025",SUBJECT, sep="-")
  ) |>
  left_join(dm, by = "USUBJID") |> 
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
    ) ,
    
    CMENDY = case_when(
      CMENDT >= RFSTDT ~ as.numeric(CMENDT - RFSTDT) + 1,
      # condition for is.na ~ value,
      TRUE ~ as.numeric(CMENDT - RFSTDT), 

    )
  ) |> View()

# if 
# else if 
# else # TRUE
#   
#   
#   !is.na()



# PAGE 42 SDTM IG
# --DY = (date portion of --DTC) - (date portion of RFSTDTC) + 1 if --DTC is on or after RFSTDTC
# --DY = (date portion of --DTC) - (date portion of RFSTDTC) if --DTC precedes RFSTDTC


# Exercise 4: Take the concomitant medication data. 
# The sample source data also has ATC coded variables for the medication names.
# Plan the mapping of these into cm_mapping.xlsx excel file



# Exercise 5: Compare the mapping you proposed for exercise 4 with the mapping file given by Binod.


# 
# Exercise 6. After completing the comparison, create a single cm.R program to create CM and SUPPCM . 
# After that, validate the CM & SUPPCM datasets with provided dataset (upon request). 
# 
