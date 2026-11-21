# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2025. Unauthorized distribution or reuse prohibited.

library(tidyverse)
library(labelled)
library(diffdf)

rsetup::get_data("ae")

var_drops <- c(
  c("SITE", "SUBJECTID", "PROJECTID", "RECORDPOSITION", "TARGETDAYS", "PROJECT",
    "PROJECT", "STUDYID", "SITEGROUP", "SITENUMBER", "SITEID", "INSTANCENAME", 
    "INSTANCEREPEATNUMBER", "INSTANCEID", "PAGEREPEATNUMBER", "FOLDER", 
    "FOLDERID", "FOLDERSEQ", "DATAPAGEID", "RECORDDATE", "RECORDID", 
    "MINCREATED", "MAXUPDATED", "SITEID", "ENVIRONMENTNAME", 
    "STUDYENVSITENUMBER", "SAVETS")
)

raw_ae <- raw_ae |> 
  select(-all_of(var_drops), -starts_with("Z_")) 

# Exercise 1: Write a program to map proper formats for AESTDTC and AEENDTC from raw AE dataset. 
print_lbl(raw_ae)

ex1 <- raw_ae |> mutate(
  AESTDTC = case_when(
    str_detect(AESTDT_RAW, "UNK") ~ str_sub(AESTDT_RAW, 8, 11),
    !is.na(dmy(AESTDT_RAW)) & AESTTM != "" ~ paste0(dmy(AESTDT_RAW), "T", AESTTM), 
    !is.na(dmy(AESTDT_RAW)) ~ dmy(AESTDT_RAW) |> as.character()
  ), 
  AEENDTC = case_when(
    str_detect(AEENDT_RAW, "UNK") ~ str_sub(AEENDT_RAW, 8, 11),
    !is.na(dmy(AEENDT_RAW)) & AEENTM != "" ~ paste0(dmy(AEENDT_RAW), "T", AEENTM), 
    !is.na(dmy(AEENDT_RAW)) ~ dmy(AEENDT_RAW) |> as.character()
  )
) 

# Exercise 2: Write a program to create Unique Subject Identifier variable and Sequence variable. 

ex2 <- raw_ae |> 
  mutate(
    STUDYID = "TRA-025",
    USUBJID = paste(STUDYID, SUBJECT,  sep = "-"),
  ) |> 
  group_by(USUBJID) |> 
  mutate(
    AESEQ = row_number()
  ) |> 
  select(STUDYID, USUBJID, AESEQ) |> 
  ungroup() |> 
  slice_max(AESEQ)

# first.subject, last.subject continue in DM


# Exercise 3: Write a program to calculate Study Day variable. (AESTDTC, AEENDTC) 

# Assignment



# Exercise 4: Write a program to calculate Adverse Event Treatment Emergent (AETRTEM).

raw_ae |> 
  mutate(
    AETRTEM = if_else(AESTDY > 1, "Y", "N")
  )


# Exercise 5: See the Mapping specs to get required information to create AE and SUPPAE datasets.


# Exercise 6: After that, validate the AE & SUPPAE datasets with provided dataset (upon request).
