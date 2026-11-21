# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2025. Unauthorized distribution or reuse prohibited.


library(tidyverse)
rsetup::get_data("ae")

# remove operational variable
var_drops <- c(
  "PROJECTID", "RECORDPOSITION", "TARGETDAYS", "PROJECT", "PROJECT", "STUDYID",
  "SITEGROUP", "SITENUMBER", "SITEID", "INSTANCENAME", "INSTANCEREPEATNUMBER",
  "INSTANCEID", "PAGEREPEATNUMBER", "FOLDER", "FOLDERID", "FOLDERSEQ", "DATAPAGEID",
  "RECORDDATE", "RECORDID", "MINCREATED", "MAXUPDATED", "SITEID", "ENVIRONMENTNAME",
  "STUDYENVSITENUMBER", "SAVETS", "SUBJECTID", "SITE"
)

raw_ae1 <- raw_ae |>
  select(
    -all_of(var_drops), -starts_with("Z_")
  ) 


# Exercise 1: Write a program to map proper formats for AESTDTC and AEENDTC from raw AE dataset. 

names(raw_ae1)

raw_ex1 <- raw_ae1 |> 
  mutate(
    AESTDTC = case_when(
      AESTDT_RAW == "" ~ NA,
      AESTDT_RAW != ""  ~ paste(dmy(AESTDT_RAW), AESTTM, sep="T"),
      AESTDT_RAW != "" & AESTTM == "" ~ dmy(AESTDT_RAW) |> as.character()
    ),
    AEENDTC = case_when(
      is.na(AEENDT_RAW) ~ NA,
      AEENDT_RAW != ""  ~ paste(dmy(AEENDT_RAW), AEENTM, sep="T"),
      AEENDT_RAW != "" & AEENTM == "" ~ dmy(AEENDT_RAW) |> as.character()
    ),
  ) |> 
  select(SUBJECT, AESTDTC, AEENDTC) 


# Exercise 2: Write a program to create Unique Subject Identifier variable and Sequence variable. 



# Exercise 3: Write a program to calculate Study Day variable. (AESTDTC, AEENDTC) 




# Exercise 4: Write a program to calculate Adverse Event Treatment Emergent (AETRTEM).

raw_ex1 |> 
  mutate(
    AETRTEM = if_else(AESTDTC > RFSTDTC, "Y", "N")
  )


# Exercise 5: See the Mapping specs to get required information to create AE and SUPPAE datasets.



# Exercise 6: After that, validate the AE & SUPPAE datasets with provided dataset (upon request).
