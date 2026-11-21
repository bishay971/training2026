# *******************************************************************************************************
# Program Name:                 cm.R
# Project:                      TRA-025
# Purpose:                      Create SDTM Dataset of CM
# Original Author:              Binod Jung Bogati (linkedin.com/in/bjungbogati)
# Copyright:                    © 2025. Unauthorized distribution or reuse prohibited.
# Date Created:                 7/23/2025
# Parameters:                   NA
#
# Input:                        raw_cm dm
# Output:                       v_cm v_suppcm
# Modifications:
#   Date        By             Changes
# **********  *************  *************************************
#
# ********************************************************************************************************/

# load # Day 5

library(tidyverse)
library(labelled)
library(diffdf)

rsetup::get_data("cm")

# remove operational variable
var_drops <- c(
  "PROJECTID", "RECORDPOSITION", "TARGETDAYS", "PROJECT", "PROJECT", "STUDYID",
  "SITEGROUP", "SITENUMBER", "SITEID", "INSTANCENAME", "INSTANCEREPEATNUMBER",
  "INSTANCEID", "PAGEREPEATNUMBER", "FOLDER", "FOLDERID", "FOLDERSEQ", "DATAPAGEID",
  "RECORDDATE", "RECORDID", "MINCREATED", "MAXUPDATED", "SITEID", "ENVIRONMENTNAME",
  "STUDYENVSITENUMBER", "SAVETS", "SUBJECTID", "SITE"
)

raw_cm1 <- raw_cm |>
  select(
    -var_drops, -starts_with("Z_")
  ) |> 
  filter(CMTRT != "")

freq_fmt <- c(
  "X1 = once" = "ONCE",
  "QD = 1x per day" = "QD"
)

unit_fmt <- c(
  "mL" = "mL",
  "Tablet" = "TABLET"
)

route_fmt <- c(
  "Intravenous" = "INTRAVENOUS",
  "Oral" = "ORAL"   
)

cm_mapping <- raw_cm1 |>
  mutate(
    STUDYID = "TRA-025",
    DOMAIN = "CM",
    USUBJID = paste(STUDYID, SUBJECT, sep = "-"),
    # CMSEQ
    CMTRT = CMTRT,
    CMCAT = FOLDERNAME,
    CMROUTE = route_fmt[CMROUTE] |> unname(), # fix
    CMINDC = CMINDIC,
    CMDOSE = as.numeric(CMDOSE) ,
    CMDOSU = unit_fmt[CMUNIT] |> unname(), # fix
    CMDOSFRQ = freq_fmt[CMFREQ] |> unname(), # fix
    CMENRF = if_else(CMONGO == "Yes", "ONGOING", "")
  ) |> 
  mutate(
    CMSTDTC = case_when(
      str_detect(CMSTDT_RAW, "UNK") ~ str_sub(CMSTDT_RAW, 8, 11),
      !is.na(dmy(CMSTDT_RAW)) ~ dmy(CMSTDT_RAW) |> as.character()
    ),
    CMENDTC = case_when(
      str_detect(CMENDT_RAW, "UN") ~ str_replace(CMENDT_RAW, "UN", "01") |> dmy() |> str_sub(1, 7),
      !is.na(dmy(CMENDT_RAW)) ~ dmy(CMENDT_RAW) |> as.character()
    ), 
    
    CMENDTC = if_else(is.na(CMENDTC), "", CMENDTC) # fix
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
      TRUE ~ as.numeric(CMENDT - RFSTDT)
    ),
    
    EPOCH = case_when(
      CMSTDT < RFSTDT ~ "SCREENING", 
      CMSTDT > RFSTDTC & CMENDT < RFENDTC ~ "TREATMENT",
      CMSTDT > RFENDT ~ "FOLLOW-UP"
    ) # fix
    
    
    
  ) |> 
  arrange(STUDYID, USUBJID, CMTRT, CMSTDTC) |> 
  group_by(USUBJID) |> 
  mutate(CMSEQ = row_number() |> as.numeric()) |> 
  ungroup()

cm <- cm_mapping |> 
  select(
    STUDYID, 
    DOMAIN, 
    USUBJID, 
    CMSEQ, 
    CMTRT, 
    CMCAT, 
    CMDOSE, 
    CMDOSU,
    CMDOSFRQ,
    CMROUTE, 
    CMSTDTC, 
    CMENDTC, 
    CMSTDY,
    CMENDY,
    EPOCH,
    CMENRF
  )

# Create a named vector of labels
variable_labels <- c(
  STUDYID = "Study Identifier",
  DOMAIN = "Domain Abbreviation",
  USUBJID = "Unique Subject Identifier",
  CMSEQ = "Sequence Number",
  CMTRT = "Reported Name of Drug, Med, or Therapy",
  CMCAT = "Category for Medication",
  CMDOSE = "Dose",
  CMDOSU = "Dose Units",
  CMDOSFRQ = "Dose Frequency per Interval",
  CMROUTE = "Route of Administration",
  CMSTDTC = "Start Date/Time of Medication",
  CMENDTC = "End Date/Time of Medication",
  CMSTDY = "Study Day of Start of Medication",
  CMENDY = "Study Day of End of Medication",
  EPOCH = "Epoch",
  CMENRF = "End Relative to Reference Period"
)

# Apply labels
cm <- cm |>
  set_variable_labels(.labels = variable_labels)

diffdf(
  base = cm, 
  compare = v_cm
)



suppcm <- cm_mapping |> 
  mutate(
    CMDICVER = paste(CMTRT_CODERDICTNAME, CMTRT_CODERDICTVERSION, sep=" "),
    RDOMAIN = "CM",
    IDVAR = "CMSEQ", 
    ATC = CMTRT_ATC, 
    ATCCD = CMTRT_ATC_CODE,
  ) |> 
  pivot_longer(
    cols = c(ATC, ATCCD, CMDICVER),
    names_to = "QNAM", 
    values_to = "QVAL"
  ) |> 
  mutate(
    QLABEL = case_when(
      QNAM == "ATC" ~ "ATC Level 1 Term", 
      QNAM == "ATCCD" ~ "ATC Level 1 Code",
      QNAM == "CMDICVER" ~ "Medical Coding Dictionary & Version" 
    ),
    QORIG = "eDT",
    QEVAL = "",
    IDVARVAL = as.character(CMSEQ)
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
         QEVAL)

# fix
variable_labels <- c(
  STUDYID = "Study Identifier",
  RDOMAIN = "Related Domain Abbreviation",
  USUBJID = "Unique Subject Identifier",
  IDVAR = "Identifying Variable",
  IDVARVAL = "Identifying Variable Value",
  QNAM = "Qualifier Variable Name",
  QLABEL = "Qualifier Variable Label",
  QVAL = "Data Value",
  QORIG = "Origin",
  QEVAL = "Evaluator"
)

# Apply labels
suppcm <- suppcm |>
  set_variable_labels(.labels = variable_labels)

diffdf(
  base = suppcm, 
  compare = v_suppcm
)


