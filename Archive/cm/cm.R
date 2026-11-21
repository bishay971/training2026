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

# load data and library /package
library(tidyverse)
library(labelled)
library(diffdf)

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


# set formats (named vector)
route_fmt <- c("Intravenous" = "INTRAVENOUS", "Oral" = "ORAL")
unit_fmt <- c("mL" = "mL", "Tablet" = "TABLET")
freq_fmt <- c("QD = 1x per day" = "QD", "X1 = once" = "ONCE")

# ?tidyr


# dose <- numeric
# dostxt <- c(1, "10", "10 mg x per day")

# min(), max()
# if first.subject in ;

cm_data <- raw_cm |> 
  filter(CMTRT != "") |>  # is.na(CMTRT)
  mutate(
    STUDYID = "TRA-025", 
    DOMAIN = "CM", 
    USUBJID = paste("TRA-025", SUBJECT,  sep = "-"), 
    CMCAT = FOLDERNAME,
    # CMYN
    CMTRT = CMTRT,
    # ATC
    # ATCCD
    CMROUTE = route_fmt[CMROUTE] |> unname(),
    CMINDC = CMINDIC,
    CMDOSE = CMDOSE |> as.numeric(), 
    CMDOSU = unit_fmt[CMUNIT] |> unname(),
    CMDOSFRQ = freq_fmt[CMFREQ] |> unname(),
    #CMMEDVER 
    CMENRF = if_else(CMONGO == "Yes", "ONGOING", CMONGO)
  ) |>  
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
    ), 
    EPOCH = case_when(
      CMSTDT < RFSTDT ~ "SCREENING", 
      CMSTDT > RFSTDT & CMENDT < RFENDT ~ "TREATMENT", 
      CMSTDT > RFENDT ~ "FOLLOW-UP"
    )
  ) |> 
  arrange(STUDYID, USUBJID, CMTRT, CMSTDTC) |> 
  group_by(USUBJID) |> 
  mutate(
    CMSEQ = row_number() |> as.numeric() # _N_
  ) |> 
  ungroup()

cm <- cm_data |>
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
    EPOCH,
    CMSTDTC,
    CMENDTC,
    CMSTDY,
    CMENDY,
    CMENRF
  )

# 
# 
# variable_labels <- extract_labels(sdtm_meta, domain = "CM") |> deframe()
# 

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
  EPOCH = "Epoch",
  CMSTDTC = "Start Date/Time of Medication",
  CMENDTC = "End Date/Time of Medication",
  CMSTDY = "Study Day of Start of Medication",
  CMENDY = "Study Day of End of Medication",
  CMENRF = "End Relative to Reference Period"
)

cm <- cm |> 
  set_variable_labels(
    .labels = variable_labels
  )

# Apply labels
cm <- cm |>
  set_variable_labels(.labels = variable_labels)

diffdf(
  base = cm, 
  compare = v_cm,
)

# supplemental

suppcm <- cm_data |> 
  mutate(
    CMDICVER = paste(CMTRT_CODERDICTNAME, CMTRT_CODERDICTVERSION),
    ATC = CMTRT_ATC, 
    ATCCD = CMTRT_ATC_CODE,
    RDOMAIN = DOMAIN, 
    IDVAR = "CMSEQ"
  ) |> 
  pivot_longer(
    cols = c(CMDICVER, ATC, ATCCD), 
    names_to = "QNAM", 
    values_to = "QVAL"
  ) |> 
  mutate(
    QLABEL = case_when(
      QNAM == "ATC" ~ "ATC Level 1 Term", 
      QNAM == "ATCCD" ~ "ATC Level 1 Code",
      QNAM == "CMDICVER" ~ "Medical Coding Dictionary & Version"
    ), 
    IDVARVAL = CMSEQ |> as.character(), 
    QORIG = "eDT", 
    QEVAL = ""
  ) |> 
  arrange(STUDYID, USUBJID, IDVAR, IDVARVAL, QNAM) |> 
  select(
    STUDYID, 
    RDOMAIN, 
    USUBJID, 
    IDVAR, 
    IDVARVAL, QNAM, QLABEL, QVAL, QORIG, 
    QEVAL
  )


diffdf(
  base = suppcm, 
  compare = v_suppcm,
)




