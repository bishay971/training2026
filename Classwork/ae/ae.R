# *******************************************************************************************************
# Program Name:                 ae.R
# Project:                      TRA-025
# Purpose:                      Create SDTM Dataset of CM
# Original Author:              Binod Jung Bogati (linkedin.com/in/bjungbogati)
# Copyright:                    © 2025. Unauthorized distribution or reuse prohibited.
# Date Created:                 7/23/2025
# Parameters:                   NA
#
# Input:                        raw_ae dm
# Output:                       v_ae v_suppae
# Modifications:
#   Date        By             Changes
# **********  *************  *************************************
#   
# ********************************************************************************************************/



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

ae_data <- raw_ae1 |> 
  mutate(
    STUDYID, 
    DOMAIN,
    USUBJID, 
    AETERM, 
    AELLT, 
    AELLTCD, 
    # AEDECOD = AETERM, 
    AEPTCD, 
    AEHLT, 
    AEHLTCD, 
    AEHLGT, 
    AEHLGTCD, 
    AEBODSYS, 
    AEBDYSCD, 
    AESOC = AEBODSYS, 
    AESOCCD = AEBDYSCD,
    AESEV = str_extract(AETOXGR, "Mild|Moderate|Severe"), 
    AESER, 
    AEACN, 
    AEREL, 
    AEOUT,
    AESDTH,
    AESLIFE,
    AETOXGR = str_extract(AETOXGR, "Grade 1|Grade 2|Grade 3|Grade 4"), 
    AESTDTC, 
    AEENDTC, 
    AESTDY, 
    AEENDY
  ) |> 
  arrange(STUDYID, USUBJID, AETERM, AESTDTC) |> 
  group_by(USUBJID) |> 
  mutate(AESEQ = row_number()) |> 
  ungroup()




suppae <- ae_data |> 
  mutate(
    MEDVER = paste(AETERM_CODERDICTNAME, AETERM_CODERDICTVERSION, sep=" "),
    RDOMAIN = "AE",
    IDVAR = "AESEQ"
  ) |> 
  pivot_longer(
    cols = c(MEDVER),
    names_to = "QNAM", 
    values_to = "QVAL"
  ) |> 
  mutate(
    QLABEL = case_when(
      QNAM == "MEDVER" ~ "Medra Dicitonary Version" 
    ),
    QORIG = "eDT",
    QEVAL = "",
    IDVARVAL = as.character(AESEQ)
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