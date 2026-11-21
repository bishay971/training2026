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


ae_data <- raw_ae |> 
mutate(
  STUDYID = "TRA-025", 
  DOMAIN = "AE", 
  USUBJID = paste(STUDYID, SUBJECT,  sep = "-"), 
  #AESEQ,
  AETERM  = AETERM,
  # MedDRA terms
  AELLT = AETERM_LLT, 
  AELLTCD,
  AEDECOD, 
  AEPTCD,
  AEPTCD, 
  AEHLT, 
  AEHLTCD,
  AEHLGT, 
  AEHLGTCD, 
  AEBODSYS  = AETERM_SOC,
  AEBDSYCD = AETERM_SOC_CODE,
  AESOC = AEBODSYS,
  AESOCCD = AEBDSYCD, 
  AESER = AESER,
  AEACN, 
  AEREL, 
  AESEV = str_extract(AETOXGR, "Mild|Moderate|Severe"), 
  AEOUT, 
  AESDTH, 
  AESLIFE, 
  AETOXGR = str_extract(AETOXGR, "Grade 1|Grade 2|Grade 3|Grade 4"),
  # AESTDTC, 
  # AEENDTC, 
  # AESTDY, 
  # AEENDY
)


