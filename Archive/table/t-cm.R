# *******************************************************************************************************
# Program Name:                 t-cm.R
# Project:                      TRA-025
# Purpose:                      Create Table of CM
# Original Author:              Binod Jung Bogati (linkedin.com/in/bjungbogati)
# Copyright:                    © 2025. Unauthorized distribution or reuse prohibited.
# Date Created:                 8/11/2025
# Parameters:                   NA
#
# Input:                        
# Output:                       
# Modifications:
#   Date        By             Changes
# **********  *************  *************************************
#   
# ********************************************************************************************************/

# load(url("https://tinyurl.com/bp66vz5a"))

library(tidyverse)
library(Tplyr)
library(r2rtf)

rsetup::get_data("adcm_table")

adcm <- adcm |> 
  mutate(
    CMDECOD = CMTRT, 
    TRTP = ARMCD
  )

# adsl$trtp

bigN <- adsl |> 
  count(TRTP) |> 
  pivot_wider(
    names_from = TRTP,
    values_from = n
  )

anycm <- adcm |> 
  distinct(USUBJID, TRTP, .keep_all = TRUE) |> 
  count(TRTP) |> 
  pivot_wider(
    names_from = TRTP,
    values_from = n
  ) |> 
  mutate(
    ATCPT = "Any concomitant medication",
    TRA25 = paste0(
      TRA25, " (", round(TRA25/bigN$TRA25*100, digits = 1), "%)"
    ),
    PLAC = paste0(
      PLAC, " (", round(PLAC/bigN$Placebo*100, digits = 1), "%)"
    )
  ) |> 
  select(row_label2=ATCPT, var1_TRA25=TRA25, var1_PLAC=PLAC)

# new_name = old_name

# bigN$Placebo

adcm_table <- adcm |> 
  tplyr_table(TRTP) |> 
  add_layer(
    group_count(
      vars(ATC, CMDECOD)
    )
  ) |> 
  build() |>
  select(2:4)

final_tbl <- bind_rows(
  anycm, adcm_table
)

# 
rtitle <- "Table 2.1 Summary of Concomitant Medications \n Safety Population"

col_header <- paste0("ATC Class \n Preferred Term", "|",
                     "Treatment A \n (N=", bigN$TRA25, ")", "|", 
                     "Placebo \n (N=", bigN$Placebo, ")")

col_align <- c("l", "c", "c")

final_tbl |> 
  rtf_title(rtitle) |> 
  rtf_colheader(col_header, 
                text_justification = col_align) |> 
  rtf_body( text_justification = col_align) |> 
  rtf_footnote("Note: WHODRUG Global X.XX") |> 
  rtf_encode() |> 
  write_rtf("t-cm.rtf")
