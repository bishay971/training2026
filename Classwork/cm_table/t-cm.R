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

# data and package


# install.packages(c("Tplyr", "r2rtf"))
# library(dplyr)


library(tidyverse)
library(Tplyr)
library(r2rtf)
rsetup::get_data("adcm_table")

# fixed in adam programs
adcm <- adcm |> 
  mutate(
    CMDECOD = CMTRT,
    TRTP = ARMCD, 
    SAFFL = "Y"
  )

bigN <- adsl |> 
  count(TRTP) |> 
  pivot_wider(
    names_from = TRTP, 
    values_from = n
  )
# bigN$TRA25
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



adcm_table <- tplyr_table(target = adcm, treat_var = TRTP, 
                          where = SAFFL == "Y") |> 
  add_layer(
    group_count(
      vars(ATC, CMDECOD), 
      # set_order_count_method("bycount") |> 
      #   set_ordering_cols(var1_PLAC, var1_PLAC) |> 
      #   set_result_order_var(n)
    )
  ) |> 
  build() |> 
  select(2:4)

# new_name = old_name  

final_tbl <- bind_rows(
  anycm, adcm_table
)


colheader <- paste0("ATC Class \n Preferred Term", "|",  
                    "Treatment A \n (N=", bigN$TRA25, ")", "|",  
                    "Placebo \n (N=", bigN$Placebo, ")"
)

align <- c("l", "c", "c")

final_tbl |> 
  rtf_title("Table 2.1 Summary of Concomitant Medication \n Safety Population") |> 
  rtf_colheader(colheader, 
                text_justification = align) |> 
  rtf_body(text_justification = align) |> 
  rtf_footnote("Note: WHODRUG Global X.XX") |> 
  rtf_encode() |> 
  write_rtf("t-cm.rtf")




























