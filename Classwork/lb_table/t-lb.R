# *******************************************************************************************************
# Program Name:                 t-lb.R
# Project:                      TRA-025
# Purpose:                      Create Table of LB
# Original Author:              Binod Jung Bogati (linkedin.com/in/bjungbogati)
# Copyright:                    © 2025. Unauthorized distribution or reuse prohibited.
# Date Created:                 8/12/2025
# Parameters:                   NA
#
# Input:                        
# Output:                       
# Modifications:
#   Date        By             Changes
# **********  *************  *************************************
#   
# ********************************************************************************************************/


library(tidyverse)
library(r2rtf)
library(Tplyr)

rsetup::get_data("adlb_table") 


grade_fmt <- c("Grade 0", "Grade 1", "Grade 2", "Grade 3", "Grade 4")


adlb_tbl <- adlb |>
  mutate(
    BTOXGRH = factor(BTOXGRH, levels = grade_fmt), 
    ATOXGRH = factor(ATOXGRH, levels = grade_fmt), 
  )

adlb_shift <- tplyr_table(
  adlb_tbl,  
  TRTP,
  where = SAFFL == "Y" &
    WTOXGRH == "Y" & TRTP == "TRA25" &
    PARCAT1 == "CHEMISTRY" & PARAMCD == "SODIUM"
) |> 
  add_layer(
    group_shift(
      vars(
        row = BTOXGRH, 
        column = ATOXGRH
      ), 
      by = vars(PARAM, ATOXDSCH, PARCAT1)
    )
  ) |> 
  build() |> 
  select(
    -starts_with("var1_Placebo"), 
    -starts_with("ord_layer")
  ) |> 
  rename_with(
    ~ str_remove_all(., "var1_TRA25_")
  ) |> 
  mutate(
    across(
      starts_with("Grade"), as.numeric
    )
  ) |> 
  mutate(
    Total = rowSums(across(
      starts_with("Grade")
    ))
  )

total_row <- adlb_shift |> 
  summarise(
    across(where(is.numeric), sum)
  ) |> 
  mutate(row_label4 = "Total")


shift_tbl <- bind_rows(
  adlb_shift, 
  total_row
)

bigN <- adsl |> 
  count(TRTP) |> 
  pivot_wider(
    names_from = TRTP, 
    values_from = n
  )

final_table <- shift_tbl |> 
  mutate(
    `Grade 0` = paste0(
      `Grade 0`, " (", round(`Grade 0`/bigN$TRA25*100, digits = 1), "%)"
    )
  ) |> 
  select( -row_label2, -row_label3)


colheader <- paste(
  "Lab Parameter", 
  "Treatment", 
  "Baseline Grade",
  "Grade 0",
  "Grade 1",
  "Grade 2",
  "Grade 3",
  "Grade 4", 
  sep = "|"
)


# function(data, inputs) {
#   final_table |> 
#     rtf_title("Title") |> 
#     rtf_colheader(colheader) |> 
#     rtf_body() |> 
#     rtf_encode() |> 
#     write_rtf("t-lb.rtf")
#   
# }

final_table |> 
  rtf_title("Title") |> 
  rtf_colheader(colheader) |> 
  rtf_body() |> 
  rtf_encode() |> 
  write_rtf("t-lb.rtf")




