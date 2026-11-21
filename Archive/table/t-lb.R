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


# load(url("https://tinyurl.com/4refv4m2"))

library(tidyverse)
library(Tplyr)
library(r2rtf)

get_data("adlb_table") 


grade_fmt <- c("Grade 0", "Grade 1", "Grade 2", "Grade 3", "Grade 4")

adlb_tbl <- adlb |> 
  mutate(
    BTOXGRH = factor(BTOXGRH, levels = grade_fmt), 
    ATOXGRH = factor(ATOXGRH, levels = grade_fmt)
  )

adlb_shift <- adlb_tbl |> 
  tplyr_table(
    TRTP,
    where = SAFFL == "Y" & WTOXGRH == "Y" & TRTP == "TRA25" &
      PARAMCD == "SODIUM"
  ) |> 
  add_layer(
    group_shift(
      vars(
        row = BTOXGRH, 
        column = ATOXGRH
      ),
      by = vars(PARAM, ATOXDSCH, PARCAT1)
    ) #|> 
    #  set_format_strings(f_str("xxx (xx.xx%)", n, pct))
    
    
  ) |> 
  build() |> 
  select(
    -starts_with("var1_Placebo"), 
    -starts_with("ord_layer")
  ) |> 
  rename_with(~ str_remove_all(., "var1_TRA25_")) |> 
  mutate(
    across(
      starts_with("Grade"), as.numeric
    )
  ) |> 
  mutate(
    Total = rowSums(across(starts_with("Grade")))
  )

total_row <- adlb_shift |> 
  summarise(
    across(
      where(is.numeric), sum
    )
  ) |> 
  mutate(
    across(
      starts_with(c("Grade")), as.character
    ), 
    row_label4 = "Total"
  )

bigN <- adsl |>
  count(TRTP) |>
  pivot_wider(
    names_from = TRTP,
    values_from = n
  )

shift_tbl <- adlb_shift |> 
  mutate(
    `Grade 0` = paste0( `Grade 0` , " (", round(`Grade 0`/bigN$TRA25 * 100, digits = 1), ")"),
    `Grade 1` = paste0( `Grade 1` , " (", round(`Grade 1`/bigN$TRA25 * 100, digits = 1), ")"),
    `Grade 2` = paste0( `Grade 2` , " (", round(`Grade 2`/bigN$TRA25 * 100, digits = 1), ")"),
    `Grade 3` = paste0( `Grade 3` , " (", round(`Grade 3`/bigN$TRA25 * 100, digits = 1), ")"),
    `Grade 4` = paste0( `Grade 4` , " (", round(`Grade 4`/bigN$TRA25 * 100, digits = 1), ")")
  ) |> 
  mutate(
    across(
      starts_with("Grade"), ~ if_else(. == "0 (0)", "0", .)
    )
  ) |> 
  select(-row_label3)

final_tbl <- bind_rows(
  shift_tbl, 
  total_row
)

col_header <- paste0(
  "Lab parameter", "|",
  "Treatment", "|",
  "Baseline Grade", "|",
  "Grade 0", "|",
  "Grade 1", "|",
  "Grade 2", "|",
  "Grade 3", "|",
  "Grade 4", "|",
  "Total", "|"
)

final_tbl |> 
  rtf_title("Table 14.3.4.7") |> 
  rtf_colheader(colheader =  col_header) |> 
  rtf_body(
    text_justification = "l"
  ) |> 
  rtf_encode() |> 
  write_rtf("t-lb.rtf")

# 
# rtf_footnote()
# 
# rtf_page_footer()
# 
# 






