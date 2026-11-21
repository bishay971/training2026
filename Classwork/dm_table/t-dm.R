# *******************************************************************************************************
# Program Name:                 t-dm.R
# Project:                      TRA-025
# Purpose:                      Create Table of DM
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
library(Tplyr)
library(r2rtf)

rsetup::get_data("adsl_table")


sex_fmt <- c(
  "M" = "Male",
  "F" = "Female"
)

race_fmt <- c(
  "AMERICAN INDIAN OR ALASKA NATIVE" = "American Indian / Alaska Native",
  "ASIAN" = "Asian",
  "BLACK OR AFRICAN AMERICAN" = "Black or African American",
  "NATIVE HAWAIIAN/PACIFIC ISLANDER" = "Native Hawaiian / Pacific Islander",
  "WHITE" = "White",
  "NOT REPORTED" = "Other"
)

ethnic_fmt <- c(
  "HISPANIC OR LATINO" = "Hispanic or Latino",
  "NOT HISPANIC OR LATINO" = "Not Hispanic or Latino",
  "UNKNOWN" = "Unknown"
)

desc_fmt <- c("n", "Mean (SD)", "Median", "Min - Max")

adsl_tbl <- adsl |>
  mutate(
    SEX = factor(
      SEX,
      levels = names(sex_fmt),
      labels = unname(sex_fmt)
    ),
    RACE = factor(
      RACE,
      levels = names(race_fmt),
      labels = unname(race_fmt)
    ),
    ETHNIC = factor(
      ETHNIC,
      levels = names(ethnic_fmt),
      labels = unname(ethnic_fmt)
    )
  )


bigN <- adsl |>
  count(TRTP) |>
  pivot_wider(
    names_from = TRTP,
    values_from = n
  ) |>
  mutate(
    Overall = Placebo + TRA25
  )

# adsl |>
#   group_by(TRTA) |>
#   summarise(
#     n= n(),
#     mean = mean(AGE),
#     sd = sd(AGE),
#     median = median(AGE),
#     min = min(AGE),
#     max = max(AGE)
#   )

dm_table <- adsl_tbl |>
  tplyr_table(
    treat_var = TRTP,
    where = SAFFL == "Y"
  ) |>
  set_desc_layer_formats(
    "n" = f_str("xx", n),
    "Mean (SD)" = f_str("xx.xx (xx.xxx)", mean, sd),
    "Median" = f_str("xx.xx", median),
    "Min - Max" = f_str("xx.x - xx.x", min, max)
  ) |>
  add_total_group() |>
  add_layer(
    group_desc(
      AGE,
      by = "Age (years)"
    )
  ) |>
  add_layer(
    group_count(
      SEX,
      by = "Sex"
    )
  ) |>
  add_layer(
    group_count(
      RACE,
      by = "Race"
    )
  ) |>
  add_layer(
    group_count(
      ETHNIC,
      by = "Ethnicity"
    )
  ) |>
  build() |>
  apply_row_masks(row_breaks = TRUE) |>
  mutate(
    row_label3 = case_when(
      row_label2 == desc_fmt[1] ~ desc_fmt[1],
      row_label2 == desc_fmt[2] ~ desc_fmt[2],
      row_label2 == desc_fmt[3] ~ desc_fmt[3],
      row_label2 == desc_fmt[4] ~ desc_fmt[4],
      row_label2 != "" ~ "n (%)",
      TRUE ~ ""
    ),
    row_label2 = case_when(
      row_label2 == desc_fmt[1] ~ "",
      row_label2 == desc_fmt[2] ~ "",
      row_label2 == desc_fmt[3] ~ "",
      row_label2 == desc_fmt[4] ~ "",
      TRUE ~ row_label2
    )
  ) |>
  select(
    starts_with("row_"), "var1_TRA25", "var1_Placebo", "var1_Total"
  ) |>
  select(1:6) |>
  mutate(
    across(
      starts_with("var1_"),
      ~ str_replace(., "0\\s*\\(\\s*0\\.0%\\s*\\)", " 0")
    )
  )

# dm_table[dm_table == "0 ( 0.0%)"] <- " 0"





col_header <- paste0(
  "Variables", "|",
  "Category", "|",
  "Statistics", "|",
  "Treatment A", "\n (N=", bigN$TRA25, ")", "|",
  "Placebo", "\n (N=", bigN$Placebo, ")", "|",
  "Overall", "\n (N=", bigN$Overall, ")"
)

col_rel_width <- c(1, 2, 1, 1, 1, 1)

rtime <- format(Sys.time(), "%d%b%Y %H:%M")

rtab <- function(x) paste0(rep("\t", x), collapse = "")

rpageheader <- paste0(
  "Sponsor XXX", rtab(8),
  "Page \\pagenumber of \\pagefield", 
  "\n Protocol XXX", 
  rtab(12), rtime
)



dm_table |>
  rtf_page(
    orientation = "landscape", 
    margin = c(0.3, 0.3, 0.5, 0.3, 0.5, 0.5),
    border_first = "single", 
    border_last = "single"
  ) |>
  
  rtf_page_header(
    text = rpageheader
  ) |> 
  rtf_title("Table x.xx \n Summary of Demographics \n Saftey Population") |>
  rtf_colheader(col_header,
                text_justification = "l",
                col_rel_width = col_rel_width,
                border_left = NULL,
                border_right = NULL,
                border_top = NULL,
                border_bottom = NULL
  ) |>
  rtf_body(
    text_justification = "l",
    col_rel_width = col_rel_width,
    border_left = NULL,
    border_right = NULL,
    border_top = NULL,
    border_bottom = NULL,
    border_first = NULL,
    border_last = NULL,
  ) |>
  rtf_footnote("Note: Percentage is calculated based on population count and treatment groups") |>
  rtf_encode() |>
  write_rtf("t-dm.rtf")
