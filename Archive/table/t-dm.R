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


# load(url("https://tinyurl.com/2tev5w6n"))


library(tidyverse)
library(Tplyr)
library(r2rtf)

rsetup::get_data("adsl_table")

bigN <- adsl |>
  count(TRTP) |>
  pivot_wider(
    names_from = TRTP,
    values_from = n
  ) |>
  mutate(Overall = TRA25 + Placebo)


sex_fmt <- c(
  "M" = "Male",
  "F" = "Female"
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
    ETHNIC = factor(
      ETHNIC,
      levels = names(ethnic_fmt),
      labels = unname(ethnic_fmt)
    )
  )

dm_table <- adsl_tbl |>
  tplyr_table(
    TRTP,
    where = SAFFL == "Y"
  ) |>
  set_desc_layer_formats(
    "n" = f_str("xx", n),
    "Mean" = f_str("xx.xx (xx.xxx)", mean, sd),
    "Median" = f_str("xx.xx", median),
    "Min - Max" = f_str("xx.x - xx.x", min, max),
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
  apply_row_masks(row_breaks = T) |>
  mutate(
    row_label3 = case_when(
      row_label2 == desc_fmt[1] ~ desc_fmt[1],
      row_label2 == desc_fmt[2] ~ desc_fmt[2],
      row_label2 == desc_fmt[3] ~ desc_fmt[3],
      row_label2 == desc_fmt[4] ~ desc_fmt[4],
      row_label2 != "" ~ "n (%)",
      TRUE ~ ""
    )
  ) |>
  select(
    starts_with("row_"), "var1_TRA25", "var1_Placebo", "var1_Total"
  )


# table creation

rtitle <- "Table x.xx \n Summary of Demographics \n Safety Population"

col_header <- paste0(
  "Variables", "|",
  "Category", "|",
  "Statistics", "|",
  "Treatment A", "\n (N=", bigN$TRA25, ")", "|",
  "Placebo", "\n (N=", bigN$Placebo, ")", "|",
  "Overall", "\n (N=", bigN$Overall, ")", "|"
)

rfooter <- "Note: Percentage calculated based on population count."

rtime <- format(Sys.time(), "%d%b%Y %H:%M")
rtab <- function(x) paste0(rep("\t", x), collapse = "")

rpageheader <- paste0(
  "Sponsor XX",
  rtab(8),
  "Page \\pagenumber of \\pagefield",
  "\n Protocol: TRAINING 26",
  rtab(12),
  rtime
)


col_rel_width <- c(1, 2, 1, 1, 1, 1)

rpath <- here::here() |> str_replace_all("_", "-")

rpagefooter <- paste0(
  "Dataset: ADSL", rtab(1),
  "Program Name:", rpath, "/t-dm.R"
)


table_function <- function(data, rtitle, rpath, rfooter, table_name) {
  
  data |> 
    rtf_page() |> 
    rtf_page_header() |> 
    rtf_encode() |> 
    write_rtf(table_name)
  
}

dm_table |>
  rtf_page(
    orientation = "landscape",
    margin = c(0.3, 0.3, 0.5, 0.3, 0.5, 0.5),
    nrow = 29,
    border_first = "single",
    border_last = "single"
  ) |>
  rtf_page_header(
    text = rpageheader,
    text_justification = "c",
    text_font = 9,
    text_font_size = 10
  ) |>
  rtf_title(rtitle) |>
  rtf_colheader(col_header,
                col_rel_width = col_rel_width
  ) |>
  rtf_body(
    col_rel_width = col_rel_width, 
    border_top = NULL, 
    border_left = NULL, 
    border_bottom = NULL,
    border_right = NULL,
    text_font = 9,
    text_font_size = 10, 
    text_justification = "l"
  ) |>
  rtf_footnote(rfooter) |>
  rtf_page_footer(
    text = rpagefooter,
    text_font = 7,
    text_font_size = 12
  ) |>
  rtf_encode() |>
  write_rtf("t-dm.rtf")
