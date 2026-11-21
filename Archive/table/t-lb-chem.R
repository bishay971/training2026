# *******************************************************************************************************
# Program Name:                 t-lb_chem.R
# Project:                      TRA-025
# Purpose:                      Lab NCI CTCAE Grade Shift Table - Chemistry (multi-page RTF)
# Original Author:              Binod Jung Bogati (linkedin.com/in/bjungbogati)
# Copyright:                    © 2025. Unauthorized distribution or reuse prohibited.
# Date Created:                 8/12/2025
# Parameters:                   grade_dir = "high" | "low" ; parcat1 = "CHEMISTRY"
#
# Input:                        adlb, adsl (via get_data("adlb_table"))
# Output:                       t-lb-chem-high.rtf
#                               t-lb-chem-low.rtf
# Modifications:
#   Date        By             Changes
# **********  *************  *************************************
#   2026-05-29  BJB            Single-build; Tplyr shift; apply_formats; subline/group_by.
#   2026-05-29  BJB            eCTD compliance: 0.75" margins, TNR 9pt body / 10pt footnote;
#                              SPLIT_CHAR header alignment; arm spacer rows; PARAM prefix;
#                              space-only padding (no tabs — avoids strwidth 0x09 warning).
#
# ********************************************************************************************************/


library(tidyverse)
library(Tplyr)
library(r2rtf)
library(here)

get_data("adlb_table")


# ── Page constants ─────────────────────────────────────────────────────────────
# eCTD: landscape allowed for wide tables; minimum 0.75 inch on ALL sides.

PAGE_ORIENT <- "landscape"
PAGE_MARGIN <- c(0.75, 0.75, 0.75, 0.75, 0.5, 0.5)   # top, bottom, left, right, header, footer
PAGE_NROW   <- 29
TEXT_WIDTH  <- (if (PAGE_ORIENT == "landscape") 11 else 8.5) - PAGE_MARGIN[3] - PAGE_MARGIN[4]
TAB_TWIPS   <- round(TEXT_WIDTH * 1440)   # right-aligned tab stop position in twips


# ── Submission font standard ───────────────────────────────────────────────────
# eCTD: Times New Roman recommended. r2rtf font table: 1=Times New Roman, 2=Arial, 3=Courier New.
# Body/headers 9pt (eCTD minimum); footnotes 10pt (eCTD minimum).

FONT_FAMILY   <- 1     # Times New Roman
FONT_SIZE     <- 9     # body, column headers, page header/footer
FOOTNOTE_SIZE <- 10    # eCTD minimum for footnotes



# ── Table constants ────────────────────────────────────────────────────────────

GRADE_LEVELS <- c(paste("Grade", 0:4), "Missing")
GRADE_SHORT  <- c("G0", "G1", "G2", "G3", "G4", "Missing")
PCT_FMT      <- "xxx (xx.x%)"


# ── Submission template ────────────────────────────────────────────────────────

PROTOCOL <- "TRAINING 26"
DRUG_IND <- "TRA25 / COMPOUND-ASSOCIATED STUDY"
TLF_VER  <- "Final Database Lock"
DB_VER   <- format(Sys.Date(), "%d%b%Y")
TASK_LBL <- "Primary Analysis"

rfooter <- c(
  "(1) Percentages are calculated using the arm safety population N as the denominator.",
  "(2) For each cell, n represents participants with the given baseline-to-worst post-baseline grade combination; worst value on study is the worst grade observed post-baseline for a given participant.",
  "Grade 0 = Below Grade 1 and any grade in the other direction.",
  "Dataset: ADLB"
)

rtime <- format(Sys.time(), "%d%b%y (%H:%M)")

rpageheader <- c(
  paste0("\\tqr\\tx", TAB_TWIPS, " PROTOCOL: ", PROTOCOL,  "\\tab TLF Version: ",      TLF_VER),
  paste0("\\tqr\\tx", TAB_TWIPS, " DRUG/INDICATION: ",     DRUG_IND, "\\tab DATABASE VERSION: ", DB_VER),
  paste0("\\tqr\\tx", TAB_TWIPS, " TASK: ", TASK_LBL,     "\\tab Page \\pagenumber of \\pagefield")
)

rpgm    <- "t-lb-chem"
rpagefooter <- c(
  rfooter,
  paste0("\\tqr\\tx", TAB_TWIPS, " Program: ", rpgm, "\\tab DATE(TIME): ", rtime)
)


# ── 1. Prep data ───────────────────────────────────────────────────────────────

prep_lb_data <- function(data_adlb, data_adsl,
                         grade_dir  = c("high", "low"),
                         treatments = c("TRA25", "Placebo"),
                         parcat1    = "CHEMISTRY") {
  
  grade_dir <- match.arg(grade_dir)
  sfx       <- if (grade_dir == "high") "H" else "L"
  btox_col  <- paste0("BTOXGR", sfx)
  atox_col  <- paste0("ATOXGR", sfx)
  wtox_col  <- paste0("WTOXGR", sfx)
  
  adlb_tbl <- data_adlb |>
    filter(TRTP %in% treatments, PARCAT1 == parcat1) |>
    mutate(across(c(!!sym(btox_col), !!sym(atox_col)),
                  ~ factor(if_else(is.na(.x), "Missing", as.character(.x)),
                           levels = GRADE_LEVELS)))
  
  arm_n <- data_adsl |>
    filter(TRTP %in% treatments) |>
    count(TRTP) |>
    deframe()
  
  list(adlb_tbl = adlb_tbl, arm_n = arm_n,
       btox_col = btox_col, atox_col = atox_col, wtox_col = wtox_col,
       treatments = treatments, grade_dir = grade_dir, parcat1 = parcat1)
}


# ── 2. Raw shift counts ────────────────────────────────────────────────────────

build_shift_raw <- function(prep) {
  prep$adlb_tbl |>
    tplyr_table(TRTP, where = SAFFL == "Y" & .data[[prep$wtox_col]] == "Y") |>
    add_layer(
      group_shift(
        vars(row = !!sym(prep$btox_col), column = !!sym(prep$atox_col)),
        by = vars(PARAM, ATOXDSCH)
      ) |>
        set_format_strings(f_str("xxx", n))
    ) |>
    build() |>
    select(-starts_with("ord_layer"), -row_label2) |>   # drop ord cols + ATOXDSCH
    rename(PARAM = row_label1, baseline = row_label3) |>
    rename_with(
      ~ .x |> str_remove("^var1_") |> str_replace_all("Grade ", "G"),
      starts_with("var1_")
    ) |>
    filter(baseline %in% GRADE_LEVELS) |>
    mutate(across(matches("_(G[0-4]|Missing)$"), as.numeric)) |>
    filter(any(rowSums(across(matches("_(G[0-4]|Missing)$")), na.rm = TRUE) > 0), .by = PARAM)
}


# ── 3. Long shape + marginal totals ───────────────────────────────────────────

shape_long <- function(df, treatments) {
  count_cols <- intersect(names(df), unlist(lapply(treatments, \(t) paste0(t, "_", GRADE_SHORT))))
  
  long <- df |>
    summarise(across(all_of(count_cols), ~ sum(.x, na.rm = TRUE)), .by = c(PARAM, baseline)) |>
    pivot_longer(all_of(count_cols), names_to = c("arm", "anl"),
                 names_pattern = "(.+)_(G[0-4]|Missing)", values_to = "n")
  
  bind_rows(
    long,
    long |> summarise(n = sum(n), .by = c(PARAM, arm, baseline)) |> mutate(anl = "Total"),
    long |> summarise(n = sum(n), .by = c(PARAM, arm, anl))      |> mutate(baseline = "Total"),
    long |> summarise(n = sum(n), .by = c(PARAM, arm))           |> mutate(baseline = "Total", anl = "Total")
  )
}


# ── 4. Format cells ────────────────────────────────────────────────────────────

format_cells <- function(long, arm_n, treatments) {
  long |>
    mutate(
      N    = arm_n[arm],
      pct  = if_else(N > 0, n / N * 100, 0),
      cell = case_when(
        anl == "Total" | baseline == "Total" ~ as.character(n),
        n == 0                               ~ "0",
        TRUE                                 ~ apply_formats(PCT_FMT, n, pct)
      )
    ) |>
    select(PARAM, arm, baseline, anl, cell) |>
    pivot_wider(names_from = anl, values_from = cell) |>
    mutate(
      treatment = paste0(arm, " (N=", arm_n[arm], ")"),
      .arm      = factor(arm, levels = treatments),
      .base     = if_else(baseline == "Total",
                          length(GRADE_LEVELS) + 1L, match(baseline, GRADE_LEVELS))
    ) |>
    arrange(PARAM, .arm, .base) |>
    select(PARAM, treatment, baseline, all_of(GRADE_SHORT), Total)
}


# ── 5. Blank row after each treatment arm (visual separator between arm blocks) ─
# group_modify receives data WITHOUT the PARAM grouping column; PARAM is in `key`
# and is restored by group_modify after .f returns.

add_arm_spacers <- function(df) {
  df |>
    group_by(PARAM) |>
    group_modify(function(data, key) {
      arms <- unique(data$treatment)
      lapply(seq_along(arms), function(i) {
        block <- filter(data, treatment == arms[i])
        if (i < length(arms)) {
          blank <- block[1L, ]
          blank[, setdiff(names(blank), "treatment")] <- ""
          bind_rows(block, blank)
        } else {
          block
        }
      }) |> bind_rows()
    }) |>
    ungroup()
}


# ── 6. RTF render ──────────────────────────────────────────────────────────────

render_rtf <- function(df, prep, out_file) {
  dir_lbl <- if (prep$grade_dir == "high") "Increased" else "Decreased"
  n_g     <- length(GRADE_SHORT) + 1L   # G0-G4 + Missing + Total = 7
  
  # Prefix PARAM with indented label; add blank row separators between arm blocks.
  df <- df |>
    mutate(PARAM = paste0(PARAM, " - ", dir_lbl, " Direction")) |>
    add_arm_spacers()
  
  # Column widths derived from TEXT_WIDTH (auto from PAGE_ORIENT + PAGE_MARGIN)
  trt_w  <- TEXT_WIDTH * 0.18
  base_w <- TEXT_WIDTH * 0.14
  g_w    <- (TEXT_WIDTH - trt_w - base_w) / n_g
  
  vis_w     <- c(trt_w, base_w, rep(g_w, n_g))   # 8 cols — colheaders use this
  body_w    <- c(0.001, vis_w)                    # 9 cols — PARAM hidden by subline_by
  body_just <- c("l", "l", "l", rep("c", n_g))
  
  span_w   <- c(trt_w, base_w, g_w * n_g)         # 3-cell spanning row (cell 3 = colspan 6)
  span_hdr <- "Treatment | Baseline Grade | Maximum Post-Baseline Grade"
  sub_hdr  <- paste(c(" ", " ", c(GRADE_LEVELS, "Total")), collapse = " | ")
  
  df |>
    rtf_page(
      orientation  = PAGE_ORIENT,
      margin       = PAGE_MARGIN,
      nrow         = PAGE_NROW,
      border_first = "single",
      border_last  = "single"
    ) |>
    rtf_page_header(
      text               = rpageheader,
      text_justification = "l",
      text_font          = FONT_FAMILY,
      text_font_size     = FONT_SIZE
    ) |>
    rtf_title(
      paste0(
        "Table 14.3.4.7\n",
        "Laboratory NCI CTCAE Grade Shift from Baseline to Worst Post-Baseline ",
        dir_lbl, " (", str_to_title(prep$parcat1), ")\n",
        "Safety Population"
      ),
      text_format    = "b",
      text_font      = FONT_FAMILY,
      text_font_size = FONT_SIZE
    ) |>
    rtf_colheader(
      span_hdr,
      col_rel_width  = span_w,
      text_font      = FONT_FAMILY,
      text_font_size = FONT_SIZE,
      border_top     = "single",
      border_bottom  = c("", "", "single"),   # underline only under spanning cell
      border_left    = "",
      border_right   = ""
    ) |>
    rtf_colheader(
      sub_hdr,
      col_rel_width  = vis_w,
      text_font      = FONT_FAMILY,
      text_font_size = FONT_SIZE,
      border_top     = "",
      border_bottom  = "single",
      border_left    = "",
      border_right   = ""
    ) |>
    rtf_body(
      col_rel_width      = body_w,
      text_justification = body_just,
      text_font          = FONT_FAMILY,
      text_font_size     = FONT_SIZE,
      border_top         = NULL,
      border_left        = NULL,
      border_bottom      = NULL,
      border_right       = NULL,
      subline_by         = "PARAM",
      group_by           = "treatment"
    ) |>
    rtf_page_footer(
      text               = rpagefooter,
      text_justification = "l",
      text_font          = FONT_FAMILY,
      text_font_size     = FOOTNOTE_SIZE
    ) |>
    rtf_encode() |>
    write_rtf(out_file)
  
  invisible(out_file)
}


# ── 7. Main ────────────────────────────────────────────────────────────────────

build_lb_grade_table <- function(grade_dir  = c("high", "low"),
                                 treatments = c("TRA25", "Placebo"),
                                 parcat1    = "CHEMISTRY",
                                 out_file   = NULL) {
  
  grade_dir <- match.arg(grade_dir)
  get_data("adlb_table")
  
  prep <- prep_lb_data(adlb, adsl,
                       grade_dir  = grade_dir,
                       treatments = treatments,
                       parcat1    = parcat1)
  
  if (is.null(out_file)) out_file <- sprintf("t-lb-chem-%s.rtf", grade_dir)
  
  build_shift_raw(prep) |>
    shape_long(prep$treatments) |>
    format_cells(prep$arm_n, prep$treatments) |>
    render_rtf(prep, out_file)
}


# ── Run ────────────────────────────────────────────────────────────────────────

build_lb_grade_table(grade_dir = "high")
build_lb_grade_table(grade_dir = "low")
