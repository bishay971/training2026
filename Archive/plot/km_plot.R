# ══════════════════════════════════════════════════════════════════════════════
# Exercise 02: Kaplan-Meier Plot with tern
#
# Endpoint: TTDE — Time to First Dermatologic Event (safety endpoint)
# Arms: TRTA = Xanomeline Low Dose  |  TRTB = Xanomeline High Dose
#
# Placebo is excluded here: g_km() with annot_coxph = TRUE requires exactly
# 2 arm levels. This plot compares two active doses (secondary analysis).
# For a Phase III primary analysis, you would run one KM per active arm vs Placebo.
#
# Annotation boxes use normalized [0,1] coordinates — NOT data values.
# x/y = center of the box; x=0 is left edge, x=1 is right edge of the canvas.
# KM curves occupy y ≈ 0.28–1.0; the at-risk table occupies y ≈ 0.0–0.28.
#
# This exercise builds the plot in 3 steps so you can see each layer added:
#   Step 3: curves only (baseline check)
#   Step 4: + at-risk table, CI ribbon, HR box, median box
#   Step 5: + clean legend → final plot ready to annotate and save
# ══════════════════════════════════════════════════════════════════════════════


# ── Step 0: Load packages ──────────────────────────────────────────────────────

library(tidyverse)
library(survival)   # survival models: Surv(), coxph()
library(tern)       # g_km(), control_coxph_annot(), control_surv_med_annot()
library(patchwork)  # plot_annotation() adds title/subtitle/caption
library(clinicalfd)   # CDISC Pilot 01 datasets: adtte, adlbc, adsl


# ── Step 1: Parameters ────────────────────────────────────────────────────────

# ARM_LEVELS: the two arm labels and their ORDER.
# Order matters: first level = reference in Cox PH; sets legend and table column order.
# Used in: factor(ARM, levels = ARM_LEVELS) in Step 2.
ARM_LEVELS <- c("TRTA", "TRTB")

# ARM_COLORS: one color per arm, matched by POSITION to ARM_LEVELS.
# TRTA (position 1) gets first color; TRTB (position 2) gets second color.
# Used in: g_km(col = ARM_COLORS) — controls KM curve, CI ribbon, and censoring mark color.
ARM_COLORS <- c("blue", "#F44336")  # fill in: blue hex for TRTA, red already given for TRTB

# ARM_LTYPE: line type per arm — helps distinguish arms in black-and-white prints.
# Options: "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"
# Used in: g_km(lty = ARM_LTYPE).
ARM_LTYPE <- c("dashed", "dotdash")

# MAX_TIME: x-axis upper limit in days. Cuts the sparse tail where few subjects remain.
# Used in: g_km(max_time = MAX_TIME) and to build X_TICKS below.
MAX_TIME <- 200  # try 200

# X_TICKS: positions of tick marks on the time axis.
# seq(0, 200, by = 50) produces: 0, 50, 100, 150, 200.
# Used in: g_km(xticks = X_TICKS).
X_TICKS <- seq(0, MAX_TIME, by = 50)  # try every 50 days


# ── Step 2: Load and prepare data ─────────────────────────────────────────────
# Strip the `labelled` class from trta before filtering — avoids type mismatch.
# tern requires base data.frame, not tibble.

data("adtte")

df <- adtte |>
  mutate(trta = as.character(trta)) |>  # convert labelled → plain character
  filter(trta != "Placebo") |>       # remove Placebo — this plot compares two active doses only
  mutate(
    # CDISC: cnsr = 0 means the event happened; cnsr = 1 means censored
    is_event = if_else(cnsr == 0L, TRUE, FALSE),
    
    # Recode full arm names to short labels defined in ARM_LEVELS
    # case_when() checks each condition top-to-bottom; first match wins
    ARM = factor(
      dplyr::case_when(
        trta == "Xanomeline Low Dose"  ~ "TRTA",  # short label for low dose  (must match ARM_LEVELS)
        trta == "Xanomeline High Dose" ~ "TRTB",  # short label for high dose (must match ARM_LEVELS)
        TRUE ~ NA_character_
      ),
      levels = ARM_LEVELS  # sets factor order → controls legend and table column order
    )
  ) |>
  filter(!is.na(aval), !is.na(ARM)) |>  # drop rows with missing time or missing arm label
  as.data.frame()                           # tibble → base data.frame (tern requirement)

cat(sprintf("Subjects: %d  |  Events: %d\n", nrow(df), sum(df$is_event)))
print(table(df$ARM, df$is_event, dnn = c("ARM", "event")))


# ── Step 3: Core g_km() — curves only ────────────────────────────────────────
# Build the KM curves first (no annotations) to verify the data and curves look right.
# Add annotations in Step 4 once the curves are confirmed.

p_km <- g_km(
  df        = df,     # base data.frame prepared in Step 2
  variables = list(
    tte      = "aval",  # column containing time values (numeric, in days)
    is_event = "is_event",  # logical column: TRUE = event occurred, FALSE = censored
    arm      = "ARM"   # factor column with ARM_LEVELS as levels
  ),
  
  col = ARM_COLORS,  # curve color per arm (matched by position to ARM_LEVELS)
  lty = ARM_LTYPE,   # line type per arm
  lwd = 0.9,         # line width; 0.9 is slightly thinner than default
  
  # Censoring marks: small + symbols at times when a subject left without an event
  censor_show = TRUE,  # TRUE to show marks; FALSE to hide them
  pch  = 3,              # symbol type: 3 = "+" cross
  size = 2.5,            # symbol size
  
  # Axes
  xlab     = "Time (Days)",   # x-axis label (e.g. "Time (Days)")
  yval     = "Survival", # what the y-axis shows; alternative: "Failure" = cumulative incidence
  ylab     = "Event-Free Probability",   # y-axis label (e.g. "Event-Free Probability")
  xticks   = X_TICKS,    # tick positions; uses X_TICKS defined in Step 1
  max_time = MAX_TIME,   # x-axis upper limit; uses MAX_TIME from Step 1
  ylim     = c(0, 1)     # y-axis range: 0% to 100% probability
)

p_km  # preview — curves only, no at-risk table or annotation boxes yet


# ── Step 4: Add annotations ───────────────────────────────────────────────────
# Now add the at-risk table, CI ribbon, HR box, and median survival box.
#
# Box positioning reference (normalized [0,1] canvas):
#   y = 1.0  ┌─────────────────────────────────┐
#             │  KM curves (y ≈ 0.28 to 1.0)   │
#   y ≈ 0.88 │  ┌── HR box centered here ──┐   │
#   y ≈ 0.68 │  ├── Median box here ───────┤   │
#             │  └──────────────────────────┘   │
#   y ≈ 0.28 ├─────────────────────────────────┤
#             │  At-risk table (y ≈ 0 to 0.28) │
#   y = 0.0  └─────────────────────────────────┘

p_km <- g_km(
  df = df,
  variables = list(tte = "aval", is_event = "is_event", arm = "ARM"),
  col = ARM_COLORS, lty = ARM_LTYPE, lwd = 0.9,
  censor_show = TRUE, pch = 3, size = 2.5,
  xlab = "Time (Days)", yval = "Survival", ylab = "Event-Free Probability",
  xticks = X_TICKS, max_time = MAX_TIME, ylim = c(0, 1),
  
  # at-risk table: shows how many subjects are still in study at each time tick
  annot_at_risk       = TRUE,  # TRUE to show the table below the x-axis
  annot_at_risk_title = TRUE,    # TRUE to show arm label on the left of each row
  
  # Median survival box: shows estimated median time-to-event per arm
  annot_surv_med = TRUE,  # TRUE to show the median annotation box
  
  # Cox PH HR box: shows hazard ratio + 95% CI compared to ref_group_coxph
  # Requires exactly 2 arm levels — will error with 3 arms
  annot_coxph = TRUE,         # TRUE to show the HR annotation box
  ref_group_coxph = "TRTA",   # arm label used as the denominator in HR (e.g. "TRTA")
  
  # 95% CI ribbon: pointwise confidence band around each KM curve (Greenwood's formula)
  ci_ribbon = TRUE,  # TRUE to show shaded bands; FALSE for cleaner curves
  
  # HR box position: x/y = CENTER of the box in normalized [0,1] canvas space
  control_annot_coxph = control_coxph_annot(
    x    = 0.73,   # horizontal center (0 = left, 1 = right)
    y    = 0.88, # vertical center; upper KM area ≈ 0.88
    w    = 0.42,   # box width
    h    = 0.14,   # box height
    fill = FALSE  # FALSE removes the default grey background shadow
  ),
  
  # Median box: position it below the HR box (smaller y value = lower on canvas)
  control_annot_surv_med = control_surv_med_annot(
    x    = 0.73,  # same horizontal alignment as HR box
    y    = 0.68,  # below the HR box
    w    = 0.42,
    h    = 0.16,  # slightly taller than the HR box to fit 2 rows of text; try 0.16
    fill = FALSE
  ),
  
  rel_height_plot = 0.72,  # fraction of total height for KM curves (remainder = at-risk table)
  font_size       = 10,
  title           = NULL   # title added via plot_annotation() in Step 6
)

p_km  # preview with annotations


# ── Step 5: Clean legend + final KM call ──────────────────────────────────────
# ggtheme controls the legend INSIDE the KM sub-panel.
# Re-run g_km() with all options plus the legend theme applied.

p_km_final <- g_km(
  df = df,
  variables = list(tte = "aval", is_event = "is_event", arm = "ARM"),
  col = ARM_COLORS, lty = ARM_LTYPE, lwd = 0.9,
  censor_show = TRUE, pch = 3, size = 2.5,
  xlab = "Time (Days)", yval = "Survival", ylab = "Event-Free Probability",
  xticks = X_TICKS, max_time = MAX_TIME, ylim = c(0, 1),
  annot_at_risk = TRUE, annot_at_risk_title = TRUE,
  annot_surv_med = TRUE, annot_coxph = TRUE, ci_ribbon = TRUE,
  ref_group_coxph = "TRTA",
  control_annot_coxph    = control_coxph_annot(x=0.73, y=0.88, w=0.42, h=0.14, fill=FALSE),
  control_annot_surv_med = control_surv_med_annot(x=0.73, y=0.68, w=0.42, h=0.16, fill=FALSE),
  rel_height_plot = 0.72, font_size = 10, title = NULL,
  
  # ggtheme: standard ggplot2 theme() applied inside the KM panel
  # Removes the default legend box shadow and positions legend top-left horizontal
  ggtheme = theme(
    legend.position       = "top",   # "top", "bottom", "left", "right", or c(x, y)
    legend.justification  = "left",     # anchor point of the legend box
    legend.direction      = "horizontal",
    legend.background     = element_blank(),     # remove legend background fill
    legend.box.background = element_blank(),     # remove border box around legend
    legend.key            = element_blank(),     # remove key background boxes
    legend.text           = element_text(size = 9),
    legend.title          = element_blank(),     # no legend title ("ARM" label removed)
    legend.margin         = margin(2, 0, 2, 0)
  )
)


# ── Step 6: Add title / subtitle / caption via patchwork ──────────────────────

p_final <- p_km_final +
  plot_annotation(
    title    = "Figure x.xx  Kaplan-Meier Plot: Time to First Dermatologic Event",
    subtitle = "Dose Comparison | TRTA = Low Dose; TRTB = High Dose",
    caption  = paste0(
      "Dataset: ADTTE (CDISC Pilot 01)  |  Method: Kaplan-Meier\n",  # method name, e.g. "Kaplan-Meier"
      "HR: Cox PH (TRTB vs TRTA, ref = TRTA).  Shaded: 95% pointwise CI.  +: censored.\n",
      "Dose comparison (secondary analysis). For primary: compare each arm vs Placebo separately."
    ),
    theme = theme(
      plot.title    = element_text(hjust = 0.5, size = 13, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 9,  color = "grey30"),
      plot.caption  = element_text(hjust = 0,   size = 8,  color = "grey40"),
      plot.margin   = margin(10, 15, 8, 15)
    )
  )


# ── Step 7: Save ──────────────────────────────────────────────────────────────

ggsave(
  filename = "ex02_km_output.pdf",
  plot     = p_final,  # the final annotated plot object
  width    = 24,  # cm — try 24
  height   = 20,  # cm — try 20
  units    = "cm"
)

message("Saved: ex02_km_output.pdf")


# ── Explore further ───────────────────────────────────────────────────────────
# 1. Try including Placebo (3 arms) — what happens when annot_coxph = TRUE?
# 2. Change MAX_TIME to 160 — how does the at-risk table change?
# 3. Set ci_ribbon = FALSE — is the plot cleaner or less informative?
# 4. Try yval = "Failure" — this flips the y-axis to show cumulative incidence
# 5. Change ref_group_coxph to "TRTB" — how does the HR and its direction flip?
