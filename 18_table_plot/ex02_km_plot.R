# ══════════════════════════════════════════════════════════════════════════════
# Exercise 02: Kaplan-Meier Plot with tern
# Workshop: Clinical Plots in R
# Mode: Student exercise — fill in all ______ blanks
#
# Goal: Reproduce p-tern-km.pdf using tern + clinicalfd
# Reference output: 00_plots/p-tern-km.pdf
# Reference script: 00_plots/p-tern-km.R (complete version — look after)
#
# Clinical context:
#   Endpoint: TTDE — Time to First Dermatologic Event (safety endpoint)
#   Arms: TRTA = Xanomeline Low Dose  |  TRTB = Xanomeline High Dose
#   Placebo excluded: g_km() with annot_coxph needs exactly 2 arms.
#   This is a DOSE COMPARISON (secondary analysis).
#   For FDA primary submission: run separately per active arm vs Placebo.
#
# Function: g_km() builds KM curves + at-risk table + HR + median boxes
# Note: annotation x/y coordinates are in normalized [0,1] space, not days
#
# Time: ~15 minutes
# ══════════════════════════════════════════════════════════════════════════════

# ── Step 0: Load packages ──────────────────────────────────────────────────────

library(tidyverse)
library(________)    # Surv(), coxph()
library(tern)        # g_km(), control_coxph_annot(), control_surv_med_annot()
library(patchwork)   # plot_annotation()
library(________)    # practice data: adtte


# ── Step 1: Parameters ─────────────────────────────────────────────────────────

ARM_LEVELS <- c("TRTA", "TRTB")
ARM_COLORS <- c("______", "#F44336")    # HINT: blue = "#2196F3"
ARM_LTYPE  <- c("dashed", "dotdash")

MAX_TIME <- ______                       # HINT: x-axis end (days); try 200
X_TICKS  <- seq(0, MAX_TIME, by = ______)  # HINT: tick every 50 days


# ── Step 2: Load and prepare data ──────────────────────────────────────────────

data("adtte")

df <- adtte |>
  mutate(trta = as.character(trta)) |>  # strip labelled class first
  filter(trta != "______") |            # HINT: arm to exclude (3rd arm)
  mutate(
    is_event = cnsr == 0L,              # CDISC: 0 = event
    ARM = factor(
      dplyr::case_when(
        trta == "Xanomeline Low Dose"  ~ "______",   # HINT: short label A
        trta == "Xanomeline High Dose" ~ "______",   # HINT: short label B
        TRUE ~ NA_character_
      ),
      levels = ARM_LEVELS
    )
  ) |>
  filter(!is.na(aval), !is.na(______)) |>  # HINT: filter out NA arms
  as.data.frame()

cat(sprintf("Subjects: %d  |  Events: %d\n", nrow(df), sum(df$is_event)))
print(table(df$ARM, df$is_event, dnn = c("ARM", "event")))


# ── Step 3: Core g_km() — curves only (preview) ───────────────────────────────
# Build the KM plot without annotations first to check the curves

p_km <- g_km(
  df        = ______,       # HINT: your prepared data.frame
  variables = list(
    tte      = "______",    # HINT: time column (days)
    is_event = "______",    # HINT: logical event column
    arm      = "______"     # HINT: factor ARM column
  ),
  col  = ARM_COLORS,
  lty  = ARM_LTYPE,
  lwd  = 0.9,

  censor_show = ______,     # HINT: TRUE to show + marks at censored times
  pch  = 3,
  size = 2.5,

  xlab     = "______",      # HINT: "Time (Days)"
  yval     = "Survival",
  ylab     = "______",      # HINT: "Event-Free Probability"
  xticks   = X_TICKS,
  max_time = MAX_TIME,
  ylim     = c(0, 1)
)

p_km   # preview — curves only, no annotations yet


# ── Step 4: Add annotations ────────────────────────────────────────────────────
# Add at-risk table, HR box, median survival box, and CI ribbon
# IMPORTANT: x/y in control_*_annot() are NORMALIZED [0,1] coordinates
#   Canvas layout:  y=1.0 (top) ... y≈0.28 (KM/risk boundary) ... y=0.0 (bottom)
#   KM area ≈ y 0.28–1.0;  at-risk area ≈ y 0.0–0.28

p_km <- g_km(
  df        = df,
  variables = list(tte = "aval", is_event = "is_event", arm = "ARM"),
  col  = ARM_COLORS,  lty = ARM_LTYPE,  lwd = 0.9,
  censor_show = TRUE, pch = 3, size = 2.5,
  xlab = "Time (Days)", yval = "Survival", ylab = "Event-Free Probability",
  xticks = X_TICKS, max_time = MAX_TIME, ylim = c(0, 1),

  annot_at_risk       = ______,    # HINT: show at-risk numbers below x-axis?
  annot_at_risk_title = TRUE,
  annot_surv_med      = ______,    # HINT: show median survival box?
  annot_coxph         = ______,    # HINT: show Cox PH HR box?
  ci_ribbon           = ______,    # HINT: show 95% CI shaded bands?

  ref_group_coxph     = "______",  # HINT: which arm is reference for HR? (TRTA or TRTB)

  control_annot_coxph = control_coxph_annot(
    x    = 0.73,
    y    = ______,     # HINT: upper KM area ≈ 0.88
    w    = 0.42,
    h    = 0.14,
    fill = ______      # HINT: FALSE removes grey shadow box
  ),
  control_annot_surv_med = control_surv_med_annot(
    x    = 0.73,
    y    = 0.68,       # below Cox PH box
    w    = 0.42,
    h    = ______,     # HINT: slightly taller than coxph box ≈ 0.16
    fill = FALSE
  ),

  rel_height_plot = 0.72,   # 72% for curves, 28% for at-risk
  font_size       = 10,
  title           = NULL
)

p_km   # preview — with annotations


# ── Step 5: Clean legend + final plot ──────────────────────────────────────────
# ggtheme controls legend inside the KM panel
# Re-run full g_km() with all options + legend theme

p_km_final <- g_km(
  df        = df,
  variables = list(tte = "aval", is_event = "is_event", arm = "ARM"),
  col  = ARM_COLORS,  lty = ARM_LTYPE,  lwd = 0.9,
  censor_show = TRUE, pch = 3, size = 2.5,
  xlab = "Time (Days)", yval = "Survival", ylab = "Event-Free Probability",
  xticks = X_TICKS, max_time = MAX_TIME, ylim = c(0, 1),
  annot_at_risk = TRUE, annot_at_risk_title = TRUE,
  annot_surv_med = TRUE, annot_coxph = TRUE, ci_ribbon = TRUE,
  ref_group_coxph = "TRTA",
  control_annot_coxph    = control_coxph_annot(x=0.73, y=0.88, w=0.42, h=0.14, fill=FALSE),
  control_annot_surv_med = control_surv_med_annot(x=0.73, y=0.68, w=0.42, h=0.16, fill=FALSE),
  rel_height_plot = 0.72, font_size = 10, title = NULL,

  ggtheme = theme(
    legend.position       = "______",   # HINT: "top"
    legend.justification  = "left",
    legend.direction      = "horizontal",
    legend.background     = element_blank(),
    legend.box.background = element_blank(),
    legend.key            = element_blank(),
    legend.text           = element_text(size = 9),
    legend.title          = element_blank(),
    legend.margin         = margin(2, 0, 2, 0)
  )
)


# ── Step 6: Add title / subtitle / caption via patchwork ──────────────────────

p_final <- p_km_final +
  plot_annotation(
    title    = "Figure x.xx  Kaplan-Meier Plot: Time to First Dermatologic Event",
    subtitle = "______",    # HINT: include arms and population (TRTA = Low Dose; TRTB = High Dose)
    caption  = paste0(
      "Dataset: ADTTE  |  Method: ______\n",  # HINT: method name
      "HR: Cox PH (TRTB vs TRTA, ref = TRTA).  Shaded: 95% CI.  +: censored.\n",
      "Note: Dose comparison (secondary). For primary: compare each arm vs Placebo."
    ),
    theme = theme(
      plot.title    = element_text(hjust = 0.5, size = 13, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 9,  color = "grey30"),
      plot.caption  = element_text(hjust = 0,   size = 8,  color = "grey40"),
      plot.margin   = margin(10, 15, 8, 15)
    )
  )


# ── Step 7: Save ───────────────────────────────────────────────────────────────

ggsave(
  filename = "00_plots/exercises/ex02_km_output.pdf",
  plot     = ______,    # HINT: final plot object
  width    = ______,    # HINT: try 24
  height   = ______,    # HINT: try 20
  units    = "cm"
)

message("Saved: 00_plots/exercises/ex02_km_output.pdf")
message("Compare with: 00_plots/p-tern-km.pdf")


# ══════════════════════════════════════════════════════════════════════════════
# BONUS CHALLENGES (if you finish early):
#   1. Add Placebo arm (3 arms) — what happens to annot_coxph?
#      (tern requires exactly 2 arms for Cox PH annotation → error or warning)
#   2. Change MAX_TIME to 160 — what changes in the at-risk table?
#   3. Set ci_ribbon = FALSE — is the plot cleaner or less informative?
#   4. Try yval = "Failure" — what does this show vs "Survival"?
#   5. Change ref_group_coxph to "TRTB" — how does the HR direction flip?
# ══════════════════════════════════════════════════════════════════════════════
