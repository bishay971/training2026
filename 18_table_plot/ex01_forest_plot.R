# ══════════════════════════════════════════════════════════════════════════════
# Exercise 01: Subgroup Forest Plot with tern
# Workshop: Clinical Plots in R
# Mode: Student exercise — fill in all ______ blanks
#
# Goal: Reproduce p-tern-forest.pdf using tern + clinicalfd
# Reference output: 00_plots/p-tern-forest.pdf
# Reference script: 00_plots/p-tern-forest.R (complete version — look after)
#
# Clinical context:
#   Endpoint: TTDE — Time to First Dermatologic Event (safety endpoint)
#   Comparison: Xanomeline High Dose vs Placebo
#   Why Placebo median is NE: Xanomeline causes this AE; Placebo subjects
#   rarely reach the event → KM curve stays > 50% → median not estimable.
#   HR > 1 = High Dose has events sooner (expected for safety endpoint).
#
# Pipeline:
#   1. extract_survival_subgroups() → Cox PH per subgroup
#   2. tabulate_survival_subgroups() → rtables TableTree
#   3. g_forest() → forest figure
#
# Time: ~15 minutes
# ══════════════════════════════════════════════════════════════════════════════

# ── Step 0: Load packages ──────────────────────────────────────────────────────

library(tidyverse)
library(________)    # provides Surv(), coxph()
library(tern)        # provides extract_survival_subgroups, g_forest
library(________)    # provides basic_table(), tabulate_survival_subgroups
library(patchwork)   # provides plot_annotation()
library(________)    # provides practice data: adtte, adlbc, adsl


# ── Step 1: Set parameters ─────────────────────────────────────────────────────

REF_ARM  <- "________"               # HINT: look at unique(adtte$trta) — reference arm
COMP_ARM <- "Xanomeline High Dose"   # comparator arm

X_LIM <- c(0.1, ______)              # HR axis limits (log scale); try 0.1 to 10
X_AT  <- c(0.25, 0.5, 1, 2, 4)      # tick positions on log HR axis


# ── Step 2: Load and prepare data ──────────────────────────────────────────────
# REQUIRED: tern needs base data.frame (not tibble)
# REQUIRED: subgroup variables MUST be factors

data("adtte")
unique(adtte$trta)   # check arm names first

df <- adtte |>
  filter(trta %in% c(REF_ARM, COMP_ARM)) |>
  mutate(
    # CDISC: cnsr = 0 → event occurred (opposite of base R survival)
    is_event = cnsr == ______,    # HINT: fill in the value for "event"

    # Treatment arm: 2-level factor, reference arm first
    ARM = factor(trta,
                 levels = c(REF_ARM, COMP_ARM),
                 labels = c("______", "______")),   # HINT: short display labels

    # Subgroup factors — must be factors, not characters
    SEX    = factor(sex,
                    levels = c("F", "M"),
                    labels = c("______", "Male")),  # HINT: label for Female

    AGEGR1 = factor(______, levels = c("<65", "65-80", ">80"))  # HINT: column name
  ) |>
  as.data.frame()   # REQUIRED: tern does not accept tibble

cat(sprintf("Subjects: %d  |  Events: %d (%.0f%%)\n",
            nrow(df), sum(df$is_event), 100 * mean(df$is_event)))


# ── Step 3: Extract subgroup stats (Cox PH per subgroup) ──────────────────────
# extract_survival_subgroups() returns a LIST:
#   $survtime — median survival per arm × subgroup
#   $hr       — Cox PH hazard ratio per subgroup
# NOTE: Placebo $survtime median will be NE — expected (see clinical context above)

surv_subgroups <- extract_survival_subgroups(
  variables = list(
    tte       = "______",              # HINT: time-to-event column in adtte
    is_event  = "______",              # HINT: the logical column you created
    arm       = "______",              # HINT: the factor ARM column
    subgroups = c("______", "______")  # HINT: the two subgroup factor columns
  ),
  data      = df,
  label_all = "All Patients",
  control   = control_coxph(conf_level = ______)  # HINT: standard 95% = 0.95
)

str(surv_subgroups)   # inspect: list with $survtime and $hr


# ── Step 4: Build rtables TableTree ───────────────────────────────────────────
# tabulate_survival_subgroups() wraps the list into a structure
# that g_forest() knows how to read

forest_tbl <- basic_table() |>
  tabulate_survival_subgroups(
    df        = ______,     # HINT: the list from Step 3
    vars      = c("n_tot_events", "n_events", "median", "hr", "______"),  # HINT: CI column
    time_unit = "______"    # HINT: time unit (AVAL is in days)
  )

print(forest_tbl)


# ── Step 5: Create forest plot with g_forest() ────────────────────────────────
# HR axis: always log scale. vline = 1 = null effect (no difference between arms).

p_forest <- g_forest(
  tbl              = ______,    # HINT: TableTree from Step 4
  xlim             = ______,    # HINT: X_LIM from Step 1
  logx             = ______,    # HINT: TRUE for log-scale HR axis
  x_at             = ______,    # HINT: X_AT from Step 1
  vline            = ______,    # HINT: null effect line at HR = ?
  col              = "______",  # HINT: try "#2166AC" (dark blue)
  font_size        = 8,
  rel_width_forest = ______     # HINT: 0.35 = forest takes 35% of width
)


# ── Step 6: Add title, subtitle, caption via patchwork ─────────────────────────
# For safety endpoint: HR > 1 means comparator has more events (not "favors")

p_final <- p_forest +
  plot_annotation(
    title    = sprintf("Figure x.xx  Subgroup Forest Plot: %s vs %s",
                       ______, REF_ARM),   # HINT: first arg = comparator arm
    subtitle = "Time to First Dermatologic Event  |  Safety Population  |  Cox PH, 95% Wald CI",
    caption  = paste0(
      "Dataset: ADTTE  |  Reference: ", REF_ARM, "\n",
      "HR > 1: higher event rate in ", COMP_ARM, ";  Placebo median NE = not estimable.\n",
      "Cox PH unadjusted within each subgroup."
    ),
    theme = theme(
      plot.title    = element_text(hjust = 0.5, size = 13, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 9,  color = "grey30"),
      plot.caption  = element_text(hjust = 0,   size = 8,  color = "grey40"),
      plot.margin   = margin(10, 15, 8, 15)
    )
  )


# ── Step 7: Save to PDF ────────────────────────────────────────────────────────

ggsave(
  filename = "00_plots/exercises/ex01_forest_output.pdf",
  plot     = ______,    # HINT: the final annotated plot
  width    = ______,    # HINT: try 42 cm (wide for readable table columns)
  height   = ______,    # HINT: try 20 cm
  units    = "cm"
)

message("Saved: 00_plots/exercises/ex01_forest_output.pdf")
message("Compare with: 00_plots/p-tern-forest.pdf")


# ══════════════════════════════════════════════════════════════════════════════
# BONUS CHALLENGES (if you finish early):
#   1. Change COMP_ARM to "Xanomeline Low Dose" and re-run — does HR change?
#   2. Add RACE as a third subgroup:
#      - Add RACE = factor(race) in the mutate() step
#      - Add "RACE" to subgroups = c("SEX", "AGEGR1", "RACE")
#   3. Change the dot color to "#D62728" (red)
#   4. Narrow xlim to c(0.5, 5) — what subgroups get clipped?
# ══════════════════════════════════════════════════════════════════════════════
