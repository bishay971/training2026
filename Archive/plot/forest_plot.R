# ══════════════════════════════════════════════════════════════════════════════
# Exercise 01: Subgroup Forest Plot with tern
#
# Endpoint: TTDE — Time to First Dermatologic Event (safety endpoint)
# Comparison: Xanomeline High Dose vs Placebo
#
# Placebo median is NE (Not Estimable): Xanomeline causes dermatologic AEs.
# Placebo subjects rarely experience the event → KM curve stays above 50% →
# median survival time cannot be computed. NE is a clinical finding, not an error.
#
# HR direction for a safety/AE endpoint:
#   HR > 1 = comparator reaches event sooner (more adverse events)
#   HR < 1 = comparator reaches event later  (fewer adverse events)
#
# Pipeline:
#   1. extract_survival_subgroups() → Cox PH hazard ratio per subgroup
#   2. tabulate_survival_subgroups() → rtables TableTree (table structure)
#   3. g_forest()                   → ggplot forest figure from the TableTree
# ══════════════════════════════════════════════════════════════════════════════


# ── Step 0: Load packages ──────────────────────────────────────────────────────

library(tidyverse)
library(________)   # survival models: Surv(), coxph(), survfit()
library(tern)       # clinical figures: extract_survival_subgroups(), g_forest()
library(________)   # table structure: basic_table(), tabulate_survival_subgroups()
library(patchwork)  # figure assembly: plot_annotation() adds title/caption layer
library(________)   # CDISC Pilot 01 datasets: adtte, adlbc, adsl


# ── Step 1: Parameters ────────────────────────────────────────────────────────
# Define once at the top — REF_ARM and COMP_ARM flow through filter, factor
# levels, plot title, and caption automatically without touching the rest of code.

REF_ARM  <- "________"              # reference arm   → used in filter(), factor(), title
COMP_ARM <- "Xanomeline High Dose"  # comparator arm  → used in filter(), factor(), title

# X_LIM and X_AT control the HR axis appearance inside g_forest()
# Log scale: HR 2.0 (double risk) and HR 0.5 (half risk) sit equidistant from 1
X_LIM <- c(0.1, ______)             # lower and upper HR axis bounds (log scale)
X_AT  <- c(0.25, 0.5, 1, 2, 4)     # tick mark positions on the log HR axis


# ── Step 2: Load and prepare data ─────────────────────────────────────────────
# tern requires a base data.frame — not a tibble.
# Subgroup columns must be factor type so the function loops within levels.

data("adtte")
unique(adtte$trta)   # inspect available arm names before filtering

df <- adtte |>
  filter(trta %in% c(REF_ARM, COMP_ARM)) |>
  mutate(
    # CDISC convention: cnsr = 0 → event occurred (opposite of base R survival)
    # cnsr = 1 → censored (subject left study without experiencing the event)
    is_event = cnsr == ______,    # fill in: the integer value that means "event occurred"
    
    # ARM: two-level factor — reference arm must be the FIRST level
    # g_forest() uses this order to place reference vs comparator columns
    ARM = factor(trta,
                 levels = c(REF_ARM, COMP_ARM),
                 labels = c("______", "______")),  # short display labels for column headers
    
    # Subgroup factors — character columns won't work; must convert to factor
    SEX    = factor(sex,
                    levels = c("F", "M"),
                    labels = c("______", "Male")),         # label for Female
    
    AGEGR1 = factor(______, levels = c("<65", "65-80", ">80"))  # column name in adtte
  ) |>
  as.data.frame()  # convert tibble → base data.frame (tern requirement)

cat(sprintf("Subjects: %d  |  Events: %d (%.0f%%)\n",
            nrow(df), sum(df$is_event), 100 * mean(df$is_event)))


# ── Step 3: Extract subgroup stats — Cox PH per subgroup ─────────────────────
# extract_survival_subgroups() fits a separate Cox PH model inside each subgroup
# and returns a LIST with two data.frames:
#   $survtime — median survival time per arm × subgroup (Placebo will be NE)
#   $hr       — hazard ratio, 95% CI, p-value per subgroup

surv_subgroups <- extract_survival_subgroups(
  variables = list(
    tte       = "______",             # time-to-event column (numeric, days)
    is_event  = "______",             # logical event indicator (TRUE = event)
    arm       = "______",             # treatment arm factor (2 levels)
    subgroups = c("______", "______") # subgroup factor columns to loop over
  ),
  data      = df,
  label_all = "All Patients",         # row label for the overall (non-subgroup) estimate
  control   = control_coxph(conf_level = ______)  # confidence level: 0.95 = 95% CI
)

str(surv_subgroups)  # always inspect: confirms list structure with $survtime and $hr


# ── Step 4: Build rtables TableTree ───────────────────────────────────────────
# tabulate_survival_subgroups() converts the list from Step 3 into an rtables
# TableTree object that g_forest() reads. The `vars` argument controls which
# columns appear in the text panel left of the forest plot.

forest_tbl <- basic_table() |>
  tabulate_survival_subgroups(
    df   = ______,  # the list returned by extract_survival_subgroups()
    vars = c(
      "n_tot_events",  # total events (both arms combined) per subgroup row
      "n_events",      # events per arm (shown as separate columns)
      "median",        # median survival time per arm (NE when < 50% had event)
      "hr",            # Cox PH hazard ratio
      "______"         # 95% confidence interval of the HR
    ),
    time_unit = "______"  # label for the median column header (e.g. "Days")
  )

print(forest_tbl)  # preview in console — useful to check before plotting


# ── Step 5: Create forest plot with g_forest() ────────────────────────────────
# g_forest() reads column attributes set by tabulate_survival_subgroups() and
# draws: text table (left) + forest panel with dots and CI bars (right).

p_forest <- g_forest(
  tbl  = ______,  # TableTree from Step 4
  
  # HR axis — always log scale for forest plots
  xlim = ______,  # X_LIM: axis range; narrow range (0.1–10) is standard for single-trial
  logx = ______,  # TRUE: log scale makes HR symmetric — required for correct visual distance
  x_at = ______,  # X_AT: custom tick positions; include 1 so the null line is labeled
  
  vline = ______,  # vertical reference line; HR = 1 means no difference between arms
  
  # Appearance
  col              = "______",  # color for all dots and CI bars (e.g. "#2166AC" dark blue)
  font_size        = 8,         # text size for table values; reduce if rows are dense
  rel_width_forest = ______     # fraction of total width used by the forest panel
  # 0.35 = 35% forest, 65% table; increase if plot looks cramped
)


# ── Step 6: Add title / subtitle / caption via patchwork ──────────────────────
# plot_annotation() wraps any ggplot or patchwork object with title/caption.
# For a safety endpoint, avoid "favors" language — HR direction differs from efficacy.

p_final <- p_forest +
  plot_annotation(
    title    = sprintf("Figure x.xx  Subgroup Forest Plot: %s vs %s",
                       ______, REF_ARM),  # comparator arm first, then reference
    subtitle = "Time to First Dermatologic Event  |  Safety Population  |  Cox PH, 95% Wald CI",
    caption  = paste0(
      "Dataset: ADTTE (CDISC Pilot 01)  |  Reference: ", REF_ARM, "\n",
      "HR > 1: higher event rate in ", COMP_ARM, ".  Placebo median NE = not estimable ",
      "(< 50% of Placebo subjects experienced the event).\n",
      "Hazard ratios from unadjusted Cox PH model within each subgroup."
    ),
    theme = theme(
      plot.title    = element_text(hjust = 0.5, size = 13, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 9,  color = "grey30"),
      plot.caption  = element_text(hjust = 0,   size = 8,  color = "grey40"),
      plot.margin   = margin(10, 15, 8, 15)
    )
  )


# ── Step 7: Save ──────────────────────────────────────────────────────────────
# Width >= 40 cm keeps table column text readable in the final PDF.

ggsave(
  filename = "ex01_forest_output.pdf",
  plot     = ______,  # the annotated final plot object
  width    = ______,  # cm — try 42; forest plots need width for the text table
  height   = ______,  # cm — try 20
  units    = "cm"
)

message("Saved: ex01_forest_output.pdf")


# ── Explore further ───────────────────────────────────────────────────────────
# 1. Change COMP_ARM to "Xanomeline Low Dose" and re-run — does the HR change?
# 2. Add RACE as a third subgroup:
#      RACE = factor(race) in the mutate() step
#      subgroups = c("SEX", "AGEGR1", "RACE")
# 3. Change col to "#D62728" (red) — what changes in the output?
# 4. Narrow xlim to c(0.5, 5) — which subgroups get clipped off the axis?
