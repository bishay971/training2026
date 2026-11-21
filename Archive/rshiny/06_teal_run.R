# Teal — Run It First
# Goal: Run a complete teal app. Understand the 3-step structure.
# TODOs: 0 — just run and explore
#
# Previous exercises ≈ 150 lines for one study
# Teal              ≈ 30 lines, works for any CDISC study
#
# What teal adds automatically:
#   - Filter panel for every variable in every dataset
#   - Cross-dataset filtering (filter ADSL → ADAE updates)
#   - Download buttons on every output
#   - "Show R code" button for reproducibility
#
# 3 steps:
#   Step 1: teal_data()  → wrap your CDISC datasets + join_keys
#   Step 2: modules()    → pick pre-built clinical modules
#   Step 3: init()       → teal assembles the app
#
# RUN NOW. Explore for a few minutes. Then read the code.

library(teal)
library(teal.data)
library(teal.modules.general)
library(teal.modules.clinical)
library(pharmaverseadam)

# Prevents font rendering errors in some environments
options(shiny.useragg = FALSE)

# pharmaverseadam datasets are pre-merged: ADAE/ADLB already contain every ADSL
# column. teal merges analysis dataset with ADSL at runtime → duplicate columns
# crash. Strip ADSL-only columns inside within(), convert ARM to factor.
data <- teal_data()
data <- within(data, {
  library(pharmaverseadam)
  ADSL <- adsl
  ADSL$ARM    <- factor(ADSL$ARM)
  ADSL$ACTARM <- factor(ADSL$ACTARM)
  adsl_only <- setdiff(names(adsl), c("STUDYID", "USUBJID"))
  ADAE <- adae[, !names(adae) %in% adsl_only]
  adlb_trim <- adlb[, !names(adlb) %in% adsl_only]
  ADLB <- adlb_trim[grepl("^(BASELINE|WEEK)", adlb_trim$AVISIT, ignore.case = TRUE), ]
})
join_keys(data) <- default_cdisc_join_keys[c("ADSL", "ADAE", "ADLB")]

# Shared arm variable — define once, reuse across all modules
# variable_choices(): validates column exists in dataset at build time
cs_arm_var <- choices_selected(
  choices  = variable_choices(adsl, "ARM"),
  selected = "ARM"
)

# choices_selected(choices, selected) — dropdown with a default
# data_extract_spec + select_spec — used by tm_g_distribution
app_modules <- modules(
  
  tm_data_table(label = "Data Listing"),
  
  tm_t_summary(
    label          = "Demographics",
    dataname       = "ADSL",
    arm_var        = cs_arm_var,
    summarize_vars = choices_selected(
      c("AGE", "SEX", "RACE"), c("AGE", "SEX", "RACE")
    )
  ),
  
  tm_t_events(
    label      = "Adverse Events",
    dataname   = "ADAE",
    arm_var    = cs_arm_var,
    # variable_choices() restricts dropdown to only valid columns
    hlt        = choices_selected(
      choices  = variable_choices(adae, c("AEBODSYS", "AESOC")),
      selected = "AEBODSYS"
    ),
    llt        = choices_selected(
      choices  = variable_choices(adae, c("AETERM", "AEDECOD")),
      selected = "AEDECOD"
    ),
    add_total  = TRUE,           # adds "All Patients" total column
    event_type = "adverse event"
  ),
  
  tm_t_summary_by(
    label          = "Lab Values",
    dataname       = "ADLB",
    arm_var        = cs_arm_var,
    by_vars        = choices_selected(
      choices  = variable_choices(adlb, c("PARAM", "AVISIT")),
      selected = c("PARAM", "AVISIT"),
      fixed    = TRUE
    ),
    summarize_vars = choices_selected(
      choices  = variable_choices(adlb, c("AVAL", "CHG")),
      selected = "AVAL"
    ),
    paramcd        = choices_selected(
      choices  = value_choices(adlb, "PARAMCD", "PARAM"),
      selected = "ALT"
    )
  )
)

# teal_slices: pre-configure the filter panel
# count_type = "all" — show subject counts across all datasets
# teal_slice(dataname, varname, selected) — pre-select a filter value
# SAFFL = "Y" pins the app to the safety-evaluable population on launch
app <- init(
  data    = data,
  modules = app_modules,
  filter  = teal_slices(
    count_type = "all",
    teal_slice(dataname = "ADSL", varname = "SAFFL",  selected = "Y"),
    teal_slice(dataname = "ADSL", varname = "SEX"),
    teal_slice(dataname = "ADSL", varname = "AGE"),
    teal_slice(dataname = "ADLB", varname = "AVAL")
  )
) |>
  modify_title("Clinical Study Explorer") |>
  modify_header(
    tags$span(
      style = "font-size: 24px; font-weight: bold;",
      "Clinical Study Explorer — Training App"
    )
  ) |>
  modify_footer(
    tags$p("Training app built with teal. Data: pharmaverseadam.")
  )

shinyApp(app$ui, app$server)

# Try these:
# 1. Filter panel: SAFFL is pre-set to "Y" (safety population) — notice subject count
# 2. Filter ARM in filter panel → switch to AE tab (only those patients shown)
# 3. Demographics: add COUNTRY to "Summarize variables"
# 4. AE tab: click "Show R code" top right
# 5. AE tab: notice "Total" column from add_total = TRUE
# 6. Lab Values: change group from PARAMCD to ARM
