# Build Your Own Teal App
# Goal: Build a teal app from scratch, one module at a time.
#
# Strategy: complete ONE TODO at a time → run → confirm → next
# Do not do all TODOs at once.

library(teal)
library(teal.data)
library(teal.modules.general)
library(teal.modules.clinical)
library(pharmaverseadam)

options(shiny.useragg = FALSE)

# TODO 1: load datasets inside within() — pharmaverseadam datasets are pre-merged
# TODO 2: join_keys — use default_cdisc_join_keys after within()
data <- teal_data()
data <- within(data, {
  library(pharmaverseadam)
  ADSL <- adsl
  ADSL$ARM    <- factor(ADSL$ARM)
  ADSL$ACTARM <- factor(ADSL$ACTARM)
  adsl_only <- setdiff(names(adsl), c("STUDYID", "USUBJID"))
  ADAE <- adae[, !names(adae) %in% adsl_only]
})
join_keys(data) <- default_cdisc_join_keys[c("ADSL", "ADAE")]

# Shared arm variable — define once, reuse across all modules
# variable_choices() validates the column exists at build time
cs_arm_var <- choices_selected(
  choices  = variable_choices(adsl, "ARM"),
  selected = "ARM"
)

app_modules <- modules(
  
  tm_data_table(label = "Data Listing"),
  
  # TODO 3: tm_variable_browser    (Variable Browser)           ✓
  tm_variable_browser(
    label     = "Variable Browser",
    datanames = "ADSL"
  ),
  
  # TODO 4: tm_t_summary           (Demographics)               ✓
  tm_t_summary(
    label          = "Demographics",
    dataname       = "ADSL",
    arm_var        = cs_arm_var,
    summarize_vars = choices_selected(
      choices  = c("AGE", "SEX", "RACE", "COUNTRY"),
      selected = c("AGE", "SEX", "RACE")
    )
  ),
  
  # TODO 5: tm_t_events            (Adverse Events)             ✓
  tm_t_events(
    label      = "Adverse Events",
    dataname   = "ADAE",
    arm_var    = cs_arm_var,
    hlt        = choices_selected(
      choices  = variable_choices(adae, c("AEBODSYS", "AESOC")),
      selected = "AEBODSYS"
    ),
    llt        = choices_selected(
      choices  = variable_choices(adae, c("AETERM", "AEDECOD")),
      selected = "AEDECOD"
    ),
    add_total  = TRUE,
    event_type = "adverse event"
  ),
  
)

# TODO 7: replace "" with your app title                        ✓
# teal_slices: pre-configure filter panel on launch
# SAFFL = "Y" pins the app to safety-evaluable population
app <- init(
  data    = data,
  modules = app_modules,
  filter  = teal_slices(
    count_type = "all",
    teal_slice(dataname = "ADSL", varname = "SAFFL",  selected = "Y"),
    teal_slice(dataname = "ADSL", varname = "SEX"),
    teal_slice(dataname = "ADSL", varname = "AGE")
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

# Final checklist:
# [x] App launches without error
# [x] Demographics: AGE, SEX, RACE visible, split by ARM
# [x] AE tab: AEBODSYS and AEDECOD columns visible + Total column
# [x] Filter ARM → AE tab shows only those patients (requires TODO 2)
# [x] Filter panel: SAFFL pre-set to Y (safety population)


# ══════════════════════════════════════════════════════════════════
# TODO GUIDE (reference)
# ══════════════════════════════════════════════════════════════════
#
# TODO 1+2: load datasets inside within(), set join_keys after
#   data <- teal_data()
#   data <- within(data, {
#     library(pharmaverseadam)
#     ADSL <- adsl
#     ADSL$ARM    <- factor(ADSL$ARM)
#     ADSL$ACTARM <- factor(ADSL$ACTARM)
#     adsl_only <- setdiff(names(adsl), c("STUDYID", "USUBJID"))
#     ADAE <- adae[, !names(adae) %in% adsl_only]
#   })
#   join_keys(data) <- default_cdisc_join_keys[c("ADSL", "ADAE")]
#
# TODO 3: variable browser (add after tm_data_table, with comma)
#   tm_variable_browser(
#     label     = "Variable Browser",
#     datanames = "ADSL"
#   )
#
# TODO 4: demographics summary table
#   tm_t_summary(
#     label          = "Demographics",
#     dataname       = "ADSL",
#     arm_var        = cs_arm_var,
#     summarize_vars = choices_selected(
#       choices  = c("AGE", "SEX", "RACE", "COUNTRY"),
#       selected = c("AGE", "SEX", "RACE")
#     )
#   )
#
# TODO 5: AE frequency table
#   tm_t_events(
#     label      = "Adverse Events",
#     dataname   = "ADAE",
#     arm_var    = cs_arm_var,
#     hlt        = choices_selected(
#       choices  = variable_choices(adae, c("AEBODSYS", "AESOC")),
#       selected = "AEBODSYS"
#     ),
#     llt        = choices_selected(
#       choices  = variable_choices(adae, c("AETERM", "AEDECOD")),
#       selected = "AEDECOD"
#     ),
#     add_total  = TRUE,
#     event_type = "adverse event"
#   )
#
# TODO 6: app title + filter panel + header/footer
#   app <- init(
#     data    = data,
#     modules = app_modules,
#     filter  = teal_slices(
#       count_type = "all",
#       teal_slice(dataname = "ADSL", varname = "SAFFL", selected = "Y"),
#       teal_slice(dataname = "ADSL", varname = "SEX"),
#       teal_slice(dataname = "ADSL", varname = "AGE")
#     )
#   ) |>
#     modify_title("Clinical Study Explorer") |>
#     modify_header(tags$span(style = "font-size: 24px; font-weight: bold;",
#       "Clinical Study Explorer — Training App")) |>
#     modify_footer(tags$p("Training app built with teal. Data: pharmaverseadam."))
