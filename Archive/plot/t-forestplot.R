
# forest plot

library(tidyverse)
library(tern)
library(rtables)
library(clinicalfd)
library(survival)
library(patchwork)

data("adtte")


sex_fmt <- c(
  "F" = "Female", 
  "M" = "Male"
)

adtte_df <- adtte |> 
  # filter(trt) |> 
  mutate(
    SEX = factor(sex, levels = names(sex_fmt), 
                 labels = unname(sex_fmt)
    ),
    AGEGR1  = factor(agegr1, levels= c("<65", "65-80", ">80")), 
    RACE = factor(race), 
    
    ARM = factor(trta, levels = c("Placebo", "Xanomeline High Dose"), labels = c("Placebo", "High Dose")), 
    is_event = cnsr == 0
  )

# Cox PH HRs

# hazard ratio

surv_subgroups <- extract_survival_subgroups(
  variables = list(
    tte = "aval", 
    is_event = "is_event", 
    arm = "ARM", 
    subgroups = c("SEX", "AGEGR1")
  ), 
  data = adtte_df, 
  control = control_coxph(conf_level = 0.95)
)

## forest table

forest_tbl <- basic_table() |> 
  tabulate_survival_subgroups(
    df = surv_subgroups, 
    vars = c("n_tot_events", "n_events", "median", "hr", "ci"),
    time_unit = "Days"
  )

# forest plot

?g_forest


p_forest <- g_forest(
  tbl = forest_tbl,
  vline = 1,
  xlim = c(0.1, 10),
  logx = TRUE,
  x_at = c(0.1, 1, 10),
  font_size = 12
)



p_final <- p_forest |> 
  plot_annotation(
    title = "Figure X.XX \n Forest Plot", 
    
    caption = paste0(
      "Dataset: ADTTE"
    ), 
    theme = theme(plot.title = element_text(size = 16))
  )


# ?plot_annotation
# 
# p_final
# 
# ggsave(
#   filename = "p-forest.pdf", 
#   plot = p_final, 
#   width = 42, 
#   height = 20, 
#   units = "cm"
# )
# 



# proc ;








?g_forest()
