# Setup — Run Once Before Starting

install.packages(c(
  "shiny", "DT", "dplyr",
  "teal", "teal.data",
  "teal.modules.general",
  "teal.modules.clinical",
  "pharmaverseadam"
))

library(shiny)
library(dplyr)
library(DT)
library(pharmaverseadam)

# Verify data
adsl |> select(USUBJID, ARM, AGE, SEX, RACE) |> head()

nrow(adsl)
adsl |> distinct(ARM)
names(adae)

# Raise your hand if you see any ERROR messages above.
