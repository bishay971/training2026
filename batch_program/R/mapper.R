# *******************************************************************************************************
# Program Name:                 mapper.R
# Project:                      TRA-025
# Purpose:                      Batch export ADaM datasets to submission-ready .xpt
# Original Author:              Binod Jung Bogati (linkedin.com/in/bjungbogati)
# Copyright:                    © 2025. Unauthorized distribution or reuse prohibited.
# Date Created:                 8/12/2025
# Parameters:                   NA
#
# Input:                        an in-memory ADaM dataset (e.g. adsl, adae)
# Output:                       adam/<name>.xpt
# Modifications:
#   Date        By             Changes
# **********  *************  *************************************
#
# ********************************************************************************************************/

library(rlang)
library(haven)

# export_adam() --------------------------------------------------------------
# SAS analogy: a %macro(dsin=) that takes an already-built dataset and
# writes it out as a transport (.xpt) file for submission - one call per
# ADaM dataset instead of a hand-written PROC COPY / xport step.
#
# Call it two ways:
#   export_adam(adsl)                 - name is taken from the variable
#   export_adam(adsl, name = "adsl")  - name given explicitly (needed in a
#                                        loop, where the variable is `.x`)
export_adam <- function(data, name = NULL, output_path = "adam") {
  if (is.null(name)) name <- as_label(enquo(data))

  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

  output_file <- file.path(output_path, paste0(name, ".xpt"))
  write_xpt(data, path = output_file)

  cat("Successfully created:", output_file, "\n")

  invisible(data)
}
