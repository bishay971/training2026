# *******************************************************************************************************
# Program Name:                 copy_rename.R
# Project:                      TRA-025
# Purpose:                      Batch-archive R programs as read-only .txt copies
# Original Author:              Binod Jung Bogati (linkedin.com/in/bjungbogati)
# Copyright:                    © 2025. Unauthorized distribution or reuse prohibited.
# Date Created:                 8/12/2025
# Parameters:                   NA
#
# Input:                        every *.R program in R/
# Output:                       matching *.txt copy in programs/
# Modifications:
#   Date        By             Changes
# **********  *************  *************************************
#
# ********************************************************************************************************/

library(tools)

## batch copy + rename .R programs to .txt ----------------------------------
# SAS analogy: many validation environments keep a locked, non-executable
# copy of each .sas program for QC/audit. This does the R equivalent -
# copy every program to programs/ and swap the extension to .txt so it
# can be opened/reviewed without being run.

# 1. find every source program, skipping the batch utilities themselves
#    (batch.R, mapper.R, copy_rename.R) since those aren't domain programs
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
r_files <- r_files[!basename(r_files) %in% c("batch.R", "mapper.R", "copy_rename.R")]

# 2. make sure the archive folder exists (like PROC OPTIONS / autoexec
#    creating the output libref if it isn't there yet)
if (!dir.exists("programs")) dir.create("programs")

# 3. build the matching .txt path for each .R file, then copy in one pass
txt_files <- file.path("programs", paste0(file_path_sans_ext(basename(r_files)), ".txt"))
file.copy(r_files, txt_files, overwrite = TRUE)

cat("Archived", length(r_files), "program(s) to programs/ as .txt\n")
