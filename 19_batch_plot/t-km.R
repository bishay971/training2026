# *******************************************************************************************************
# Program Name:                 t-km.R
# Project:                      TRA-025
# Purpose:                      Create KM plot
# Original Author:              Binod Jung Bogati (linkedin.com/in/bjungbogati)
# Copyright:                    © 2025. Unauthorized distribution or reuse prohibited.
# Date Created:                 8/12/2025
# Parameters:                   NA
#
# Input:
# Output:
# Modifications:
#   Date        By             Changes
# **********  *************  *************************************
#
# ********************************************************************************************************/

# install.packages(c("survival", "survminer", "ggsurvfit"))

library(tidyverse)
library(survival)
library(survminer)
library(ggsurvfit)
library(broom)

get_data("HEART")



