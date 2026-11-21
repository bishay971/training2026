# *******************************************************************************************************
# Program Name:                 meta.R
# Project:                      CDISC
# Purpose:                      Create Meta of ADAM
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


library(metacore)
library(metatools)
library(tidyverse)
library(admiral)
library(haven)
library(xportr)

rsetup::get_data("sdtm_demo")
