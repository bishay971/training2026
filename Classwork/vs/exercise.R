# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2026. Unauthorized distribution or reuse prohibited.

library(tidyverse)
rsetup::get_data("vs")

# Exercise 1: Use raw VS datsets and append them to create: 
# USUBJID, VSTESTCD, VSTEST, VSORRES, VSSPID, VSDTC, VISIT, VISITNUM, VSTPT, VSTPTNUM.

# Screening 1
# DAY1 TO DAY 5, 6
# Week 6 , 7


ex1 <- raw_vs |> 
  pivot_longer(
    cols = c(VSHR, VSRESP, VSSYS, VSDIA, VSTEMP), 
    names_to = "VSTESTCD",
    values_to = "VSORRES"
  ) |> 
  mutate(
    VSTESTCD = case_when(
      VSTESTCD == "VSHR" ~ "PULSE", 
      VSTESTCD == "VSRESP" ~ "RESP", 
      VSTESTCD == "VSSYS" ~ "SYSBP", 
      VSTESTCD == "VSDIA" ~ "DIABP", 
      VSTESTCD == "VSTEMP" ~ "TEMP"
    )
  ) |> 
  mutate(
    VSTEST = case_when(
      VSTESTCD == "PULSE" ~ "Pulse Rate", 
      VSTESTCD == "RESP" ~ "Respiration", 
      VSTESTCD == "SYSBP" ~ "Systolic Blood Pressure", 
      VSTESTCD == "DIABP" ~ "Diastolic Blood Pressure", 
      VSTESTCD == "TEMP" ~ "Temperature"
    ), 
    
    VSORRESU = case_when(
      VSTESTCD == "PULSE" ~ "bpm", 
      VSTESTCD == "RESP" ~ "breaths/min", 
      VSTESTCD == "SYSBP" ~ "mmHg", 
      VSTESTCD == "DIABP" ~ "mmHg", 
      VSTESTCD == "TEMP" ~ "C"
    ), 
    USUBJID = paste("TRA-025", SUBJECT, sep = "-"),
    VSDTC = VSDT |> as.character(),
    VISIT = str_to_title(FOLDERNAME),
    VISITNUM = case_when(
      VISIT == "Screening" ~ 1, 
      VISIT == "Week 6" ~ 7, 
    )
  ) |> 
  select(USUBJID, VSTESTCD, VSTEST, VSORRES, VSDTC, VISIT, VISITNUM)


visit_fmt <- c(
  "SCREENING" = "Screening" , 
  "WEEK 6" = "Week 6",  
  "Day 1" = "Day 1", 
  "Day 2"= "Day 2", 
  "Day 3"=  "Day 3", 
  "Day 4"=  "Day 4", 
  "Day 5"=  "Day 5",  
  "Screening"= "Screening"
)

visitn_fmt <- c(
  "SCREENING" = "1", 
  "Day 1"= "2",
  "Day 2"= "3",
  "Day 3"= "4",
  "Day 4"= "5",
  "Day 5"= "6",
  "WEEK 6"= "7"
)

ex2 <- raw_vs1 |> 
  pivot_longer(
    cols = c(VS1HR, VS1RESP, VS1SYS, VS1DIA, VS1TEMP), 
    names_to = "VSTESTCD",
    values_to = "VSORRES"
  ) |> 
  mutate(
    VSTESTCD = case_when(
      VSTESTCD == "VS1HR" ~ "PULSE", 
      VSTESTCD == "VS1RESP" ~ "RESP", 
      VSTESTCD == "VS1SYS" ~ "SYSBP", 
      VSTESTCD == "VS1DIA" ~ "DIABP", 
      VSTESTCD == "VS1TEMP" ~ "TEMP"
    )
  ) |> 
  mutate(
    VSTEST = case_when(
      VSTESTCD == "PULSE" ~ "Pulse Rate", 
      VSTESTCD == "RESP" ~ "Respiration", 
      VSTESTCD == "SYSBP" ~ "Systolic Blood Pressure", 
      VSTESTCD == "DIABP" ~ "Diastolic Blood Pressure", 
      VSTESTCD == "TEMP" ~ "Temperature"
    ), 
    
    VSORRESU = case_when(
      VSTESTCD == "PULSE" ~ "bpm", 
      VSTESTCD == "RESP" ~ "breaths/min", 
      VSTESTCD == "SYSBP" ~ "mmHg", 
      VSTESTCD == "DIABP" ~ "mmHg", 
      VSTESTCD == "TEMP" ~ "C"
    ), 
    USUBJID = paste("TRA-025", SUBJECT, sep = "-"),
    VSDTC = if_else(VS1TM == "", as.character(VS1DT), 
                    paste(VS1DT, VS1TM, sep = "T")   
    ),
    VISIT = visit_fmt[FOLDERNAME],
    VISITNUM = visitn_fmt[FOLDERNAME] |> as.numeric(),
    VSTPT = VS1TPT, 
    VSTPTNUM = VS1TPT_STD
  ) |> 
  select(USUBJID, VSTESTCD, VSTEST, VSORRES, VSDTC, VSTPT, VSTPTNUM, VISIT, VISITNUM)

ex3 <- raw_vs2 |> 
  pivot_longer(
    cols = c(VS2HGT, VS2WGT), 
    names_to = "VSTESTCD",
    values_to = "VSORRES"
  ) |> 
  mutate(
    VSTESTCD = case_when(
      VSTESTCD == "VS2HGT" ~ "HEIGHT", 
      VSTESTCD == "VS2WGT" ~ "WEIGHT"
    )
  ) |> 
  mutate(
    VSTEST = case_when(
      VSTESTCD == "HEIGHT" ~ "Height", 
      VSTESTCD == "WEIGHT" ~ "Weight"
    ), 
    
    VSORRESU = case_when(
      VSTESTCD == "HEIGHT" ~ "cm", 
      VSTESTCD == "WEIGHT" ~ "kg"
    ), 
    USUBJID = paste("TRA-025", SUBJECT, sep = "-"),
    VSDTC = paste(VS2DT),
    VISIT = visit_fmt[FOLDERNAME],
    VISITNUM = visitn_fmt[FOLDERNAME] |> as.numeric(),
  ) |> 
  select(USUBJID, VSTESTCD, VSTEST, VSORRES,VSORRESU,  VSDTC, VISIT, VISITNUM)



# USUBJID, VSTESTCD, VSTEST, VSORRES, VSSPID, VSDTC, VISIT, VISITNUM, VSTPT, VSTPTNUM.

vs_data <- bind_rows(ex1 ,ex2, ex3)

# Exercise 2: Write the derivation of VSBLFL (Baseline Flag) using the datasets form exercise 1.

dm_vs <- vs_data |> 
  left_join(
    select(dm, USUBJID, RFXSTDTC), 
    by = "USUBJID"
  )

baseline <- dm_vs |> 
  filter(
    VSDTC <= RFXSTDTC, 
    !is.na(VSORRES) | VSORRES == "",
    !is.na(VSDTC) | VSDTC == ""
  ) |>
  group_by(USUBJID, VSTESTCD) |> 
  filter(VISITNUM == max(VISITNUM)) |>
  mutate(VSBLFL = "Y") |> 
  ungroup() |> 
  select(USUBJID, VSTESTCD, VSDTC, VISITNUM, VSBLFL)

# Exercise 3: Derive VSSTAT using the dataset.

ex3 <- vs_data |> 
  mutate(
    VSSTAT = if_else(VSORRES == "" | is.na(VSORRES), "NOT DONE", "")
  )

# Exercise 4: Derive VSSEQ and study day variable.