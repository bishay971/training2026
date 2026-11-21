# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2026. Unauthorized distribution or reuse prohibited.

library(tidyverse)
rsetup::get_data("vs")


visit_fmt <- c(
  "SCREENING" = "Screening",
  "Screening" = "Screening",
  "Day 1" = "Day 1",
  "Day 2" = "Day 2",
  "Day 3" =   "Day 3",
  "Day 4" =   "Day 4",
  "Day 5" =  "Day 5", 
  "WEEK 6" = "Week 6"
)

visitn_fmt <- c(
  "Screening" = 1,
  "Day 1" = 2,
  "Day 2" = 3,
  "Day 3" = 4,
  "Day 4" = 5,
  "Day 5" = 6,
  "Week 6" = 7
)

# Exercise 1: Use raw VS datsets and append them to create: 
# USUBJID, VSTESTCD, VSTEST, VSORRES, VSSPID, VSDTC, VISIT, VISITNUM, VSTPT, VSTPTNUM.

vs1 <- raw_vs |> 
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
    ), 
    VSTEST = case_when(
      VSTESTCD == "PULSE" ~ "Pulse Rate", 
      VSTESTCD == "RESP" ~ "Respiration",
      VSTESTCD ==  "SYSBP" ~ "Systolic Blood Pressure",
      VSTESTCD ==  "DIABP" ~ "Diastolic Blood Pressure",
      VSTESTCD ==  "TEMP" ~ "Temperature"
    ), 
    VSORRESU = case_when(
      VSTESTCD == "PULSE" ~ "bpm", 
      VSTESTCD == "RESP" ~ "breaths/min",
      VSTESTCD ==  "SYSBP" ~ "mmHg",
      VSTESTCD ==  "DIABP" ~ "mmHg",
      VSTESTCD ==  "TEMP" ~ "C"
    ), 
    USUBJID = paste0("TRA-025", "-", SUBJECT),
    VSDTC = if_else(VSTM != "", paste0(VSDT, "T", VSTM), as.character(VSDT)), 
    VISIT = str_to_title(FOLDERNAME), 
    VISITNUM = if_else(VISIT == "Screening", 1, 7)
  ) |> 
  select(
    USUBJID, VSTEST, VSTESTCD, VSORRES, VSORRESU, VSDTC, VISIT, VISITNUM
  )

vs2 <- raw_vs1 |> 
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
    ), 
    VSTEST = case_when(
      VSTESTCD == "PULSE" ~ "Pulse Rate", 
      VSTESTCD == "RESP" ~ "Respiration",
      VSTESTCD ==  "SYSBP" ~ "Systolic Blood Pressure",
      VSTESTCD ==  "DIABP" ~ "Diastolic Blood Pressure",
      VSTESTCD ==  "TEMP" ~ "Temperature"
    ), 
    VSORRESU = case_when(
      VSTESTCD == "PULSE" ~ "bpm", 
      VSTESTCD == "RESP" ~ "breaths/min",
      VSTESTCD ==  "SYSBP" ~ "mmHg",
      VSTESTCD ==  "DIABP" ~ "mmHg",
      VSTESTCD ==  "TEMP" ~ "C"
    ), 
    USUBJID = paste0("TRA-025", "-", SUBJECT),
    VSDTC = if_else(VS1TM != "", paste0(VS1DT, "T", VS1TM), as.character(VS1DT)), 
    VISIT = visit_fmt[FOLDERNAME], 
    VISITNUM = visitn_fmt[FOLDERNAME] |> as.numeric(), 
    
    VSTPT = VS1TPT, 
    VSTPTNUM = VS1TPT_STD
  ) |> 
  select(
    USUBJID, VSTEST, VSTESTCD, VSORRES, VSORRESU, VSDTC, VISIT, VISITNUM, VSTPT, VSTPTNUM
  )

# 5 mins for exploring raw_vs2 and create SDTM.VS structure like exercise 1 and 2.


vs3 <- raw_vs2 |>
  pivot_longer(
    cols = c(VS2WGT, VS2HGT),
    names_to = "VSTESTCD",
    values_to = "VSORRES"
  ) |>
  mutate(
    VSTESTCD = case_when(
      VSTESTCD == "VS2WGT" ~ "WEIGHT",
      VSTESTCD == "VS2HGT" ~ "HEIGHT"
    ),
    VSTEST = case_when(
      VSTESTCD == "WEIGHT" ~ "Weight",
      VSTESTCD == "HEIGHT" ~ "Height"
    ),
    VSORRESU = case_when(
      VSTESTCD == "WEIGHT" ~ "kg",
      VSTESTCD == "HEIGHT" ~ "cm"
    ),
    USUBJID = paste0("TRA-025", "-", SUBJECT),
    VSDTC = as.character(VS2DT),
    VISIT = visit_fmt[FOLDERNAME], 
    VISITNUM = visitn_fmt[FOLDERNAME] |> as.numeric()
  ) |> 
  select(
    USUBJID, VSTEST, VSTESTCD, VSORRES, VSORRESU, VSDTC, VISIT, VISITNUM
  )


vs4 <- bind_rows(vs1, vs2, vs3)

# Exercise 2: Write the derivation of VSBLFL (Baseline Flag) using the datasets form exercise 1.

vs_dm <- vs4 |> 
  left_join(
    select(dm, USUBJID, RFXSTDTC),
    by = "USUBJID"
  )


#
baseline <- vs_dm |> 
  filter(
    VSDTC <= RFXSTDTC, 
    !is.na(VSORRES) & VSORRES != "", 
    !is.na(VSDTC) & VSDTC != ""
  ) |> 
  group_by(USUBJID, VSTESTCD) |>
  filter(VISITNUM == max(VISITNUM)) |> 
  mutate(VSBLFL = "Y") |> 
  ungroup() |> 
  select(USUBJID, VSTESTCD, VISITNUM, VSBLFL, VSDTC) 


vs_blfl <- vs_dm |> 
  left_join(
    baseline, by = c("USUBJID", "VSTESTCD", "VISITNUM", "VSDTC")
  )

# Exercise 3: Derive VSSTAT using the dataset.

vs_stat <- vs_blfl |> 
  mutate(
    VSSTAT = if_else(
      is.na(VSORRES) | VSORRES == "", "NOT DONE", ""
    )
  )

# Exercise 4: Derive VSSEQ and study day variable.



# --DY = (date portion of --DTC) - (date portion of RFSTDTC) + 1 if --DTC is on or after RFSTDTC
# --DY = (date portion of --DTC) - (date portion of RFSTDTC) if --DTC precedes RFSTDTC

cal_days <- function(date, ref_date) {
  case_when(
    as.Date(date) >= as.Date(ref_date) ~ as.numeric(as.Date(date) - as.Date(ref_date)) + 1,
    TRUE ~ as.numeric(as.Date(date) - as.Date(ref_date))
  )
}


vs_data <- vs_stat |> 
  mutate(
    STUDYID = "TRA-025",
    DOMAIN = "VS",
    VSDY = cal_days(VSDTC, RFXSTDTC)
  ) |> 
  arrange(STUDYID, USUBJID, VSTESTCD, VSDTC) |> 
  group_by(USUBJID) |> 
  mutate(VSSEQ = row_number()) |> 
  ungroup() #|> 
# select(
#   STUDYID,
#   DOMAIN,
#   USUBJID,
#   VSSEQ,
#   VSTESTCD,
#   VSTEST,
#   VSORRES,
#   VSTPT,
#   VSTPTNUM,
#   VSSTRESC,
#   VSSTRESN,
#   VSORRESU,
#   VSSTRESU,
#   VSSTAT,
#   VISITNUM,
#   VISIT,
#   VSDTC,
#   VSBLFL,
#   VSDY,
# )



