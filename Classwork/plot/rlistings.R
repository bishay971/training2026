library(tidyverse)
library(clinicalfd)
library(rlistings)

data("adae") 

adae$newcols <- "XXXXXXXXXX"

#tidyverse approach
adae |> 
  select(newcols, usubjid, trta, age, sex, race, aedecod, aesev, astdt, aendt) 


listing <- as_listing(
  df = adae, 
  disp_cols = c("newcols", "usubjid", "trta", "age", "sex", "race", "aedecod", "aesev"), 
  key_cols = c("usubjid"), 
  main_title = c("Main Title"), 
  subtitles = c("Subtitle ABC"),
  main_footer = "Main footer",
  prov_footer = "Provenance Footer ABC"
)

# ASR, ADT

adae |> 
  mutate(
    asr = paste(age, sex, race, sep = "/"), 
    astdt =format(astdt, format = "%d%b%Y %H:%M")
  ) |> 
  select(usubjid, trta, asr, astdt) |> 
  head()



