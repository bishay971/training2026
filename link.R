# 
# 
# 
# txt <- as.data.frame(paste0("https://t25.netlify.app/", list.files("zip")))
# 
# 
# writexl::write_xlsx(txt, "link.xlsx")
# 

library(googlesheets4)
library(dplyr)

gs4_deauth()

training_links <- 
  read_sheet("https://docs.google.com/spreadsheets/d/1pyxgtlvdcukg0cJZyus3T6iBah-RYRFF-XOLUixSdhI/edit?gid=1881388254#gid=1881388254") |> 
  select(link2, uid)

training_links

save(training_links, file = "/Volumes/Disk Bishwas/R Training/training2026/public/link.rda")
