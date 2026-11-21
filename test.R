
library(tidyverse)
library(haven)

# raw_ae <- pharmaverseraw::ae_raw |> janitor::clean_names()
# raw_dm <- pharmaverseraw::dm_raw |> janitor::clean_names()
# raw_ds <- pharmaverseraw::ds_raw |> janitor::clean_names()
# raw_ec <- pharmaverseraw::ec_raw |> janitor::clean_names()
# raw_vs <- pharmaverseraw::vs_raw |> janitor::clean_names()
# 
# 
# write_sas(raw_ae, "20_batch_plot/raw_data/raw_ae.sas7bdat")
# write_sas(raw_dm, "20_batch_plot/raw_data/raw_dm.sas7bdat")
# write_sas(raw_ds, "20_batch_plot/raw_data/raw_ds.sas7bdat")
# write_sas(raw_ec, "20_batch_plot/raw_data/raw_ec.sas7bdat")
# write_sas(raw_vs, "20_batch_plot/raw_data/raw_vs.sas7bdat")


# read_sas("raw_data/raw_ae.sas7bdat")
# read_sas("raw_data/raw_dm.sas7bdat")
# read_sas("raw_data/raw_ds.sas7bdat")
# read_sas("raw_data/raw_ec.sas7bdat")
# read_sas("raw_data/raw_vs.sas7bdat")




dm <- pharmaversesdtm::dm 
ds <- pharmaversesdtm::ds 
ex <- pharmaversesdtm::ex 
ae <- pharmaversesdtm::ae 
vs <- pharmaversesdtm::vs


save(dm, ds, ex, ae, vs, file= "20_batch_plot/raw_data/operational.rda")


load("20_batch_plot/raw_data/operational.rda")
















