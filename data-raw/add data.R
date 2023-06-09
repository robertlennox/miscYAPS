setwd("C:/Users/rb501745/OneDrive - Dalhousie University/miscYAPS/data-raw")

dets<-readRDS("aurland-dets.RDS")
usethis::use_data(dets)
