## code to prepare `DATASET` dataset goes here


setwd("C:/Users/rb501745/OneDrive - Dalhousie University/miscYAPS/data-raw")

aur<-readRDS("aurland-dets.RDS")
usethis::use_data(aur, overwrite = TRUE)

boats<-readRDS("ws.RDS")
usethis::use_data(boats, overwrite = TRUE)
