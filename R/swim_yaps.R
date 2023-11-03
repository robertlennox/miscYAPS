#'
#' functions to get receiver and tagging metadata into session
#'
#' @name swim_yaps
#'
#' @import dplyr
#' @import beepr
#' @import purrr
#' @param fish_detections the data frame with the fish detections including epo and frac
#' @param rbi_min the minimum random burst interval of the transmitter
#' @param rbi_max the maximum random burst interval of the transmitter
#' @param runs number of times to refit the model
#' @export


swim_yaps<-function(fish_detections, runs, rbi_min, rbi_max){
tr<-fish_detections  %>%
  dplyr::count(dt=date(ts), tag) %>%
  dplyr::filter(n>50) %>%
  dplyr::mutate(i=c(1:nrow(.))) %>%
  dplyr::mutate(tag=factor(tag)) %>%
  split(.$i) %>%
  purrr::map(., ~yaps_all(transmitter_ID=.$tag, date=.$dt, runs=runs, rbi_min, rbi_max)) %>%
  purrr::discard(is.na(.)) %>%
  dplyr::bind_rows()

beepr::beep(8)
return(tr)}
