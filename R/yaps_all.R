#'
#' functions to get receiver and tagging metadata into session
#'
#' @name swim_yaps
#' @name yaps_all
#' @import data.table
#' @import magrittr
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import yaps
#' @import purrr
#' @param fish_detections the data frame with the fish detections including epo and frac
#' @param rbi_min the minimum random burst interval of the transmitter
#' @param rbi_max the maximum random burst interval of the transmitter
#' @param runs number of times to refit the model, default is 5
#' @param silent do you want to hide all the TMB code running? Default is yes (TRUE)
#' @export

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


yaps_all<-function(transmitter_ID, date, runs, rbi_min, rbi_max, silent=T){
  tryCatch({
    dat <- applySync(toa=fish_detections %>% # sync model for one download
                       dplyr::filter(date(ts)==date) %>%
                       droplevels() %>%
                       setDT %>%
                       split(.$tag) %>%
                       pluck(transmitter_ID),
                     hydros=hydros,
                     sync_model=sync_model)
    hydros_yaps <- data.table::data.table(sync_model$pl$TRUE_H)
    colnames(hydros_yaps) <- c('hx','hy','hz')
    rbi_min <- rbi_min
    rbi_max <- rbi_max
    toa <- getToaYaps(synced_dat=dat,
                      hydros=hydros_yaps,
                      pingType='rbi',
                      rbi_min=rbi_min,
                      rbi_max=rbi_max)
    nobs <- apply(toa, 1, function(k) sum(!is.na(k)))
    ####
    # YAPS
    ##
    magicYAPS<-function(x){
      tryCatch({runYaps(
        getInp(hydros_yaps,
               toa,
               E_dist="Mixture",
               n_ss=2,
               pingType="rbi",
               sdInits=1,
               rbi_min=rbi_min,
               rbi_max=rbi_max,
               ss_data_what="est",
               bbox=NULL),
        silent=silent,
        tmb_smartsearch=TRUE,
        maxIter=5000)},
        error=function(e){NA})}
    YAPS_list<-runs %>%
      rerun(magicYAPS())
    magic<-YAPS_list %>%  # the magic number
      purrr::map(purrr::pluck(4)) %>% # get the AIC cols
      purrr::map(purrr::pluck(1)) %>% # take the number
      bind_cols() %>% # make a df
      t() %>% # oops wrong order
      as_tibble %>% # obv
      dplyr::filter(V1==min(V1)) %>% # get the min val
      as.numeric# make it a number
    final_track<-YAPS_list %>%
      purrr::discard(., ~any(is.na(.x))) %>%
      purrr::compact() %>%
      keep(., as_mapper(~.x$obj %>% # keep only the run with the lowest AIC
                          pluck(1) == magic)) %>%
      pluck(1) %>%
      pluck(8) %>%
      as_tibble %>%
      mutate(dat %>% distinct(tag))},
    error=function(e){NA})
}

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
