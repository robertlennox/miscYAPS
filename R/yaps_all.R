#'
#' functions to get receiver and tagging metadata into session
#'
#' @name yaps_all
#' @import data.table
#' @import magrittr
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import yaps
#' @import purrr

#' @param fish_detections the dataframe with the fish detections including epo and frac
#' @param hydros the dataframe with the hydrophones and sync tags set up as per yaps
#' @param sync_model the sync model you generated with yaps
#' @param rbi_min the minimum random burst interval of the transmitter
#' @param rbi_max the maximum random burst interval of the transmitter
#' @param transmitter_ID the ID of the transmitter you wanted to position
#' @param date the date that you wanted to position the tag on
#' @param runs number of times to refit the model
#' @export

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


yaps_all<-function(fish_detections, hydros, sync_model, rbi_min, rbi_max,
                   transmitter_ID, date, runs){
  tryCatch({

    dat <- yaps::applySync(toa=fish_detections %>% # sync model for one download
                             dplyr::filter(lubridate::date(.data$ts)==date) %>%
                             droplevels() %>%
                             data.table::setDT %>%
                             split(.$tag) %>%
                             purrr::pluck(transmitter_ID),
                           hydros=hydros,
                           sync_model=sync_model)

    hydros_yaps <- data.table::data.table(sync_model$pl$TRUE_H)
    colnames(hydros_yaps) <- c('hx','hy','hz')

    toa <- yaps::getToaYaps(synced_dat=dat,
                            hydros=hydros_yaps,
                            pingType='rbi',
                            rbi_min=rbi_min,
                            rbi_max=rbi_max)

    ####
    # YAPS
    ##

    magicYAPS<-function(x){
      tryCatch({yaps::runYaps(
        yaps::getInp(hydros_yaps,
                     toa,
                     E_dist="Mixture",
                     n_ss=2,
                     pingType="rbi",
                     sdInits=1,
                     rbi_min=rbi_min,
                     rbi_max=rbi_max,
                     ss_data_what="est",
                     bbox=NULL),
        silent=F,
        tmb_smartsearch=TRUE,
        maxIter=5000)},
        error=function(e){NA})}

    YAPS_list<-runs %>%
      purrr::rerun(magicYAPS())

    magic<-YAPS_list %>%  # the magic number
      purrr::map(purrr::pluck(4)) %>% # get the AIC cols
      purrr::map(purrr::pluck(1)) %>% # take the number
      bind_cols() %>% # make a df
      t() %>% # oops wrong order
      tidyr::as_tibble %>% # obv
      dplyr::filter(.data$V1==min(.data$V1)) %>% # get the min val
      as.numeric# make it a number

    final_track<-YAPS_list %>%
      purrr::discard(., ~any(is.na(.x))) %>%
      purrr::compact() %>%
      purrr::keep(., purrr::as_mapper(~.x$obj %>% # keep only the run with the lowest AIC
                                        purrr::pluck(1) == magic)) %>%
      purrr::pluck(1) %>%
      purrr::pluck(8) %>%
      tidyr::as_tibble %>%
      dplyr::mutate(dat %>% dplyr::distinct(.data$tag))},
    error=function(e){NA})
}
