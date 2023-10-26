#'
#' functions to get receiver and tagging metadata into session
#'
#' @name swim_yaps
#' @import data.table
#' @import magrittr
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import yaps
#' @import purrr

#' @param fish_detections the dataframe with the fish detections including epo and frac
#' @param rbi_min the minimum random burst interval of the transmitter
#' @param rbi_max the maximum random burst interval of the transmitter
#' @param runs number of times to refit the model, default is 5
#' @param silent do you want to hide all the TMB code running? Default is yes (TRUE)
#' @export

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


swim_yaps<-function(fish_detections, rbi_min, rbi_max, runs=5, silent=T){
  yaps_all<-function (fish_detections, hydros, sync_model,
                      transmitter_ID, date)
  {
    tryCatch({
      dat <- yaps::applySync(toa = fish_detections %>% dplyr::filter(lubridate::date(.data$ts) ==
                                                                       date) %>% droplevels() %>% data.table::setDT() %>%
                               split(.$tag) %>% purrr::pluck(transmitter_ID), hydros = hydros,
                             sync_model = sync_model)
      hydros_yaps <- data.table::data.table(sync_model$pl$TRUE_H)
      colnames(hydros_yaps) <- c("hx", "hy", "hz")
      toa <- yaps::getToaYaps(synced_dat = dat, hydros = hydros_yaps,
                              pingType = "rbi", rbi_min = rbi_min, rbi_max = rbi_max)
      magicYAPS <- function(x) {
        tryCatch({
          yaps::runYaps(yaps::getInp(hydros_yaps, toa,
                                     E_dist = "Mixture", n_ss = 2, pingType = "rbi",
                                     sdInits = 1, rbi_min = rbi_min, rbi_max = rbi_max,
                                     ss_data_what = "est", bbox = NULL), silent = silent,
                        tmb_smartsearch = TRUE, maxIter = 5000)
        }, error = function(e) {
          NA
        })
      }
      YAPS_list <- runs %>% purrr::rerun(magicYAPS())
      magic <- YAPS_list %>% purrr::map(purrr::pluck(4)) %>%
        purrr::map(purrr::pluck(1)) %>% bind_cols() %>% t() %>%
        tidyr::as_tibble %>% dplyr::filter(.data$V1 == min(.data$V1)) %>%
        as.numeric
      final_track <- YAPS_list %>% purrr::discard(., ~any(is.na(.x))) %>%
        purrr::compact() %>% purrr::keep(., purrr::as_mapper(~.x$obj %>%
                                                               purrr::pluck(1) == magic)) %>% purrr::pluck(1) %>%
        purrr::pluck(8) %>% tidyr::as_tibble %>% dplyr::mutate(dat %>%
                                                                 dplyr::distinct(.data$tag))
    }, error = function(e) {
      NA
    })
  }

  function (fish_detections, hydros, sync_model, rbi_min, rbi_max,
            transmitter_ID, date, runs)
  {
    tryCatch({
      dat <- yaps::applySync(toa = fish_detections %>% dplyr::filter(lubridate::date(.data$ts) ==
                                                                       date) %>% droplevels() %>% data.table::setDT() %>%
                               split(.$tag) %>% purrr::pluck(transmitter_ID), hydros = hydros,
                             sync_model = sync_model)
      hydros_yaps <- data.table::data.table(sync_model$pl$TRUE_H)
      colnames(hydros_yaps) <- c("hx", "hy", "hz")
      toa <- yaps::getToaYaps(synced_dat = dat, hydros = hydros_yaps,
                              pingType = "rbi", rbi_min = rbi_min, rbi_max = rbi_max)
      magicYAPS <- function(x) {
        tryCatch({
          yaps::runYaps(yaps::getInp(hydros_yaps, toa,
                                     E_dist = "Mixture", n_ss = 2, pingType = "rbi",
                                     sdInits = 1, rbi_min = rbi_min, rbi_max = rbi_max,
                                     ss_data_what = "est", bbox = NULL), silent = F,
                        tmb_smartsearch = TRUE, maxIter = 5000)
        }, error = function(e) {
          NA
        })
      }
      YAPS_list <- runs %>% purrr::rerun(magicYAPS())
      magic <- YAPS_list %>% purrr::map(purrr::pluck(4)) %>%
        purrr::map(purrr::pluck(1)) %>% bind_cols() %>% t() %>%
        tidyr::as_tibble %>% dplyr::filter(.data$V1 == min(.data$V1)) %>%
        as.numeric
      final_track <- YAPS_list %>% purrr::discard(., ~any(is.na(.x))) %>%
        purrr::compact() %>% purrr::keep(., purrr::as_mapper(~.x$obj %>%
                                                               purrr::pluck(1) == magic)) %>% purrr::pluck(1) %>%
        purrr::pluck(8) %>% tidyr::as_tibble %>% dplyr::mutate(dat %>%
                                                                 dplyr::distinct(.data$tag))
    }, error = function(e) {
      NA
    })
  }

  tr<-fish_detections %>%
    dplyr::count(dt=lubridate::date(ts), tag) %>%
    dplyr::filter(n>50) %>%
    dplyr::mutate(i=c(1:nrow(.))) %>%
    dplyr::mutate(tag=factor(tag)) %>%
    split(.$i) %>%
    purrr::map(., ~miscYAPS::yaps_all(transmitter_ID=.$tag, date=.$dt,
                                      runs=runs, rbi_min=rbi_min, rbi_max=rbi_max,
                                      fish_detections=fish_detections,
                                      hydros=hydros,
                                      sync_model=sync_model)) %>%
    purrr::discard(is.na(.)) %>%
    bind_rows()

  return(tr)
}
