#' functions to get receiver and tagging metadata into session
#'
#' @name sync
#' @import data.table
#' @import magrittr
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import yaps
#' @import purrr
#' @param hydros the hydrophone data frame as a data.table
#' @param detections a data.table of the sync tag detections
#' @param ss_data a data.table of the speed of sound data formatted using yaps::tempToSs
#' @param HOW_THIN is the number for the eps_threshold.. smaller makes a thinner distribution default is 50
#' @param keep_rate what proportion of sync tag detections do you want to retain, default is 1 (100%)
#' @export

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


sync<-function(hydros, detections, ss_data, HOW_THIN=50, keep_rate=1, ss_data_what="data",
               exclude_self_detections=T, fixed=c(1:nrow(.))){

  require(data.table)
  max_epo_diff=250
  min_hydros=2
  time_keeper_idx=1
  fixed_hydros_idx=fixed
  n_offset_day=2
  n_ss_day=2
  keep_rate=keep_rate
  excl_self_detect=exclude_self_detections
  ss_data_what=ss_data_what

  syncl<-list(hydros=hydros %>%
                setDT,
              detections=detections %>%
                setDT)

  inp_sync <- NULL
  inp_sync <- getInpSync(syncl,
                         max_epo_diff,
                         min_hydros,
                         time_keeper_idx,
                         fixed_hydros_idx,
                         n_offset_day,
                         n_ss_day,
                         ss_data=ss_data,
                         keep_rate=keep_rate,
                         excl_self_detect=excl_self_detect,
                         silent_check=F,
                         ss_data_what=ss_data_what)

  sync_model_0 <- sync_model_1 <- sync_model_2 <- sync_model_3 <- sync_model_4 <- NULL
  sync_model_0 <- getSyncModel(inp_sync, silent=TRUE, max_iter=1000, tmb_smartsearch = TRUE)
  sync_model <- fineTuneSyncModel(sync_model_0, eps_threshold=HOW_THIN, silent=TRUE)
}
