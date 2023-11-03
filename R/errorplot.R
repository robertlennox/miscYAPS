#' functions to get receiver and tagging metadata into session
#'
#' @name errorplot
#' @import data.table
#' @import magrittr
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import yaps
#' @import purrr
#' @import gridExtra
#' @param hydros the hydrophone data frame as a data.table
#' @param sync_model is your preferred synchronisation model
#' @export

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

fixed=NULL

errorplot<-function(hydros=hydros, sync_model=sync_model){
  sync_model$eps_long %>%
    left_join(hydros %>% dplyr::select(x, y, hydro_idx=idx, serial)) %>%
    left_join(hydros %>% dplyr::select(sx=x, sy=y, sync_tag_idx=idx, sync_tag)) %>%
    ggplot(aes(ping, E_m, colour=factor(serial)))+
    geom_point()+
    theme_classic()+
    geom_smooth(se=F)+
    ggplot2::theme(text=element_text(size=25), axis.text=element_text(colour="black"), legend.position="top")+
    ggplot2::labs(x="Ping", y="Error (m)")
}
