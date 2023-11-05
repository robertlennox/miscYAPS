#' functions to get receiver and tagging metadata into session
#'
#' @name syncgrid
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

syncgrid<-function(hydros=hydros, sync_model=sync_model){
  sync_model %>%
    pluck("eps_long") %>%
    arrange(ping) %>%
    dplyr::left_join(hydros %>% dplyr::select(x, y, hydro_idx=idx, serial)) %>%
    dplyr::left_join(hydros %>% dplyr::select(sx=x, sy=y, sync_tag_idx=idx, sync_tag)) %>%
    dplyr::mutate(i=(x-sx)^2) %>%
    dplyr::mutate(j=(y-sy)^2) %>%
    dplyr::mutate(dist=sqrt(i+j)) %>%
    dplyr::group_by(serial, sync_tag, dist, x, y,sx, sy) %>%
    dplyr::summarise(E=mean(E)) %>%
    ggplot2::ggplot(aes(sync_tag %>% factor, serial %>% factor, fill=E, label=paste(round(dist), "metres apart")))+
    ggplot2::geom_tile()+
    ggplot2::scale_fill_gradientn(colours=c("purple", "yellow", "green", "yellow", "purple"))+
    ggplot2::theme_classic()+
    ggplot2::geom_text()+
    ggplot2::theme(legend.position="top", legend.key.width=unit(3, "cm"))+
    ggplot2::labs(x="Sync Tag", y="Serial", fill="Mean time to detect (s)")
}
