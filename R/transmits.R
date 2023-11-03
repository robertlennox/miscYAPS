#' functions to get receiver and tagging metadata into session
#'
#' @name transmits
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
#' @param transmits is a number of transmissions to plot
#' @param map is a ggplot object showing your study site boundaries
#' @export

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

fixed=NULL

transmits<-function(hydros=hydros, sync_model=sync_model, transmits=20, map) {
  i<-sync_model$eps_long %>% dplyr::summarise(m=max(ping)) %>%
    pluck(1)
  n<-sample(c(1:i), 1)
  map+
    ggplot2::geom_segment(data=sync_model$eps_long %>%
                            dplyr::filter(ping==n) %>%
                            dplyr::left_join(hydros %>% dplyr::select(x, y, hydro_idx=idx)) %>%
                            dplyr::left_join(hydros %>% dplyr::select(sx=x, sy=y, sync_tag_idx=idx)) %>%
                            dplyr::slice_sample(n=transmits),
                          aes(x=sx, xend=x, y=sy, yend=y, colour=E), size=4, inherit.aes=F)+
    ggplot2::theme_classic()+
    ggplot2::theme(legend.position = "top", legend.key.width=unit(3, "cm"))+
    ggplot2::scale_colour_gradientn(colours=c("purple", "yellow", "green", "yellow", "purple"))+
    ggplot2::geom_text(data=hydros, aes(x, y, label=serial), inherit.aes=F)+
    ggplot2::theme(text=element_text(size=25), axis.text=element_text(colour="black"))+
    labs(x="UTM (x)", y="UTM (y)")+
    ggplot2::facet_wrap(~paste("ping", ping, "out of", i))
}
