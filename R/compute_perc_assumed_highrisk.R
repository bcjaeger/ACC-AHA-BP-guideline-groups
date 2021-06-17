#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author bcjaeger
#' @export
compute_perc_assumed_highrisk <- function(design) {

  lowcvdrisk_props <-
    svytable(~any_ckd_diab_age65, design) %>%
    as.data.frame() %>%
    mutate(p = Freq / sum(Freq))

  lowcvdrisk_props %>%
    filter(any_ckd_diab_age65 == 'yes') %>%
    mutate(perc = p * 100) %>%
    pull(perc)

}
