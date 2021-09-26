#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author bcjaeger
#' @export
clean_bp_meds <- function() {

  has_label <- function(x) !is.null(attr(x, 'label'))

  list.files('data_htn_meds', full.names = TRUE) |>
    set_names(c('2013-2014',
                '2015-2016',
                '2017-2017')) |>
    map(read_sas) |>
    map(select, where(has_label)) |>
    bind_rows(.id = 'exam') |>
    rowwise() |>
    mutate(meds_n_bp_classes = sum(TD, ACEI, ARBs, CCB,
                                   LOOP, KSPARING, ALDO,
                                   BB, DRI, AB, CA, DV),
           .before = TD) %>%
    rename(seqn = SEQN)

}
