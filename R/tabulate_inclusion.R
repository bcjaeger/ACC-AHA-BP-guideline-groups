##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data_derived
tabulate_inclusion <- function(data_derived) {

  data_derived %>%
    group_by(exam) %>%
    nest() %>%
    ungroup() %>%
    add_row(exam = 'Overall', data = list(data_derived), .before = 1) %>%
    mutate(included = map(data, include_exclude),
           included = map(included, 'tbl')) %>%
    unnest(included) %>%
    select(exam, label, sample_size) %>%
    pivot_wider(values_from = sample_size, names_from = exam)

}
