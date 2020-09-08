##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param design
tabulate_bpdist <- function(design, decimals) {

  fix_names <- function(.x, first_name = 'bp_cat'){
    if(ncol(.x) == 3){
      set_names(.x, c(first_name, 'level', 'n'))
    } else {
      set_names(.x, c(first_name, 'n'))
    }
  }

  list(
    overall = ~ bp_cat,
    diabetes = ~ bp_cat + diabetes,
    ckd = ~ bp_cat + ckd,
    # diabetes_and_ckd = ~ bp_cat + diabetes_and_ckd,
    age_group = ~ bp_cat + age_gt65,
    any = ~ bp_cat + any_ckd_diab_age65
  ) %>%
    map(svytable, design = design) %>%
    map(as_tibble) %>%
    map(fix_names, first_name = 'bp_cat') %>%
    bind_rows(.id = 'variable') %>%
    filter(level == 'yes' | is.na(level)) %>%
    select(-level) %>%
    group_by(variable) %>%
    mutate(n = table_value(100 * n / sum(n)))

}
