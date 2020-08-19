##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param design
##' @param variables
tabulate_characteristics <- function(design, variables, decimals) {

  tb1_ovrl <- variables %>%
    map(tb1_fun, design = design, decimals = decimals) %>%
    bind_rows(.id = 'label')

  tb1_diab <- variables %>%
    map(.f = tb1_fun,
        design = subset(design, diabetes == 'yes'),
        decimals = decimals) %>%
    bind_rows(.id = 'label')

  tb1_ckd <- variables %>%
    map(tb1_fun,
        design = subset(design, ckd == 'yes'),
        decimals = decimals) %>%
    bind_rows(.id = 'label')

  # tb1_diab_and_ckd <- variables %>%
  #   map(tb1_fun,
  #       design = subset(design, diabetes_and_ckd == 'yes'),
  #       decimals = decimals) %>%
  #   bind_rows(.id = 'label')

  tb1_age <- variables %>%
    map(tb1_fun,
        design = subset(design, age >= 65),
        decimals = decimals) %>%
    bind_rows(.id = 'label')

  tb1_any <- variables %>%
    map(tb1_fun,
        design = subset(design, any_ckd_diab_age65 == "yes"),
        decimals = decimals) %>%
    bind_rows(.id = 'label')

  tb1 <- bind_rows(
    Overall = tb1_ovrl,
    Diabetes = tb1_diab,
    'CKD' = tb1_ckd,
    'Age 65+ years' = tb1_age,
    'Diabetes, CKD, or age 65+ years' = tb1_any,
    .id = 'name'
  ) %>%
    pivot_wider(names_from = name, values_from = value) %>%
    add_count(label) %>%
    split(.$label) %>%
    map_dfr(~if(nrow(.x)==2){.x[-1, ]} else {.x}) %>%
    mutate(label = factor(label, levels = names(variables))) %>%
    arrange(label) %>%
    mutate(
      label = as.character(label),
      level = if_else(ctns | level == 'yes', label, level),
      label = if_else(n > 2, label, NA_character_)
    ) %>%
    select(-ctns, -n)

}


tb1_fun <- function(.x, by = NULL, design, decimals){

  do_by <- !is.null(by)

  if(do_by){
    by_formula <- as.formula(glue("~ {by}"))
  } else {
    by_formula <- NULL
  }

  if(is.numeric(design$variables[[.x]])){

    formula <- as.formula(glue("~ {.x}"))

    init <- if(do_by){

      svyby(formula, by = by_formula, design = design, FUN = svymean) %>%
        as_tibble() %>%
        mutate(level = .x) %>%
        rename_at(.vars = .x, .funs = ~ 'mn')

    } else {

      svymean(x = formula, design = design) %>%
        as_tibble() %>%
        rename_at(.vars = .x, .funs = ~ 'se') %>%
        mutate(level = .x) %>%
        rename(mn = mean)

    }

    init %>%
      mutate(
        value = tbl_string("{mn} ({se})", decimals = decimals),
        ctns = TRUE
      ) %>%
      select(-mn, -se) %>%
      ungroup() %>%
      mutate_if(is.factor, as.character)

  } else {

    formula <- if(do_by){
      as.formula(glue('~ {.x} + {by}'))
    } else {
      as.formula(glue("~ {.x}"))
    }

    init <- svytable(formula = formula, design = design) %>%
      as_tibble()

    if(do_by) init <- init %>%
      group_by_at(.vars = by)

    init %>%
      mutate(
        value = n / sum(n),
        value = tbl_string("{100 * value}", decimals = decimals),
        ctns = FALSE
      ) %>%
      rename_at(.vars = .x, .funs = ~ 'level') %>%
      select(-n) %>%
      ungroup() %>%
      mutate_if(is.factor, as.character)

  }

}
