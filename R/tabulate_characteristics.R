##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param design
##' @param variables
tabulate_characteristics <- function(design, variables) {

  tb1_ovrl <- variables %>%
    map(tb1_fun, design = design) %>%
    bind_rows(.id = 'label')

  tb1_diab <- variables %>%
    map(.f = tb1_fun,
        design = subset(design, diabetes == 'yes')) %>%
    bind_rows(.id = 'label')

  tb1_ckd <- variables %>%
    map(tb1_fun,
        design = subset(design, ckd == 'yes')) %>%
    bind_rows(.id = 'label')

  tb1_age <- variables %>%
    map(tb1_fun,
        design = subset(design, age >= 65)) %>%
    bind_rows(.id = 'label')

  tb1_any <- variables %>%
    map(tb1_fun,
        design = subset(design, any_ckd_diab_age65 == "yes")) %>%
    bind_rows(.id = 'label')

  tb1_inline <- bind_rows(
    overall = tb1_ovrl,
    diabetes = tb1_diab,
    ckd = tb1_ckd,
    age_gt65 = tb1_age,
    any = tb1_any,
    .id = 'name'
  ) %>%
    mutate(variable = recode(label, !!!variables)) %>%
    select(name, variable, level, value) %>%
    as_inline(tbl_variables = c('name', 'variable', 'level'),
              tbl_value = 'value')


  tb1_data <- bind_rows(
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

  list(table = tb1_data,
       inline = tb1_inline)

}


tb1_fun <- function(.x, by = NULL, design){

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

      svymean(x = formula,
              design = design,
              na.rm = TRUE) %>%
        as_tibble() %>%
        rename_at(.vars = .x, .funs = ~ 'se') %>%
        mutate(level = .x) %>%
        rename(mn = mean)

    }

    init %>%
      mutate(
        value = table_glue("{mn} ({se})"),
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
        value = table_glue("{100 * value}"),
        ctns = FALSE
      ) %>%
      rename_at(.vars = .x, .funs = ~ 'level') %>%
      select(-n) %>%
      ungroup() %>%
      mutate_if(is.factor, as.character)

  }

}
