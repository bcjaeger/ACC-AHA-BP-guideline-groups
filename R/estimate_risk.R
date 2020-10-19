##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param design_overall
##' @param age_breaks
estimate_risk <- function(design, age_breaks) {

  design_bp <- list(
    ovrl = design,
    norm = subset(design, bp_cat == "Normal blood pressure"),
    elev = subset(design, bp_cat == "Elevated blood pressure"),
    stg1 = subset(design, bp_cat == "Stage 1 hypertension"),
    stg2 = subset(design, bp_cat == "Stage 2 hypertension")
  )

  variables <- c('diabetes', 'ckd', 'no_diabetes_or_ckd')

  enframe(design_bp, name = 'bp_cat', value = '.design') %>%
    mutate(.variable = list(variables)) %>%
    unnest(.variable) %>%
    mutate(
      bnry = map2(.variable, .design, pcr_model_bnry, age_breaks),
      ctns = map2(.variable, .design, pcr_model_ctns, age_breaks)
    )

}

pcr_model_ctns <- function(.variable, .design, age_breaks){

  data_grid <- tibble(age = age_breaks)

  if(.variable == 'Overall'){
    mdl_f <- ascvd_risk_pcr ~ age + I(age^2) + I(age^3) + I(age^4)
  } else {

    mdl_f <- as.formula(
      glue('ascvd_risk_pcr ~ (age + I(age^2) + I(age^3) + I(age^4)) * {.variable}')
    )

    data_grid[[.variable]] <- 'yes'

  }

  mdl_obj <- svyglm(mdl_f, design = .design)

  mdl_prd <- predict(mdl_obj, data_grid, interval = 'confidence')

  mdl_prd_ci <- confint(mdl_prd) %>%
    as_tibble() %>%
    set_names(c('lwr', 'upr'))

  as_tibble(mdl_prd) %>%
    bind_cols(mdl_prd_ci) %>%
    bind_cols(data_grid) %>%
    mutate(
      est = pmax(link, 0),
      lwr = pmax(lwr, 0),
      upr = pmax(upr, 0),
      est = pmin(est, 1),
      lwr = pmin(lwr, 1),
      upr = pmin(upr, 1)
    ) %>%
    select(age, est, lwr, upr)

}

pcr_model_bnry <- function(.variable, .design, age_breaks){

  data_grid <- tibble(age = age_breaks)

  if(.variable == 'Overall'){

    mdl_f <- pcr_highrisk ~ age

  } else {

    mdl_f <- as.formula(
      glue("pcr_highrisk ~ age * {.variable}")
    )

    data_grid[[.variable]] <- 'yes'

  }

  mdl_obj <- svyglm(mdl_f,
                    design = .design,
                    family = quasibinomial())

  mdl_prd <-
    predict(mdl_obj, data_grid, interval = 'confidence', type = 'response')

  mdl_prd_ci <- confint(mdl_prd) %>%
    as_tibble() %>%
    set_names(c('lwr', 'upr'))

  as_tibble(mdl_prd) %>%
    bind_cols(mdl_prd_ci) %>%
    bind_cols(data_grid) %>%
    mutate(
      est = pmax(response, 0),
      lwr = pmax(lwr, 0),
      upr = pmax(upr, 0),
      est = pmin(est, 1),
      lwr = pmin(lwr, 1),
      upr = pmin(upr, 1),
      variable = .variable
    ) %>%
    select(variable, age, est, lwr, upr)

}
