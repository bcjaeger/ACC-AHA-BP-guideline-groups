##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param design_overall
##' @param design_s1hyp
##' @return
##' @author bcjaeger
##' @export
make_inline_results <- function(design_overall,
                                design_s1hyp,
                                risk_models) {


  # rspec <- round_spec() %>%
  #   round_using_magnitude(digits = c(2,1,1),
  #                         breaks = c(1,10,Inf)) %>%
  #   default_rounder_set()

  # accompanying table 1 ----

  prevDiabetesOverall <- svyciprop(~ diabetes, design_overall) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(string = table_glue("{est}% ({lwr}, {upr})")) %>%
    pull(string)

  prevCkdOverall <- svyciprop(~ ckd, design_overall) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(string = table_glue("{est}% ({lwr}, {upr})")) %>%
    pull(string)

  prevAge65Overall <- svyciprop(~ age_gt65, design_overall) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(string = table_glue("{est}% ({lwr}, {upr})")) %>%
    pull(string)

  # not currently used in manuscript, but kept around just in case
  meanAgeOverall <- svymean(~ age, design_overall) %>%
    tidy_svy(mult_by = 1) %>%
    transmute(string = table_glue("{est} ({lwr}, {upr})")) %>%
    pull(string)

  # accompanying table 2 ----

  prevS1hOverall <- svyciprop(
    formula = ~ I(bp_cat == 'Stage 1 hypertension'),
    design = design_overall
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% (95% CI: {lwr}, {upr})")
    ) %>%
    pull(string)

  prevS1hDiabetes <- svyciprop(
    formula = ~ I(bp_cat == 'Stage 1 hypertension'),
    design = subset(design_overall, diabetes == 'yes')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% ({lwr}, {upr})")
    ) %>%
    pull(string)


  prevS1hCkd <- svyciprop(
    formula = ~ I(bp_cat == 'Stage 1 hypertension'),
    design = subset(design_overall, ckd == 'yes')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% ({lwr}, {upr})")
    ) %>%
    pull(string)

  prevS1hAge65 <- svyciprop(
    formula = ~ I(bp_cat == 'Stage 1 hypertension'),
    design = subset(design_overall, age_gt65 == 'yes')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% ({lwr}, {upr})")
    ) %>%
    pull(string)

  # accompanying table 3 ----

  medianPcrOverall <- svyquantile(
    x = ~ ascvd_risk_pcr,
    design = subset(design_overall, ever_had_ascvd == 'no'),
    quantiles = c(0.25, 0.50, 0.75)
  ) %>%
    as_tibble() %>%
    set_names(c('lwr', 'est', 'upr')) %>%
    mutate_all(~.x * 100) %>%
    transmute(string = table_glue("{est}% ({lwr}, {upr})")) %>%
    pull(string)

  medianPcrS1hOverall <- svyquantile(
    x = ~ ascvd_risk_pcr,
    design = subset(design_s1hyp,
                    ever_had_ascvd == 'no'),
    quantiles = c(0.25, 0.50, 0.75)
  ) %>%
    as_tibble() %>%
    set_names(c('lwr', 'est', 'upr')) %>%
    mutate_all(~.x * 100) %>%
    transmute(string = table_glue("{est}% ({lwr}, {upr})")) %>%
    pull(string)

  medianPcrDiabetes <- svyquantile(
    x = ~ ascvd_risk_pcr,
    design = subset(design_overall,
                    diabetes == 'yes' & ever_had_ascvd == 'no'),
    quantiles = c(0.25, 0.50, 0.75)
  ) %>%
    as_tibble() %>%
    set_names(c('lwr', 'est', 'upr')) %>%
    mutate_all(~.x * 100) %>%
    transmute(string = table_glue("{est}% ({lwr}, {upr})")) %>%
    pull(string)

  medianPcrS1hDiabetes <- svyquantile(
    x = ~ ascvd_risk_pcr,
    design = subset(design_s1hyp,
                    diabetes == 'yes' & ever_had_ascvd == 'no'),
    quantiles = c(0.25, 0.50, 0.75)
  ) %>%
    as_tibble() %>%
    set_names(c('lwr', 'est', 'upr')) %>%
    mutate_all(~.x * 100) %>%
    transmute(string = table_glue("{est}% ({lwr}, {upr})")) %>%
    pull(string)

  medianPcrCkd <- svyquantile(
    x = ~ ascvd_risk_pcr,
    design = subset(design_overall,
                    ckd == 'yes' & ever_had_ascvd == 'no'),
    quantiles = c(0.25, 0.50, 0.75)
  ) %>%
    as_tibble() %>%
    set_names(c('lwr', 'est', 'upr')) %>%
    mutate_all(~.x * 100) %>%
    transmute(string = table_glue("{est}% ({lwr}, {upr})")) %>%
    pull(string)

  medianPcrS1hCkd <- svyquantile(
    x = ~ ascvd_risk_pcr,
    design = subset(design_s1hyp,
                    ckd == 'yes' & ever_had_ascvd == 'no'),
    quantiles = c(0.25, 0.50, 0.75)
  ) %>%
    as_tibble() %>%
    set_names(c('lwr', 'est', 'upr')) %>%
    mutate_all(~.x * 100) %>%
    transmute(string = table_glue("{est}% ({lwr}, {upr})")) %>%
    pull(string)

  medianPcrAge65 <- svyquantile(
    x = ~ ascvd_risk_pcr,
    design = subset(design_overall,
                    age_gt65 == 'yes' & ever_had_ascvd == 'no'),
    quantiles = c(0.25, 0.50, 0.75)
  ) %>%
    as_tibble() %>%
    set_names(c('lwr', 'est', 'upr')) %>%
    mutate_all(~.x * 100) %>%
    transmute(string = table_glue("{est}% ({lwr}, {upr})")) %>%
    pull(string)

  medianPcrS1hAge65 <- svyquantile(
    x = ~ ascvd_risk_pcr,
    design = subset(design_s1hyp,
                    age_gt65 == 'yes' & ever_had_ascvd == 'no'),
    quantiles = c(0.25, 0.50, 0.75)
  ) %>%
    as_tibble() %>%
    set_names(c('lwr', 'est', 'upr')) %>%
    mutate_all(~.x * 100) %>%
    transmute(string = table_glue("{est}% ({lwr}, {upr})")) %>%
    pull(string)



  prevHighRiskDiabetes <- svyciprop(
    formula = ~ pcr_highrisk,
    design = subset(design_overall, diabetes == 'yes')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% (95% CI: {lwr}, {upr})")
    ) %>%
    pull(string)

  prevHighRiskCkd <- svyciprop(
    formula = ~ pcr_highrisk,
    design = subset(design_overall, ckd == 'yes')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% (95% CI: {lwr}, {upr})")
    ) %>%
    pull(string)

  prevHighRiskAge65 <- svyciprop(
    formula = ~ pcr_highrisk,
    design = subset(design_overall, age_gt65 == 'yes')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% (95% CI: {lwr}, {upr})")
    ) %>%
    pull(string)

  prevHighRiskS1hDiabetes <- svyciprop(
    formula = ~ pcr_highrisk,
    design = subset(design_s1hyp, diabetes == 'yes')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% (95% CI: {lwr}, {upr})")
    ) %>%
    pull(string)

  prevHighRiskS1hCkd <- svyciprop(
    formula = ~ pcr_highrisk,
    design = subset(design_s1hyp, ckd == 'yes')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% (95% CI: {lwr}, {upr})")
    ) %>%
    pull(string)

  prevHighRiskS1hAge65 <- svyciprop(
    formula = ~ pcr_highrisk,
    design = subset(design_s1hyp, age_gt65 == 'yes')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% (95% CI: {lwr}, {upr})")
    ) %>%
    pull(string)

  # accompanying figure 1 ----

  design_low_risk <- subset(design_overall, pcr_highrisk == 'no')

  propLowRiskOverall <- svyciprop(
    formula = ~ ascvd_risk_pcr < 0.05,
    design = design_low_risk
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% (95% CI: {lwr}, {upr})")
    ) %>%
    pull(string)

  propLowRiskDiabetes <- svyciprop(
    formula = ~ ascvd_risk_pcr < 0.05,
    design = subset(design_low_risk, diabetes == 'yes')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% (95% CI {lwr}, {upr})")
    ) %>%
    pull(string)

  propLowRiskCkd <- svyciprop(
    formula = ~ ascvd_risk_pcr < 0.05,
    design = subset(design_low_risk, ckd == 'yes')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% (95% CI {lwr}, {upr})")
    ) %>%
    pull(string)

  propLowRiskAge65 <- svyciprop(
    formula = ~ ascvd_risk_pcr < 0.05,
    design = subset(design_low_risk, age_gt65 == 'yes')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% (95% CI {lwr}, {upr})")
    ) %>%
    pull(string)

  propLowRiskS1hDiabetes <- svyciprop(
    formula = ~ ascvd_risk_pcr < 0.05,
    design = subset(design_low_risk, diabetes == 'yes' &
                      bp_cat == 'Stage 1 hypertension')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% (95% CI {lwr}, {upr})")
    ) %>%
    pull(string)

  propLowRiskS1hCkd <- svyciprop(
    formula = ~ ascvd_risk_pcr < 0.05,
    design = subset(design_low_risk, ckd == 'yes' &
                      bp_cat == 'Stage 1 hypertension')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% (95% CI {lwr}, {upr})")
    ) %>%
    pull(string)

  propLowRiskS1hAge65 <- svyciprop(
    formula = ~ ascvd_risk_pcr < 0.05,
    design = subset(design_low_risk, age_gt65 == 'yes' &
                      bp_cat == 'Stage 1 hypertension')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% (95% CI {lwr}, {upr})")
    ) %>%
    pull(string)

  # accompanying figure 2 ----

  rspec <- round_spec() %>%
    round_using_magnitude(digits = c(2, 1, 1, 0),
                          breaks = c(1, 10, 100, Inf))

  risk_50 <- risk_models %>%
    select(bp_cat, bnry) %>%
    unnest(cols = bnry) %>%
    mutate(diff = abs(est - 1/2)) %>%
    group_by(bp_cat, variable) %>%
    arrange(diff) %>%
    slice(1) %>%
    mutate(ci_est = table_glue("95% CI: {100*lwr}, {100*upr}"))

  ageHighRiskOvrlNoComorb <- risk_50 %>%
    filter(bp_cat == 'ovrl', variable == 'no_diabetes_or_ckd') %>%
    pull(age) %>%
    table_value()

  intervalHighRiskOvrlNoComorb <- risk_50 %>%
    filter(bp_cat == 'ovrl', variable == 'no_diabetes_or_ckd') %>%
    pull(ci_est)

  ageHighRiskOvrlDiabetes <- risk_50 %>%
    filter(bp_cat == 'ovrl', variable == 'diabetes') %>%
    pull(age) %>%
    table_value()

  intervalHighRiskOvrlDiabetes <- risk_50 %>%
    filter(bp_cat == 'ovrl', variable == 'diabetes') %>%
    pull(ci_est)

  ageHighRiskOvrlCkd <- risk_50 %>%
    filter(bp_cat == 'ovrl', variable == 'ckd') %>%
    pull(age) %>%
    table_value()

  intervalHighRiskOvrlCkd <- risk_50 %>%
    filter(bp_cat == 'ovrl', variable == 'ckd') %>%
    pull(ci_est)

  ageHighRiskS1hNoComorb <- risk_50 %>%
    filter(bp_cat == 'stg1', variable == 'no_diabetes_or_ckd') %>%
    pull(age) %>%
    table_value()

  intervalHighRiskS1hNoComorb <- risk_50 %>%
    filter(bp_cat == 'stg1', variable == 'no_diabetes_or_ckd') %>%
    pull(ci_est)

  ageHighRiskS1hDiabetes <- risk_50 %>%
    filter(bp_cat == 'stg1', variable == 'diabetes') %>%
    pull(age) %>%
    table_value()

  intervalHighRiskS1hDiabetes <- risk_50 %>%
    filter(bp_cat == 'stg1', variable == 'diabetes') %>%
    pull(ci_est)

  ageHighRiskS1hCkd <- risk_50 %>%
    filter(bp_cat == 'stg1', variable == 'ckd') %>%
    pull(age) %>%
    table_value()

  intervalHighRiskS1hCkd <- risk_50 %>%
    filter(bp_cat == 'stg1', variable == 'ckd') %>%
    pull(ci_est)


  list(
    prevDiabetesOverall  = prevDiabetesOverall,
    prevCkdOverall       = prevCkdOverall,
    prevAge65Overall     = prevAge65Overall,
    prevS1hOverall       = prevS1hOverall,
    prevS1hDiabetes      = prevS1hDiabetes,
    prevS1hCkd           = prevS1hCkd,
    prevS1hAge65         = prevS1hAge65,
    meanAgeOverall       = meanAgeOverall,
    medianPcrAge65       = medianPcrAge65,
    medianPcrCkd         = medianPcrCkd,
    medianPcrDiabetes    = medianPcrDiabetes,
    medianPcrOverall     = medianPcrOverall,
    medianPcrS1hAge65    = medianPcrS1hAge65,
    medianPcrS1hCkd      = medianPcrS1hCkd,
    medianPcrS1hDiabetes = medianPcrS1hDiabetes,
    medianPcrS1hOverall  = medianPcrS1hOverall,
    prevHighRiskCkd      = prevHighRiskCkd,
    prevHighRiskDiabetes = prevHighRiskDiabetes,
    prevHighRiskAge65    = prevHighRiskAge65,
    prevHighRiskS1hCkd      = prevHighRiskS1hCkd,
    prevHighRiskS1hDiabetes = prevHighRiskS1hDiabetes,
    prevHighRiskS1hAge65    = prevHighRiskS1hAge65,
    propLowRiskOverall      = propLowRiskOverall,
    propLowRiskAge65        = propLowRiskAge65,
    propLowRiskCkd          = propLowRiskCkd,
    propLowRiskDiabetes     = propLowRiskDiabetes,
    propLowRiskOverall      = propLowRiskOverall,
    propLowRiskS1hAge65     = propLowRiskS1hAge65,
    propLowRiskS1hCkd       = propLowRiskS1hCkd,
    propLowRiskS1hDiabetes  = propLowRiskS1hDiabetes,
    ageHighRiskOvrlNoComorb      = ageHighRiskOvrlNoComorb,
    ageHighRiskOvrlDiabetes      = ageHighRiskOvrlDiabetes,
    ageHighRiskOvrlCkd           = ageHighRiskOvrlCkd,
    intervalHighRiskOvrlNoComorb = intervalHighRiskOvrlNoComorb,
    intervalHighRiskOvrlDiabetes = intervalHighRiskOvrlDiabetes,
    intervalHighRiskOvrlCkd      = intervalHighRiskOvrlCkd,
    ageHighRiskS1hNoComorb       = ageHighRiskS1hNoComorb,
    ageHighRiskS1hDiabetes       = ageHighRiskS1hDiabetes,
    ageHighRiskS1hCkd            = ageHighRiskS1hCkd,
    intervalHighRiskS1hNoComorb  = intervalHighRiskS1hNoComorb,
    intervalHighRiskS1hDiabetes  = intervalHighRiskS1hDiabetes,
    intervalHighRiskS1hCkd       = intervalHighRiskS1hCkd
  )

}

# helper functions to add a CI and tidy results

tidy_svy <- function(svy_object, mult_by){

  ci <- confint(svy_object) %>%
    as_tibble() %>%
    set_names(c('lwr','upr'))

  est <- enframe(svy_object, name = 'variable', value = 'est')

  bind_cols(est, ci) %>%
    mutate(est = as.numeric(est)) %>%
    mutate_if(is.numeric, ~.x * mult_by)

}



