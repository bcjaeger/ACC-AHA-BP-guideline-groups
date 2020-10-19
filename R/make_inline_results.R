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


  # accompanying table 1 ----

  prevDiabetesOverall <- svyciprop(~ diabetes, design_overall) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(string = table_glue("{est}% ({lwr}%, {upr}%)")) %>%
    pull(string)

  prevCkdOverall <- svyciprop(~ ckd, design_overall) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(string = table_glue("{est}% ({lwr}%, {upr}%)")) %>%
    pull(string)

  prevAge65Overall <- svyciprop(~ age_gt65, design_overall) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(string = table_glue("{est}% ({lwr}%, {upr}%)")) %>%
    pull(string)

  prevAnyOverall <- svyciprop(~ any_ckd_diab_age65, design_overall) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(string = table_glue("{est}% ({lwr}%, {upr}%)")) %>%
    pull(string)

  meanAgeOverall <- svymean(~ age, design_overall) %>%
    tidy_svy(mult_by = 1) %>%
    transmute(string = table_glue("{est} ({lwr}, {upr})")) %>%
    pull(string)

  meanAgeS1h <- svymean(~ age, design_s1hyp) %>%
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
      string = table_glue("{est}% ({lwr}%, {upr}%)")
    ) %>%
    pull(string)

  prevS1hDiabetes <- svyciprop(
    formula = ~ I(bp_cat == 'Stage 1 hypertension'),
    design = subset(design_overall, diabetes == 'yes')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% ({lwr}%, {upr}%)")
    ) %>%
    pull(string)


  prevS1hCkd <- svyciprop(
    formula = ~ I(bp_cat == 'Stage 1 hypertension'),
    design = subset(design_overall, ckd == 'yes')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% ({lwr}%, {upr}%)")
    ) %>%
    pull(string)

  prevS1hAge65 <- svyciprop(
    formula = ~ I(bp_cat == 'Stage 1 hypertension'),
    design = subset(design_overall, age_gt65 == 'yes')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% ({lwr}%, {upr}%)")
    ) %>%
    pull(string)

  prevS1hAny <- svyciprop(
    formula = ~ I(bp_cat == 'Stage 1 hypertension'),
    design = subset(design_overall, any_ckd_diab_age65 == 'yes')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% ({lwr}%, {upr}%)")
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
    transmute(string = table_glue("{est}% ({lwr}%, {upr}%)")) %>%
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
    transmute(string = table_glue("{est}% ({lwr}%, {upr}%)")) %>%
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
    transmute(string = table_glue("{est}% ({lwr}%, {upr}%)")) %>%
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
    transmute(string = table_glue("{est}% ({lwr}%, {upr}%)")) %>%
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
    transmute(string = table_glue("{est}% ({lwr}%, {upr}%)")) %>%
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
    transmute(string = table_glue("{est}% ({lwr}%, {upr}%)")) %>%
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
    transmute(string = table_glue("{est}% ({lwr}%, {upr}%)")) %>%
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
    transmute(string = table_glue("{est}% ({lwr}%, {upr}%)")) %>%
    pull(string)

  medianPcrAny <- svyquantile(
    x = ~ ascvd_risk_pcr,
    design = subset(design_overall,
                    any_ckd_diab_age65 == 'yes' & ever_had_ascvd == 'no'),
    quantiles = c(0.25, 0.50, 0.75)
  ) %>%
    as_tibble() %>%
    set_names(c('lwr', 'est', 'upr')) %>%
    mutate_all(~.x * 100) %>%
    transmute(string = table_glue("{est}% ({lwr}%, {upr}%)")) %>%
    pull(string)

  medianPcrS1hAny <- svyquantile(
    x = ~ ascvd_risk_pcr,
    design = subset(design_s1hyp,
                    any_ckd_diab_age65 == 'yes' & ever_had_ascvd == 'no'),
    quantiles = c(0.25, 0.50, 0.75)
  ) %>%
    as_tibble() %>%
    set_names(c('lwr', 'est', 'upr')) %>%
    mutate_all(~.x * 100) %>%
    transmute(string = table_glue("{est}% ({lwr}%, {upr}%)")) %>%
    pull(string)

  prevHighRisk <- list(
    overall.bp_cat = ~ bp_cat,
    diabetes.overall = ~ diabetes,
    diabetes.bp_cat = ~ diabetes + bp_cat,
    ckd.overall = ~ ckd,
    ckd.bp_cat = ~ ckd + bp_cat,
    age_gt65.overall = ~ age_gt65,
    age_gt65.bp_cat = ~ age_gt65 + bp_cat,
    any.overall = ~ any_ckd_diab_age65,
    any.bp_cat = ~ any_ckd_diab_age65 + bp_cat
  ) %>%
    map(
      ~svyby(
        formula = ~pcr_highrisk,
        design = design_overall,
        by = .x,
        FUN = svyciprop
      )
    ) %>%
    map(add_confint) %>%
    map(as_tibble) %>%
    map(mutate_if, is.factor, as.character) %>%
    map(standardize_names) %>%
    bind_rows(.id = 'variable') %>%
    filter(level == "yes" | is.na(level)) %>%
    transmute(
      variable,
      bp_cat = replace(bp_cat, is.na(bp_cat), "overall"),
      bp_cat = fct_inorder(bp_cat),
      across(c(pcr_highrisk, lwr, upr), ~.x * 100),
      inline = table_glue('{pcr_highrisk}% ({lwr}%, {upr}%)')
    ) %>%
    arrange(bp_cat)

  prevHighRiskOverall <- svyciprop(~ pcr_highrisk, design_overall) %>%
    cbind(confint(.)) %>%
    as_tibble() %>%
    set_names(c('est', 'lwr', 'upr')) %>%
    mutate(across(everything(), ~.x * 100)) %>%
    mutate(inline = table_glue('{est}% ({lwr}%, {upr}%)')) %>%
    pull(inline)

  prevHighRiskS1hOverall <- prevHighRisk %>%
    filter(variable == 'overall.bp_cat',
           bp_cat == 'Stage 1 hypertension') %>%
    pull(inline)


  prevHighRiskDiabetes <- prevHighRisk %>%
    filter(variable == 'diabetes.overall') %>%
    pull(inline)

  prevHighRiskCkd <- prevHighRisk %>%
    filter(variable == 'ckd.overall') %>%
    pull(inline)

  prevHighRiskAge65 <- prevHighRisk %>%
    filter(variable == 'age_gt65.overall') %>%
    pull(inline)

  prevHighRiskAny <- prevHighRisk %>%
    filter(variable == 'any.overall') %>%
    pull(inline)

  prevHighRiskS1hDiabetes <- prevHighRisk %>%
    filter(variable == 'diabetes.bp_cat',
           bp_cat == 'Stage 1 hypertension') %>%
    pull(inline)

  prevHighRiskS1hCkd <- prevHighRisk %>%
    filter(variable == 'ckd.bp_cat',
           bp_cat == 'Stage 1 hypertension') %>%
    pull(inline)

  prevHighRiskS1hAge65 <- prevHighRisk %>%
    filter(variable == 'age_gt65.bp_cat',
           bp_cat == 'Stage 1 hypertension') %>%
    pull(inline)

  prevHighRiskS1hAny <- prevHighRisk %>%
    filter(variable == 'any.bp_cat',
           bp_cat == 'Stage 1 hypertension') %>%
    pull(inline)


  # accompanying figure 1 ----

  design_low_risk <- subset(design_overall, pcr_highrisk == 'no')

  propLowRiskOverall <- svyciprop(
    formula = ~ ascvd_risk_pcr < 0.025,
    design = design_low_risk
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% (95% CI: {lwr}%, {upr}%)")
    ) %>%
    pull(string)

  propLowRiskDiabetes <- svyciprop(
    formula = ~ ascvd_risk_pcr < 0.025,
    design = subset(design_low_risk, diabetes == 'yes')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% (95% CI: {lwr}%, {upr}%)")
    ) %>%
    pull(string)

  propLowRiskCkd <- svyciprop(
    formula = ~ ascvd_risk_pcr < 0.025,
    design = subset(design_low_risk, ckd == 'yes')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% (95% CI: {lwr}%, {upr}%)")
    ) %>%
    pull(string)

  propLowRiskAge65 <- svyciprop(
    formula = ~ ascvd_risk_pcr < 0.025,
    design = subset(design_low_risk, age_gt65 == 'yes')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% (95% CI: {lwr}%, {upr}%)")
    ) %>%
    pull(string)

  propLowRiskAny <- svyciprop(
    formula = ~ ascvd_risk_pcr < 0.025,
    design = subset(design_low_risk, any_ckd_diab_age65 == 'yes')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% (95% CI: {lwr}%, {upr}%)")
    ) %>%
    pull(string)

  propLowRiskS1hOverall <- svyciprop(
    formula = ~ ascvd_risk_pcr < 0.025,
    design = subset(design_low_risk, bp_cat == 'Stage 1 hypertension')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% (95% CI: {lwr}%, {upr}%)")
    ) %>%
    pull(string)

  propLowRiskS1hDiabetes <- svyciprop(
    formula = ~ ascvd_risk_pcr < 0.025,
    design = subset(design_low_risk, diabetes == 'yes' &
                      bp_cat == 'Stage 1 hypertension')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% (95% CI: {lwr}%, {upr}%)")
    ) %>%
    pull(string)

  propLowRiskS1hCkd <- svyciprop(
    formula = ~ ascvd_risk_pcr < 0.025,
    design = subset(design_low_risk, ckd == 'yes' &
                      bp_cat == 'Stage 1 hypertension')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% (95% CI: {lwr}%, {upr}%)")
    ) %>%
    pull(string)

  propLowRiskS1hAge65 <- svyciprop(
    formula = ~ ascvd_risk_pcr < 0.025,
    design = subset(design_low_risk, age_gt65 == 'yes' &
                      bp_cat == 'Stage 1 hypertension')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% (95% CI: {lwr}%, {upr}%)")
    ) %>%
    pull(string)

  propLowRiskS1hAny <- svyciprop(
    formula = ~ ascvd_risk_pcr < 0.025,
    design = subset(design_low_risk, any_ckd_diab_age65 == 'yes' &
                      bp_cat == 'Stage 1 hypertension')
  ) %>%
    tidy_svy(mult_by = 100) %>%
    transmute(
      string = table_glue("{est}% (95% CI: {lwr}%, {upr}%)")
    ) %>%
    pull(string)

  # accompanying figure 2 ----

  risk_50 <- risk_models %>%
    select(bp_cat, bnry) %>%
    unnest(cols = bnry) %>%
    mutate(diff = abs(est - 1/2)) %>%
    group_by(bp_cat, variable) %>%
    arrange(diff) %>%
    slice(1) %>%
    mutate(ci_est = table_glue("95% CI: {100*lwr}, {100*upr}"))

  rspec_ages <- round_spec() %>%
    round_using_decimal(digits = 0)

  ageHighRiskOvrlNoComorb <- risk_50 %>%
    filter(bp_cat == 'ovrl', variable == 'no_diabetes_or_ckd') %>%
    pull(age) %>%
    table_value(rspec = rspec_ages)

  intervalHighRiskOvrlNoComorb <- risk_50 %>%
    filter(bp_cat == 'ovrl', variable == 'no_diabetes_or_ckd') %>%
    pull(ci_est)

  ageHighRiskOvrlDiabetes <- risk_50 %>%
    filter(bp_cat == 'ovrl', variable == 'diabetes') %>%
    pull(age) %>%
    table_value(rspec = rspec_ages)

  intervalHighRiskOvrlDiabetes <- risk_50 %>%
    filter(bp_cat == 'ovrl', variable == 'diabetes') %>%
    pull(ci_est)

  ageHighRiskOvrlCkd <- risk_50 %>%
    filter(bp_cat == 'ovrl', variable == 'ckd') %>%
    pull(age) %>%
    table_value(rspec = rspec_ages)

  intervalHighRiskOvrlCkd <- risk_50 %>%
    filter(bp_cat == 'ovrl', variable == 'ckd') %>%
    pull(ci_est)

  ageHighRiskS1hNoComorb <- risk_50 %>%
    filter(bp_cat == 'stg1', variable == 'no_diabetes_or_ckd') %>%
    pull(age) %>%
    table_value(rspec = rspec_ages)

  intervalHighRiskS1hNoComorb <- risk_50 %>%
    filter(bp_cat == 'stg1', variable == 'no_diabetes_or_ckd') %>%
    pull(ci_est)

  ageHighRiskS1hDiabetes <- risk_50 %>%
    filter(bp_cat == 'stg1', variable == 'diabetes') %>%
    pull(age) %>%
    table_value(rspec = rspec_ages)

  intervalHighRiskS1hDiabetes <- risk_50 %>%
    filter(bp_cat == 'stg1', variable == 'diabetes') %>%
    pull(ci_est)

  ageHighRiskS1hCkd <- risk_50 %>%
    filter(bp_cat == 'stg1', variable == 'ckd') %>%
    pull(age) %>%
    table_value(rspec = rspec_ages)

  intervalHighRiskS1hCkd <- risk_50 %>%
    filter(bp_cat == 'stg1', variable == 'ckd') %>%
    pull(ci_est)


  list(
    prevDiabetesOverall  = prevDiabetesOverall,
    prevCkdOverall       = prevCkdOverall,
    prevAge65Overall     = prevAge65Overall,
    prevAnyOverall       = prevAnyOverall,
    prevS1hOverall       = prevS1hOverall,
    prevS1hDiabetes      = prevS1hDiabetes,
    prevS1hCkd           = prevS1hCkd,
    prevS1hAge65         = prevS1hAge65,
    prevS1hAny           = prevS1hAny,
    meanAgeOverall       = meanAgeOverall,
    meanAgeS1h           = meanAgeS1h,
    medianPcrAge65       = medianPcrAge65,
    medianPcrCkd         = medianPcrCkd,
    medianPcrDiabetes    = medianPcrDiabetes,
    medianPcrOverall     = medianPcrOverall,
    medianPcrAny         = medianPcrAny,
    medianPcrS1hAge65    = medianPcrS1hAge65,
    medianPcrS1hCkd      = medianPcrS1hCkd,
    medianPcrS1hDiabetes = medianPcrS1hDiabetes,
    medianPcrS1hOverall  = medianPcrS1hOverall,
    medianPcrS1hAny      = medianPcrS1hAny,
    prevHighRiskOverall  = prevHighRiskOverall,
    prevHighRiskCkd      = prevHighRiskCkd,
    prevHighRiskDiabetes = prevHighRiskDiabetes,
    prevHighRiskAge65    = prevHighRiskAge65,
    prevHighRiskAny      = prevHighRiskAny,
    prevHighRiskS1hOverall  = prevHighRiskS1hOverall,
    prevHighRiskS1hCkd      = prevHighRiskS1hCkd,
    prevHighRiskS1hDiabetes = prevHighRiskS1hDiabetes,
    prevHighRiskS1hAge65    = prevHighRiskS1hAge65,
    prevHighRiskS1hAny      = prevHighRiskS1hAny,
    propLowRiskOverall      = propLowRiskOverall,
    propLowRiskAge65        = propLowRiskAge65,
    propLowRiskCkd          = propLowRiskCkd,
    propLowRiskDiabetes     = propLowRiskDiabetes,
    propLowRiskAny          = propLowRiskAny,
    propLowRiskS1hOverall   = propLowRiskS1hOverall,
    propLowRiskS1hAge65     = propLowRiskS1hAge65,
    propLowRiskS1hCkd       = propLowRiskS1hCkd,
    propLowRiskS1hDiabetes  = propLowRiskS1hDiabetes,
    propLowRiskS1hAny       = propLowRiskS1hAny,
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



