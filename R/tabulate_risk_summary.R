##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param design
##' @param decimals
tabulate_risk_summary <- function(design) {

  design_no_cvdhx <- subset(design, ever_had_ascvd == 'no')

  # Proportion at high risk -------------------------------------------------

  hrisk_ovrl_ovrl <- svyciprop(~ pcr_highrisk, design)
  hrisk_ovrl_ovrl_se <- as.numeric(SE(hrisk_ovrl_ovrl))

  hrisk_ovrl <- list(
    diabetes     = ~ diabetes,
    ckd          = ~ ckd,
    # diab_and_ckd = ~ diabetes_and_ckd,
    age_group    = ~ age_gt65,
    any          = ~ any_ckd_diab_age65
  ) %>%
    map(
      ~svyby(
        formula = ~ pcr_highrisk,
        design = design,
        by = .x,
        FUN = svyciprop
      )
    ) %>%
    map(add_confint) %>%
    map(as_tibble) %>%
    map(mutate_if, is.factor, as.character) %>%
    map(standardize_names) %>%
    bind_rows(.id = 'variable') %>%
    add_row(
      variable = 'Overall', level = "yes",
      pcr_highrisk = as.numeric(hrisk_ovrl_ovrl),
      se = hrisk_ovrl_ovrl_se,
      lwr = attr(hrisk_ovrl_ovrl, 'ci')['2.5%'],
      upr = attr(hrisk_ovrl_ovrl, 'ci')['97.5%']
    ) %>%
    mutate(bp_cat = 'Overall')

  #hrisk_ovrl

  hrisk_tbl <- list(
    Overall = ~ bp_cat,
    diabetes = ~ diabetes + bp_cat,
    ckd = ~ ckd + bp_cat,
    # diab_and_ckd = ~ diabetes_and_ckd + bp_cat,
    age_group = ~ age_gt65 + bp_cat,
    any = ~ any_ckd_diab_age65 + bp_cat
  ) %>%
    map(
      ~svyby(
        formula = ~pcr_highrisk,
        design = design,
        by = .x,
        FUN = svyciprop
      )
    ) %>%
    map(add_confint) %>%
    map(as_tibble) %>%
    map(mutate_if, is.factor, as.character) %>%
    map(standardize_names) %>%
    bind_rows(.id = 'variable') %>%
    bind_rows(hrisk_ovrl) %>%
    filter(level == "yes" | is.na(level)) %>%
    transmute(
      variable,
      bp_cat = fct_inorder(bp_cat),
      bp_cat = fct_relevel(bp_cat, 'Overall'),
      tbv = table_glue('{100*pcr_highrisk}\n({100*lwr}, {100*upr})')
    ) %>%
    arrange(bp_cat) %>%
    pivot_wider(values_from = tbv, names_from = variable)

  # median risk -------------------------------------------------------------

  qts <- c(0.25, 0.50, 0.75)

  md_risk_init <-
    svyquantile(~ascvd_risk_pcr, design_no_cvdhx, quantiles = qts)

  md_risk_ovrl_ovrl <-
    tibble(
      est = as.numeric(md_risk_init$ascvd_risk_pcr[,'quantile'])[2],
      lwr = as.numeric(md_risk_init$ascvd_risk_pcr[,'quantile'])[1],
      upr = as.numeric(md_risk_init$ascvd_risk_pcr[,'quantile'])[3],
      variable = 'Overall',
      level = NA_character_
    )

  md_risk_ovrl <- suppressWarnings(list(
    diabetes     = ~ diabetes,
    ckd          = ~ ckd,
    # diab_and_ckd = ~ diabetes_and_ckd,
    age_group    = ~ age_gt65,
    any          = ~ any_ckd_diab_age65
  ) %>%
    map(
      ~svyby(
        formula = ~ascvd_risk_pcr,
        design = design_no_cvdhx,
        by = .x,
        FUN = svyquantile,
        quantiles = qts,
        ci = TRUE
      )
    ) %>%
    map(as_tibble) %>%
    map(rename,
        lwr = `ascvd_risk_pcr.0.25`,
        est = `ascvd_risk_pcr.0.5`,
        upr = `ascvd_risk_pcr.0.75`) %>%
    map(select, -starts_with('se.')) %>%
    map(mutate_if, is.factor, as.character) %>%
    map(standardize_names) %>%
    bind_rows(.id = 'variable') %>%
    bind_rows(md_risk_ovrl_ovrl) %>%
    mutate(bp_cat = 'Overall'))

  md_risk_tbl <- suppressWarnings(list(
    Overall      = ~ bp_cat,
    diabetes     = ~ bp_cat + diabetes,
    ckd          = ~ bp_cat + ckd,
    # diab_and_ckd = ~ bp_cat + diabetes_and_ckd,
    age_group    = ~ bp_cat + age_gt65,
    any          = ~ bp_cat + any_ckd_diab_age65
  ) %>%
    map(
      ~svyby(
        formula = ~ascvd_risk_pcr,
        design = design_no_cvdhx,
        by = .x,
        FUN = svyquantile,
        quantiles = qts,
        ci = TRUE
      )
    ) %>%
    map(rename,
        lwr = `ascvd_risk_pcr.0.25`,
        est = `ascvd_risk_pcr.0.5`,
        upr = `ascvd_risk_pcr.0.75`) %>%
    map(select, -starts_with('se.')) %>%
    map(mutate_if, is.factor, as.character) %>%
    map(standardize_names) %>%
    bind_rows(.id = 'variable') %>%
    bind_rows(md_risk_ovrl) %>%
    filter(level == "yes" | is.na(level)) %>%
    transmute(
      variable,
      bp_cat = fct_inorder(bp_cat),
      bp_cat = fct_relevel(bp_cat, 'Overall'),
      tbv = table_glue('{100*est}\n({100*lwr}, {100*upr})')
    ) %>%
    arrange(bp_cat) %>%
    pivot_wider(values_from = tbv, names_from = variable))

  bind_rows(
    high_risk = hrisk_tbl,
    median_risk = md_risk_tbl,
    .id = 'group'
  )

}

add_confint <- function(x){
  cbind(x, confint(x))
}

standardize_names <- function(df){

  p <- '^diabetes$|^ckd$|^age_gt65$|^any_ckd_diab_age65$|^diabetes_and_ckd$'

  names(df) <- str_replace(names(df), pattern = p, replacement = 'level')

  se_col <- str_detect(names(df), pattern = '^se\\.')
  lwr_col <- str_detect(names(df), pattern = '^2\\.5')
  upr_col <- str_detect(names(df), pattern = '^97\\.5')

  if(any(se_col)) names(df)[se_col] <- 'se'
  if(any(lwr_col)) names(df)[lwr_col] <- 'lwr'
  if(any(upr_col)) names(df)[upr_col] <- 'upr'


  df

}
