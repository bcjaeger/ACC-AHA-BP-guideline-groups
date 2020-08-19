##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param risk_models
##' @param bp_level
##' @param color_key
##'
##' TODO: add a star/dagger next to labels, then include a footnote explaining.
##'
visualize_risk_bnry <- function(risk_models,
                                bp_level,
                                color_key) {

  .design <- risk_models %>%
    filter(bp_cat == bp_level) %>%
    slice(1) %>%
    pull(.design) %>%
    .[[1]]

  .designs <- list(
    diabetes = subset(.design, diabetes == "yes"),
    ckd = subset(.design, ckd == "yes"),
    no_diabetes_or_ckd = subset(.design, no_diabetes_or_ckd == "yes")
  )

  age_quants <- map(
    .x = .designs,
    .f = ~ svyquantile(
      x = ~ age,
      design = .x,
      quantiles = c(0.25, 0.50, 0.75)) %>%
      as_tibble() %>%
      pivot_longer(cols = everything())
  ) %>%
    bind_rows(.id = '.variable') %>%
    rename(quant = name, age = value) %>%
    mutate(
      .variable = factor(
        .variable,
        levels = c('diabetes','ckd','no_diabetes_or_ckd')
      ),
      .variable = fct_recode(
        .f = .variable,
        'Diabetes' = 'diabetes',
        'Chronic kidney disease' = 'ckd',
        'No Diabetes or chronic kidney disease' = 'no_diabetes_or_ckd'
      )
    )

  ggdat <- risk_models %>%
    filter(bp_cat == bp_level) %>%
    select(.variable, bnry) %>%
    unnest(cols = bnry) %>%
    mutate(
      .variable = factor(
        .variable,
        levels = c('diabetes','ckd','no_diabetes_or_ckd')
      ),
      .variable = fct_recode(
        .f = .variable,
        'Diabetes' = 'diabetes',
        'Chronic kidney disease' = 'ckd',
        'No diabetes or chronic kidney disease' = 'no_diabetes_or_ckd'
      )
    )

  age_quants %<>%
    rename(age2 = age) %>%
    left_join(ggdat) %>%
    group_by(.variable, quant) %>%
    mutate(diff = abs(age - age2)) %>%
    arrange(diff) %>%
    slice(1) %>%
    select(.variable, age, est, quant)

  vertical_lines <- ggdat %>%
    group_by(.variable) %>%
    summarize(
      cpoint = min(age[est >= 1/2]),
      est = 1/2,
      .groups = 'drop'
    ) %>%
    mutate(cpoint = replace(cpoint, is.infinite(cpoint), NA_real_))

  nudge_x <- switch (
    bp_level,
    'ovrl' = rep(-1, 3),
    'stg1' = c(-1, -1, 1) * 1.2
  )

  nudge_y <- switch (
    bp_level,
    'ovrl' = rep(1, 3) / 100,
    'stg1' = rev(nudge_x) / 100
  )

  ggplot(ggdat) +
    aes(x = age, y = est, col = .variable) +
    geom_line() +
    geom_hline(yintercept = 0.50, col = 'grey', linetype = 2) +
    geom_point(
      data = vertical_lines,
      aes(x = cpoint, y = est),
      size = 2
    ) +
    geom_text(
      data = vertical_lines,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      show.legend = FALSE,
      mapping = aes(x = cpoint,
                    y = est,
                    label = paste0(round(cpoint, 0),'*'))
    ) +
    geom_line(size = 0.9) +
    scale_color_manual(values = color_key[levels(ggdat$.variable)]) +
    theme_bw() +
    labs(
      x = 'Age, years',
      y = 'Estimated probability that 10-year\npredicted risk for ASCVD is \u226510%',
      color = ''
    ) +
    scale_x_continuous(
      breaks = c(40, 45, 50, 55, 60, 65, 70, 75, 79),
      expand = c(0,0)
    ) +
    scale_y_continuous(labels = scales::percent,
                       expand = c(0,0)) +
    theme(
      panel.grid = element_blank(),
      legend.position = c(0.25, 0.85),
      legend.background = element_rect(fill=alpha('white', 0.0))
    )

}
