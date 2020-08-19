##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param risk_models
##' @param bp_level
##' @param color_key
visualize_risk_ctns <- function(risk_models,
                                bp_level,
                                color_key) {

  .design <- risk_models %>%
    filter(bp_cat == bp_level) %>%
    slice(1) %>%
    pull(.design) %>%
    .[[1]]

  .designs <- list(
    diabetes_and_ckd = subset(.design, diabetes_and_ckd == "yes"),
    diabetes = subset(.design, diabetes == "yes"),
    ckd = subset(.design, ckd == "yes"),
    Overall = .design
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
        levels = c('diabetes_and_ckd','diabetes','ckd','Overall')
      ),
      .variable = fct_recode(
        .f = .variable,
        'Diabetes' = 'diabetes',
        'Diabetes and chronic kidney disease' = 'diabetes_and_ckd',
        'Chronic kidney disease' = 'ckd'
      )
    )


  ggdat <- risk_models %>%
    filter(bp_cat == bp_level) %>%
    select(.variable, ctns) %>%
    unnest(cols = ctns) %>%
    mutate(
      .variable = factor(
        .variable,
        levels = c('diabetes_and_ckd','diabetes','ckd','Overall')
      ),
      .variable = fct_recode(
        .f = .variable,
        'Diabetes' = 'diabetes',
        'Diabetes and chronic kidney disease' = 'diabetes_and_ckd',
        'Chronic kidney disease' = 'ckd'
      )
    ) %>%
    filter(est < 0.175)

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
      cpoint = min(age[est >= 0.10]),
      est = 0.10,
      .groups = 'drop'
    )

  ggplot(ggdat) +
    aes(x = age, y = est, col = .variable) +
    geom_line() +
    geom_hline(yintercept = 0.10, col = 'grey', linetype = 2) +
    geom_segment(
      data = vertical_lines,
      aes(x = cpoint, y = 0, xend = cpoint, yend = 0.10),
      col = 'grey',
      linetype = 2
    ) +
    geom_point(
      data = vertical_lines,
      aes(x = cpoint, y = est),
      size = 2
    ) +
    geom_line(size = 0.9) +
    scale_color_manual(values = color_key[levels(ggdat$.variable)]) +
    theme_bw() +
    labs(
      x = 'Age, years',
      y = 'Expected 10-year predicted risk for ASCVD',
      color = ''
    ) +
    scale_x_continuous(
      breaks = vertical_lines$cpoint,
      labels = floor(vertical_lines$cpoint),
      expand = c(0,0)
    ) +
    scale_y_continuous(labels = scales::percent,
                       expand = c(0,0)) +
    theme(
      panel.grid = element_blank(),
      legend.position = c(0.25, 0.90),
      legend.background = element_rect(fill=alpha('white', 0.0))
    )

}
