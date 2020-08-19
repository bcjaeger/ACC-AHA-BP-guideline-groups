##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param design_overall
##' @param qts
visualize_risk_cdfs <- function(design, qts) {

  # do not include participants with CVD history
  design <- subset(design, ever_had_ascvd == 'no' & ascvd_risk_pcr <= 0.10)

  .cdf <- list(
    overall = svyquantile(~ascvd_risk_pcr, design, quantiles = qts),
    diabetes = svyquantile(
      ~ ascvd_risk_pcr,
      design = subset(design, diabetes == "yes"),
      quantiles = qts
    ),
    ckd = svyquantile(
      ~ ascvd_risk_pcr,
      design = subset(design, ckd == "yes"),
      quantiles = qts
    ),
    diabetes_and_ckd = svyquantile(
      ~ ascvd_risk_pcr,
      design = subset(design, diabetes_and_ckd == "yes"),
      quantiles = qts
    ),
    age = svyquantile(
      ~ ascvd_risk_pcr,
      design = subset(design, age_gt65 == "yes"),
      quantiles = qts
    ),
    any = svyquantile(
      ~ ascvd_risk_pcr,
      design = subset(design, any_ckd_diab_age65 == "yes"),
      quantiles = qts
    )
  ) %>%
    map_df(
      ~mutate(enframe(as.numeric(.x), name = 'quantile'), quantile = qts),
      .id = 'variable'
    )

  .mdl <- glm(value ~ variable * poly(quantile,3),
             data = .cdf,
             family = Gamma(link = 'log'))

  # enforce monotonic increases
  enforce_monotone <- function(x){
    for(i in 2:length(x)){
      x[i] <- max(x[i], x[i-1])
    }
    x
  }

  ggdat <- .cdf %>%
    mutate(
      prd = predict(.mdl, type = 'response'),
      variable = fct_relevel(
        variable,
        'age',
        'any',
        'diabetes',
        'ckd',
        'diabetes_and_ckd',
        'overall'
      ),
      variable = fct_recode(
        .f = variable,
        'Aged 65+ years' = 'age',
        'Any listed condition' = 'any',
        'Diabetes' = 'diabetes',
        'Diabetes and CKD' = 'diabetes_and_ckd',
        'Chronic kidney disease' = 'ckd',
        'Overall' = 'overall'
      )
    ) %>%
    filter(prd < 0.10) %>%
    group_by(variable) %>%
    mutate(prd = enforce_monotone(prd))

  vertical_lines <- ggdat %>%
    group_by(variable) %>%
    summarize(
      cpoint_75 = min(quantile[prd >= 0.075]),
      est_75 = min(prd[prd >= 0.075]),
      cpoint_50 = min(quantile[prd >= 0.05]),
      est_50 = min(prd[prd >= 0.05]),
      cpoint_25 = min(quantile[prd >= 0.025]),
      est_25 = min(prd[prd >= 0.025])
    ) %>%
    pivot_longer(cols = -variable) %>%
    separate(name, into = c('type', 'yend'), sep = '_') %>%
    mutate(yend = as.numeric(yend)/1000) %>%
    pivot_wider(names_from = type, values_from = value)

  plot_output <- ggdat %>%
    group_by(variable) %>%
    nest() %>%
    rename(cdf_data = data) %>%
    left_join(nest(group_by(vertical_lines, variable))) %>%
    ungroup() %>%
    rename(line_data = data) %>%
    mutate(
      panel_letters = LETTERS[1:n()],
      plot = pmap(
        .l = list(panel_letters, variable, cdf_data, line_data),
        .f = function(.letter, .variable, gdat, vdat){

          ylab = if_else(
            .letter %in% c("E","F"),
            true = '10-year predicted risk for ASCVD, %',
            false = ''
          )

          xlab = if_else(
            .letter %in% c("C"),
            true = 'Estimated % of US adults\nwith predicted risk \u2264 value on x-axis',
            false = ''
          )

          #ylab = '10-year predicted risk for ASCVD'
          #xlab = 'Estimated percent of US adults with predicted risk > X'

          .perc_invert <- function(x){

            percent(1-x, accuracy = 1, suffix = '')

          }

          .perc_decimal <- function(x){
            x_lt_10 <- x < 0.1
            x_eq_00 <- x <= 0.001
            out <- percent(x, accuracy = 1, suffix = '')
            out[x_lt_10] <- percent(x[x_lt_10], accuracy = .1, suffix = '')
            out[x_eq_00] <- '0'
            out
          }

          ggplot(gdat, aes(x = quantile, y = prd)) +
            geom_segment(data = vdat,
                         aes(x = cpoint, y = 0, xend = cpoint, yend = est),
                         col = 'grey', linetype = 2) +
            geom_segment(data = vdat,
                         aes(x = 0, y = est, xend = cpoint, yend = est),
                         col = 'grey', linetype = 2) +
            geom_line(size = 0.9) +
            labs(y = ylab,
                 x = xlab,
                 title = glue("{.letter}: {.variable}")) +
            scale_y_continuous(breaks = c(min(gdat$prd), vdat$est, 0.1),
                               expand = c(1/1000, 1/1000),
                               labels = .perc_decimal,
                               limits = c(0, .1)) +
            scale_x_continuous(breaks = c(0, vdat$cpoint, 1),
                               expand = c(1/100, 1/100),
                               labels = .perc_decimal,
                               limits = c(0, 1)) +
            theme_bw() +
            theme(panel.grid = element_blank(),
                  text = element_text(size = 10)) +
            coord_flip()
        })
    )

  plot_A <- with(plot_output,
       ( plot[[1]] + plot[[2]] ) /
       ( plot[[3]] + plot[[4]] ) /
       ( plot[[5]] + plot[[6]] ) )

  plot_output <- ggdat %>%
    group_by(variable) %>%
    nest() %>%
    rename(cdf_data = data) %>%
    left_join(nest(group_by(vertical_lines, variable))) %>%
    ungroup() %>%
    rename(line_data = data) %>%
    mutate(
      panel_letters = LETTERS[1:n()],
      plot = pmap(
        .l = list(panel_letters, variable, cdf_data, line_data),
        .f = function(.letter, .variable, gdat, vdat){

          ylab = if_else(
            .letter %in% c("E","F"),
            true = '10-year predicted risk for ASCVD, %',
            false = ''
          )

          xlab = if_else(
            .letter %in% c("C"),
            true = 'Estimated % of US adults\nwith predicted risk > X',
            false = ''
          )

          #ylab = '10-year predicted risk for ASCVD'
          #xlab = 'Estimated percent of US adults with predicted risk > X'

          .perc_invert <- function(x){

            percent(1-x, accuracy = 1, suffix = '')

          }

          .perc_decimal <- function(x){
            x_lt_10 <- x < 0.1
            x_eq_00 <- x <= 0.001
            out <- percent(x, accuracy = 1, suffix = '')
            out[x_lt_10] <- percent(x[x_lt_10], accuracy = .1, suffix = '')
            out[x_eq_00] <- '0'
            out
          }

          ggplot(gdat, aes(x = quantile, y = prd)) +
            geom_segment(data = vdat,
                         aes(x = cpoint, y = 0, xend = cpoint, yend = est),
                         col = 'grey', linetype = 2) +
            geom_segment(data = vdat,
                         aes(x = 1, y = est, xend = cpoint, yend = est),
                         col = 'grey', linetype = 2) +
            geom_line(size = 0.9) +
            labs(y = ylab,
                 x = xlab,
                 title = glue("{.letter}: {.variable}")) +
            scale_y_continuous(breaks = c(min(gdat$prd), vdat$est, 0.1),
                               expand = c(1/1000, 1/1000),
                               labels = .perc_decimal,
                               limits = c(0, .1)) +
            scale_x_continuous(trans = 'reverse',
                               breaks = c(0, vdat$cpoint, 1),
                               labels = .perc_invert) +
            theme_bw() +
            theme(panel.grid = element_blank(),
                  text = element_text(size = 10)) +
            coord_flip()
        })
    )

  plot_B <- with(plot_output,
                 ( plot[[1]] + plot[[2]] ) /
                   ( plot[[3]] + plot[[4]] ) /
                   ( plot[[5]] + plot[[6]] ) )

  list(plot_A = plot_A, plot_B = plot_B)

}



