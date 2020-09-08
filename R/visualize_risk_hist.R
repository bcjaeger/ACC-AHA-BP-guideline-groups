##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param design_overall
##' @param qts
visualize_risk_hist <- function(design, qts) {

  # do not include participants with CVD history
  design <- subset(design, ever_had_ascvd == 'no' & ascvd_risk_pcr <= 0.10)


  .design <- list(
    overall  = design,
    diabetes = subset(design, diabetes == "yes"),
    ckd      = subset(design, ckd == "yes"),
    age      = subset(design, age_gt65 == "yes"),
    any      = subset(design, any_ckd_diab_age65 == "yes")
  )

  .design_s1h <- map(.design, subset, bp_cat == 'Stage 1 hypertension')

  names(.design_s1h) %<>% paste0("_stg1")
  names(.design) %<>% paste0("_ovrl")

  .hist <- map_dfr(
    .x = c(.design, .design_s1h),
    .f = ~ {
      hist_svy <- svyhist(~ascvd_risk_pcr, design = .x,
                          breaks = c(0, 25, 50, 75, 100) / 1000)
      as_tibble(hist_svy[c('counts', 'mids')]) %>%
        transmute(
          x = mids,
          perc_val = counts / sum(counts),
          perc_lab = table_glue("{100 * perc_val}%")
        )
    },
    .id = 'panel'
  ) %>%
    separate(panel, into = c('panel', 'subpop')) %>%
    mutate(
      panel = factor(panel),
      panel = fct_relevel(panel, "overall", "diabetes", "ckd", "age", "any"),
      panel = fct_recode(
        panel,
        "Overall" = "overall",
        "Diabetes" = "diabetes",
        "Chronic kidney disease" = "ckd",
        "Aged 65+ years" = "age",
        "Any preceding condition" = "any"
      )
    )

  # to make separate figures

  .hist %>%
    split(f = .$subpop) %>%
    map(
      ~ ggplot(.x) +
        aes(x = x, y = perc_val, label = perc_lab) +
        geom_bar(stat = 'identity', fill = 'grey80',
                 color = 'grey80', alpha = 0.50) +
        geom_text(vjust = -1) +
        theme_bw() +
        theme(panel.grid = element_blank(),
              text = element_text(size = 12)) +
        scale_x_continuous(labels = percent) +
        scale_y_continuous(labels = percent,
                           limits = c(0,1),
                           breaks = c(0, 0.25, 0.50, 0.75, 1),
                           expand = c(0,0)) +
        labs(x = 'Predicted 10-year CVD risk, %',
             y = 'Estimated % of US adults*') +
        facet_wrap(~panel, ncol = 2, scales = 'free')
    )



  # to make it all go into one figure
  # ggplot(.hist) +
  #   aes(x = x, y = perc_val, label = perc_lab, col = subpop, fill = subpop) +
  #   geom_bar(stat = 'identity', alpha = 0.50,
  #            position = 'dodge') +
  #   geom_text(vjust = -1, position = position_dodge(width = 0.023)) +
  #   theme_bw() +
  #   theme(panel.grid = element_blank()) +
  #   scale_x_continuous(labels = percent) +
  #   scale_y_continuous(labels = percent, limits = c(0,1), expand = c(0,0)) +
  #   labs(x = 'Predicted ASCVD risk, %',
  #        y = 'Estimated % of US adults*') +
  #   facet_wrap(~panel, ncol = 2)






}



