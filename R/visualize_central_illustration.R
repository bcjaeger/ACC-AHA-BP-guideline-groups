##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param inline
##' @return
##' @export
visualize_central_illustration <- function(tbl_bpdist, inline) {

  bar_layer_overall <- tbl_bpdist %>%
    filter(variable == 'overall') %>%
    rename(n_chr = n) %>%
    mutate(
      n_dbl = as.numeric(str_remove(n_chr, '%')),
      bp_cat = factor(
        bp_cat,
        levels = c(
          "Taking antihypertensive medication",
          "Stage 2 hypertension",
          "Stage 1 hypertension",
          "Elevated blood pressure",
          "Normal blood pressure"
        ),
        labels = c(
          "Taking antihyper-\ntensive medication",
          "Stage 2\nhypertension",
          "Stage 1\nhypertension",
          "Elevated\nblood pressure",
          "Normal\nblood pressure"
        )
      )
    ) %>%
    arrange(bp_cat)

  text <- data.frame(
    n_dbl = c(22, 36, 52),
    bp_cat = 'Stage 1\nhypertension',
    n_chr = c("diabetes,",
              "CKD,",
              "age \u226565yrs:"),
    prevHighRisk = c(
      no_ci(inline$prevHighRiskS1hDiabetes),
      no_ci(inline$prevHighRiskS1hCkd),
      no_ci(inline$prevHighRiskS1hAge65)
    )
  ) %>%
    unite(n_chr, prevHighRisk,
          col = 'n_chr',
          sep = '\n') %>%
    mutate(n_chr = paste0(n_chr, c(',', ',', '.')))

  text_spanner <- data.frame(
    n_dbl = 37.5,
    bp_cat = 'Stage 1\nhypertension',
    n_chr = c("Percent at high ASCVD risk with")
  )

  bar_plot <- ggplot(bar_layer_overall) +
    aes(x = bp_cat, y = n_dbl, fill = bp_cat, label = n_chr) +
    geom_bar(stat = 'identity', show.legend = FALSE) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12,
                                     color = 'black',
                                     family = "serif"),
          plot.title = element_text(size = 20,
                                    color = 'black',
                                    family = "serif",
                                    hjust = -0.3),
          axis.ticks = element_blank()) +
    geom_text(aes(y=-6), size = 7.25, family = 'serif') +
    scale_y_continuous(limits = c(-8, 60)) +
    labs(x = '', y = '') +
    scale_fill_discrete()+
    geom_text(data = text,
              size = 5.5,
              family = 'serif',
              nudge_x = -.18,
              show.legend = FALSE) +
    geom_text(data = text_spanner,
              size = 5.5,
              family = 'serif',
              nudge_x = .55) +
    coord_flip() +
    ggtitle(label = 'US adults aged 40-79 years')


  bar_plot


}
