
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##'
##' @param exams
##' @param current_analysis
##' @param tbl1_overall
##' @param tbl1_s1hyp
##' @param tbl_bpdist
##' @param design
##' @param tbl_exclusions
##' @param fig_hist_ovrl
##' @param fig_hist_stg1
##' @param fig_risk_ovrl_bnry
##' @param fig_risk_stg1_bnry
##' @param tbl_risk_overall
##'
compile_report <- function(exams,
                           fasted_hrs_lower,
                           fasted_hrs_upper,
                           gluc_cutpoint_fasted,
                           gluc_cutpoint_fed,
                           hba1c_cutpoint,
                           egfr_cutpoint,
                           acr_cutpoint,
                           current_analysis,
                           design,
                           tbl1_overall,
                           tbl1_s1hyp,
                           tbl_bpdist,
                           tbl_risk_overall,
                           tbl_exclusions,
                           fig_hist_ovrl,
                           fig_hist_stg1,
                           fig_risk_ovrl_bnry,
                           fig_risk_stg1_bnry,
                           risk_10yr,
                           risk_high) {

  # setup ----

  abbrevs <- c(
    CI = 'confidence interval',
    BP = 'blood pressure',
    CKD = 'chronic kidney disease',
    CVD = 'cardiovascular disease',
    HDL = 'high density lipoprotein',
    ASCVD = 'atherosclerotic cardiovascular disease'
  )

  fts <- c(
    '\u2A',
    '\u2020',
    '\u2021',
    '\uA7',
    '\u2016',
    '\uB6',
    '#',
    '\u2a\u2a',
    '\u2020\u2020',
    '\u2021\u2021'
  )

  tbls_main <- tbls_supp <- tibble(
    object = list(),
    caption = NA_character_,
    reference = NA_character_
  )

  figs_main <- figs_supp <- tibble(
    object = list(),
    caption = character(),
    legend = character(),
    reference = NA_character_
  )

  nhanes_years <-  exams %>%
    map_chr(~glue("{.x}-{.x+1}")) %>%
    glue_collapse(sep = ', ', last = ', and ')

  source_note <- glue("Data are from the {nhanes_years} NHANES exams")

  value_note <- as_paragraph("Table values are mean (standard error) or proportion.")

  ftr_ckd <- as_paragraph(glue('Chronic kidney disease is defined by an albumin-to-creatinine ratio \u2265 {acr_cutpoint} mg/dl or an estimated glomerular filtration rate < {egfr_cutpoint} ml/min/1.73m\u00b2'))

  ftr_diab <- as_paragraph(glue('Diabetes was defined by fasting serum glucose \u2265 {gluc_cutpoint_fasted} mg/dL, non-fasting glucose \u2265 {gluc_cutpoint_fed} mg/dL, HbA1c \u2265 {hba1c_cutpoint}%, or self-reported use of insulin or oral glucose lowering medication.'))

  bp_cat_guide <- as_paragraph(paste(
    "Normal blood pressure: systolic blood pressure < 120 mm Hg and diastolic blood pressure < 80 mm Hg;",
    "Elevated blood pressure: systolic blood pressure from 120 to 129 mm Hg and diastolic blood pressure < 80 mm Hg;",
    "Stage 1 hypertension: systolic blood pressure between 130 and 139 mm Hg and/or diastolic blood pressure between 80 and 89 mm Hg with systolic blood pressure < 140 mm Hg and diastolic blood pressure < 90 mm Hg;",
    "Stage 2 hypertension: systolic blood pressure \u2265 140 mm Hg or diastolic blood pressure \u2265 90 mm Hg.",
    sep = '\n'
  ))

  ftr_cvdHx <- as_paragraph('High atherosclerotic cardiovascular disease risk was defined by a 10-year predicted risk for atherosclerotic cardiovascular disease \u2265 10% or prevalent cardiovascular disease')

  ftr_cvdHx_defn <- as_paragraph('Prevalent cardiovascular disease was defined by self-report of previous heart failure, coronary heart disease, stroke, or myocardial infarction')

  ftr_prisk_defn <- as_paragraph('Predicted risk for atherosclerotic cardiovascular disease was computed using the Pooled Cohort risk equations, based on the guideline by American College of Cardiology / American Heart Association, 2013')

  # sample sizes for table column headers -----------------------------------

  analysis <- current_analysis$data

  N_ovrl_unwtd <- list(
    ovrl         = nrow(analysis),
    diab         = sum(analysis$diabetes == "yes"),
    ckd          = sum(analysis$ckd == "yes"),
    age_gt65     = sum(analysis$age_gt65 == "yes"),
    any          = sum(analysis$any_ckd_diab_age65 == "yes")
  ) %>%
    map_chr(table_value)

  .s1h <- filter(analysis, bp_cat == 'Stage 1 hypertension')

  N_s1h_unwtd <- list(
    ovrl         = nrow(.s1h),
    diab         = sum(.s1h$diabetes == "yes"),
    ckd          = sum(.s1h$ckd == "yes"),
    age_gt65     = sum(.s1h$age_gt65 == "yes"),
    any          = sum(.s1h$any_ckd_diab_age65 == "yes")
  ) %>%
    map_chr(table_value)

  .age40to79 <- subset(analysis, age_40to79 == "yes")

  N_age40to79_unwtd <- list(
    ovrl         = nrow(.age40to79),
    diab         = sum(.age40to79$diabetes == "yes"),
    ckd          = sum(.age40to79$ckd == "yes"),
    age_gt65     = sum(.age40to79$age_gt65 == "yes"),
    any          = sum(.age40to79$any_ckd_diab_age65 == "yes")
  ) %>%
    map_chr(table_value)

  get_wtd_N <- function(svyobj, label){
    as.matrix(svyobj)[label, , drop = TRUE] %>%
      set_names(NULL)
  }

  N_wtd <- list(
    ovrl = sum(analysis$wts_mec_2yr),
    diab = get_wtd_N(
      svytotal(~diabetes, design, na.rm = TRUE), label = 'diabetesyes'
    ),
    ckd = get_wtd_N(
      svytotal(~ckd, design, na.rm = TRUE), label = 'ckdyes'
    ),
    age_gt65 = get_wtd_N(
      svytotal(~age_gt65, design, na.rm = TRUE), label = 'age_gt65yes'
    ),
    any = get_wtd_N(
      svytotal(~any_ckd_diab_age65, design, na.rm = TRUE),
      label = 'any_ckd_diab_age65yes'
    )
  ) %>%
    map_chr(table_value)


  col_labels <- function(N_vals){
    tbl1_overall %>%
      select(-label, -level) %>%
      names() %>%
      paste('\nN =', N_vals) %>%
      set_names(names(N_vals)) %>%
      as.list()
  }

  col_labels_ovrl <- col_labels(N_vals = N_ovrl_unwtd)
  col_labels_s1h <- col_labels(N_vals = N_s1h_unwtd)
  col_labels_age40to79 <- col_labels(N_vals = N_age40to79_unwtd)

  # table 1: characteristics ------------------------------------------------

  .tbl1_overall <- tbl1_overall %>%
    as_grouped_data(groups = 'label') %>%
    remove_empty('rows') %>%
    as_flextable(hide_grouplabel = TRUE) %>%
    add_header_row(values = c("", "Sub-groups"), colwidths = c(2, 4)) %>%
    theme_box() %>%
    set_header_labels(
      'level' = 'Characteristic',
      'Overall' = col_labels_ovrl$ovrl,
      'Diabetes' = col_labels_ovrl$diab,
      'CKD' = col_labels_ovrl$ckd,
      'Age 65+ years' = col_labels_ovrl$age_gt65,
      'Diabetes, CKD, or age 65+ years' = col_labels_ovrl$any
    ) %>%
    padding(i = 4:8, j = 1, padding.left = 10, part = 'body') %>%
    height(height = 1.5, part = 'header') %>%
    width(width = 1.15) %>%
    width(j = ~level, width = 1.75) %>%
    align(align = 'center', part = 'all') %>%
    align(j = 1, align = 'left', part = 'all') %>%
    footnote(i=2, j=1, part='header', value=value_note, ref_symbols=fts[1]) %>%
    footnote(i=2, j=3, part='header', value=ftr_diab, ref_symbols=fts[2]) %>%
    footnote(i=2, j=4, part='header', value=ftr_ckd, ref_symbols=fts[3]) %>%
    footnote(
      i = ~ level == 'Prevalent CVD',
      j = 1,
      part = 'body',
      value = ftr_cvdHx_defn,
      ref_symbols = fts[4]
    ) %>%
    footnote(
      i = 1,
      j = 1,
      part = 'body',
      value = as_paragraph(write_abbrevs(abbrevs[c("CVD", "HDL", "CKD")])),
      ref_symbols = ''
    )

  tbls_main %<>% add_row(
    object = list(.tbl1_overall),
    caption = "Characteristics of US adults overall and in subgroups defined by diabetes, chronic kidney disease, and \u2265 65 years of age",
    reference = 'tab_characteristics'
  )


  # table 2: distribution ---------------------------------------------------

  .tbl_bpdist <- tbl_bpdist %>%
    pivot_wider(names_from = variable, values_from = n) %>%
    flextable() %>%
    set_header_labels(
      bp_cat   = 'Blood pressure category',
      overall          = col_labels_ovrl$ovrl,
      diabetes         = col_labels_ovrl$diab,
      ckd              = col_labels_ovrl$ckd,
      age_group        = col_labels_ovrl$age_gt65,
      any              = col_labels_ovrl$any
    ) %>%
    add_header_row(values = c("", "Sub-groups"), colwidths = c(2, 4)) %>%
    theme_box() %>%
    height(height = 1.5, part = 'header') %>%
    width(width = 1.15) %>%
    width(j = ~bp_cat, width = 1.75) %>%
    align(align = 'center', part = 'all') %>%
    align(j = 1, align = 'left', part = 'all') %>%
    footnote(i=2, j=1, part='header', value=bp_cat_guide, ref_symbols=fts[1]) %>%
    footnote(i=2, j=3, part='header', value=ftr_diab, ref_symbols=fts[2]) %>%
    footnote(i=2, j=4, part='header', value=ftr_ckd, ref_symbols=fts[3]) %>%
    footnote(
      i = 1,
      j = 1,
      part = 'body',
      value = as_paragraph(write_abbrevs(abbrevs[c("CKD")])),
      ref_symbols = ''
    )

  tbls_main %<>% add_row(
    object = list(.tbl_bpdist),
    caption = "Estimated distribution of blood pressure categories among US adults, overall and for subgroups defined by diabetes, chronic kidney disease, and \u2265 65 years of age.",
    reference = 'tab_bpdist'
  )


  # table 3: % with high risk and median ------------------------------

  .tbl_risk_overall <- tbl_risk_overall %>%
    mutate(group = factor(group, levels = c('median_risk', 'high_risk'))) %>%
    arrange(group) %>%
    relocate(Overall, .before = diabetes) %>%
    mutate(
      group = recode(
        group,
        high_risk = paste('Proportion (95% confidence interval) with',
                          risk_high),
        mean_risk = 'Mean (95% confidence interval) predicted risk',
        median_risk = paste('Median (25th - 75th percentile)',
                            risk_10yr,
                            'among those without prevalent CVD')
      )
    ) %>%
    as_grouped_data(groups = 'group') %>%
    as_flextable(hide_grouplabel = TRUE) %>%
    set_header_labels(
      bp_cat = 'Blood pressure category',
      Overall      = col_labels_ovrl$ovrl,
      diabetes     = col_labels_ovrl$diab,
      ckd          = col_labels_ovrl$ckd,
      age_group    = col_labels_ovrl$age_gt65,
      any          = col_labels_ovrl$any
    ) %>%
    add_header_row(values = c("", "Sub-groups"), colwidths = c(2, 4)) %>%
    theme_box() %>%
    bg(i = ~ !is.na(group), bg = 'grey80') %>%
    italic(i = ~ !is.na(group), italic = TRUE) %>%
    height(height = 1.5, part = 'header') %>%
    width(width = 1.15) %>%
    width(j = ~ bp_cat, width = 2) %>%
    align(align = 'center', part = 'all') %>%
    align(j = 1, align = 'left', part = 'all') %>%
    footnote(
      i = 2,
      j = 1,
      part = 'header',
      value = bp_cat_guide,
      ref_symbols = fts[1]
    ) %>%
    footnote(
      i = 2,
      j = 3,
      part = 'header',
      value = ftr_diab,
      ref_symbols = fts[2]
    ) %>%
    footnote(
      i = 2,
      j = 4,
      part = 'header',
      value = ftr_ckd,
      ref_symbols = fts[3]
    ) %>%
    footnote(
      i = 1,
      j = 1,
      part = 'body',
      value = ftr_prisk_defn,
      ref_symbols = fts[5]
    ) %>%
    footnote(
      i = 1,
      j = 1,
      part = 'body',
      value = ftr_cvdHx_defn,
      ref_symbols = fts[4]
    ) %>%
    footnote(
      i = 8,
      j = 1,
      part = 'body',
      value = ftr_cvdHx,
      ref_symbols = fts[6]
    ) %>%
    footnote(
      i = 1,
      j = 1,
      part = 'body',
      value = as_paragraph(write_abbrevs(abbrevs[c("CKD", "CVD", "ASCVD")])),
      ref_symbols = ''
    )

  tbls_main %<>% add_row(
    object = list(.tbl_risk_overall),
    caption = "Median 10-year predicted risk for atherosclerotic cardiovascular disease and proportion of US adults with high atherosclerotic cardiovascular disease risk overall and for subgroups defined by diabetes, chronic kidney disease, and \u2265 65 years of age, stratified by blood pressure categories based on the 2017 American College of Cardiology / American Heart Association blood pressure guidelines.",
    reference = 'tab_risk_overall'
  )

  # table s1: characteristics for SPs with stage 1 hypertension ----

  .tbl1_s1hyp <- tbl1_s1hyp %>%
    as_grouped_data(groups = 'label') %>%
    remove_empty('rows') %>%
    as_flextable(hide_grouplabel = TRUE) %>%
    add_header_row(values = c("", "Sub-groups"), colwidths = c(2, 4)) %>%
    theme_box() %>%
    set_header_labels(
      'level'                   = 'Characteristic',
      'Overall'                 = col_labels_s1h$ovrl,
      'Diabetes'                = col_labels_s1h$diab,
      'CKD'                     = col_labels_s1h$ckd,
      'Age 65+ years'           = col_labels_s1h$age_gt65,
      'Diabetes, CKD, or age 65+ years' = col_labels_s1h$any
    ) %>%
    padding(i = 4:8, j = 1, padding.left = 10, part = 'body') %>%
    height(height = 1.5, part = 'header') %>%
    width(width = 1.15) %>%
    width(j = ~level, width = 1.75) %>%
    align(align = 'center', part = 'all') %>%
    align(j = 1, align = 'left', part = 'all') %>%
    footnote(i=2, j=1, part='header', value=value_note, ref_symbols=fts[1]) %>%
    footnote(i=2, j=3, part='header', value=ftr_diab, ref_symbols=fts[2]) %>%
    footnote(i=2, j=4, part='header', value=ftr_ckd, ref_symbols=fts[3]) %>%
    footnote(
      i = ~ level == 'Prevalent CVD',
      j = 1,
      part = 'body',
      value = ftr_cvdHx_defn,
      ref_symbols = fts[4]
    ) %>%
    footnote(
      i = 1,
      j = 1,
      part = 'body',
      value = as_paragraph(write_abbrevs(abbrevs[c("CVD", "HDL", "CKD")])),
      ref_symbols = ''
    )

  tbls_supp %<>% add_row(
    object = list(.tbl1_s1hyp),
    caption = "Characteristics of US adults with stage 1 hypertension, overall and for subgroups defined by diabetes, chronic kidney disease, and \u2265 65 years of age",
    reference = 'tab_risk_stg1'
  )

  # figure: inclusion / exclusion ----

  figs_supp %<>% add_row(
    object  = list('fig/include_exclude.png'),
    caption = "Flowchart showing the number of NHANES participants included in the current analyses.",
    legend = '\\* The Completed NHANES interview and exam cells include number with the response rate in parentheses.',
    reference = 'fig_include_exclude')

  # figure: cdfs of pcr_risk for all bp categories (A) ----

  figs_main %<>%
    add_row(
      object  = list(fig_hist_ovrl),
      caption = "Estimated distribution of 10-year predicted risk for atherosclerotic cardiovascular disease among US adults with predicted risk < 10%, overall and for subgroups defined by diabetes, chronic kidney disease, and \u2265 65 years of age.",
      legend  = '\\* Results do not include data from survey participants with prevalent cardiovascular disease or 10-year predicted risk for atherosclerotic cardiovascular disease \u2265 10%.',
      reference = 'fig_hist_ovrl'
    )

  figs_supp %<>%
    add_row(
      object  = list(fig_hist_stg1),
      caption = "Estimated distribution of 10-year predicted risk for atherosclerotic cardiovascular disease among US adults with stage 1 hypertension and predicted risk < 10%, overall and for subgroups defined by diabetes, chronic kidney disease, and \u2265 65 years of age.",
      legend  = '\\* Results do not include data from survey participants with prevalent cardiovascular disease or 10-year predicted risk for atherosclerotic cardiovascular disease \u2265 10%.',
      reference = 'fig_hist_stg1'
    )


  # figure: age adjusted predicted risk (binary) overall ----

  figs_main %<>%
    add_row(
      object  = list(fig_risk_ovrl_bnry),
      caption = glue(
        "Estimated Probability of ten-year predicted risk for",
        "atherosclerotic cardiovascular disease \u2265 10% by age for",
        "US adults with diabetes, with chronic kidney disease, and without",
        "diabetes or chronic kidney disease.",
        .sep = ' '),
      legend = '\\* Age at which 50% of the population is expected to have a  predicted 10-year risk for atherosclerotic cardiovascular disease \u2265 10%.',
      reference = 'fig_risk_ovrl'
    )

  # figure: age adjusted predicted risk (binary) in S1 hypertensives ----

  figs_supp %<>%
    add_row(
      object  = list(fig_risk_stg1_bnry),
      caption = glue(
        "Estimated Probability of ten-year predicted risk for",
        "atherosclerotic cardiovascular disease \u2265 10% by age among",
        "US adults with stage 1 hypertension and diabetes,",
        "chronic kidney disease, and without",
        "diabetes or chronic kidney disease.",
        .sep = ' '),
      legend = '\\* Age at which 50% of the population is expected to have a predicted 10-year risk for atherosclerotic cardiovascular disease \u2265 10%.',
      reference = 'fig_risk_stg1'
    )


  # format and bind ---------------------------------------------------------

  tbls <- bind_rows(table_main = tbls_main,
                    table_supplement = tbls_supp,
                    .id = 'split_me') %>%
    group_by(split_me) %>%
    mutate(
      pre_cap = glue("Table {1:n()}"),
      pre_cap = if_else(split_me == 'table_supplement',
                        true = str_replace(pre_cap, 'Table ', 'Table S'),
                        false = as.character(pre_cap)),
      object = map(object,
                   table_polisher,
                   font_size = 11,
                   font_name = "Calibri")
    )

  figs <- bind_rows(figure_main = figs_main,
                    figure_supplement = figs_supp,
                    .id = 'split_me') %>%
    group_by(split_me) %>%
    mutate(
      pre_cap = glue("Figure {1:n()}"),
      pre_cap = if_else(split_me == 'figure_supplement',
                        true = str_replace(pre_cap, 'Figure ', 'Figure S'),
                        false = as.character(pre_cap))
    )

  bind_rows(tbls, figs) %>%
    separate(split_me, into = c('object_type', 'location')) %>%
    mutate(caption = glue("{pre_cap}: {caption}"), .after = object)

}
