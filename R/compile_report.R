
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param tbl1_overall
##' @param tbl1_s1hyp
##' @param tbl_bpdist
##' @param tbl_risk_overall
##' @param tbl_risk_40to79
compile_report <- function(exams,
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
                           inline) {

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

  ftr_ckd <- as_paragraph('Chronic kidney disease is defined by an albumin-to-creatinine ratio \u2265 30 mg/dl or an estimated glomerular filtration rate <60 ml/min/1.73m\u00b2')

  ftr_diab <- as_paragraph('Diabetes was defined by fasting serum glucose \u2265 126 mg/dL, non-fasting glucose \u2265 200 mg/dL, HbA1c \u2265 6.5%, or self-reported use of insulin or oral glucose lowering medication.')


  bp_cat_guide <- as_paragraph(paste(
    "Normal blood pressure: systolic/diastolic blood pressure < 120/80 mm Hg;",
    "Elevated blood pressure: systolic/diastolic blood pressure 120-129/<80 mm Hg;",
    "Stage 1 hypertension: systolic/diastolic blood pressure 130-139/80-89 mm Hg;",
    "Stage 2 hypertension: systolic/diastolic blood pressure \u2265 140/90 mm Hg.",
    sep = '\n'
  ))

  ftr_cvdHx <- as_paragraph('Data from survey participants with prevalent cardiovascular disease were not included for these statistics')

  ftr_cvdHx_defn <- as_paragraph('Prevalent cardiovascular disease was defined by self-report of previous heart failure, coronary heart disease, stroke, or myocardial infarction')

  ftr_prisk_defn <- as_paragraph('Predicted risk for cardiovascular disease was computed using the Pooled Cohort Risk equations, based on the guideline by American College of Cardiology / American Heart Association, 2013')

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
    caption = "Characteristics of US adults overall and with diabetes, chronic kidney disease, and \u2265 65 years of age.",
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
    caption = "Estimated distribution of blood pressure categories among US adults, overall and for subgroups with diabetes, chronic kidney disease, and \u2265 65 years of age.",
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
        high_risk = 'Proportion (95% confidence interval) with 10-year predicted risk for ASCVD \u226510% or prevalent cardiovascular disease',
        mean_risk = 'Mean (95% confidence interval) predicted risk',
        median_risk = 'Median (25th - 75th percentile) predicted risk')
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
      i = 8,
      j = 1,
      part = 'body',
      value = ftr_cvdHx_defn,
      ref_symbols = fts[4]
    ) %>%
    footnote(
      i = 8,
      j = 1,
      part = 'body',
      value = ftr_prisk_defn,
      ref_symbols = fts[5]
    ) %>%
    footnote(
      i = 1,
      j = 1,
      part = 'body',
      value = ftr_cvdHx,
      ref_symbols = fts[6]
    ) %>%
    footnote(
      i = 1,
      j = 1,
      part = 'body',
      value = as_paragraph(write_abbrevs(abbrevs[c("CKD", "ASCVD")])),
      ref_symbols = ''
    )

  tbls_main %<>% add_row(
    object = list(.tbl_risk_overall),
    caption = "Median predicted risk for cardiovascular disease and proportion of US adults with predicted risk \u2265 10% overall and among those with diabetes, chronic kidney disease, and \u2265 65 years of age, stratified by categorization of blood pressure according to the 2017 American College of Cardiology / American Heart Association blood pressure guidelines.",
    reference = 'tab_risk_overall'
  )

  # table: exclusions for current analysis (deprecated) ----

  ### Dropped this in favor of a figure
  # .tbl_exclusions <- tbl_exclusions %>%
  #   mutate(across(where(is.numeric), ~table_value(as.integer(.x)))) %>%
  #   flextable(theme_fun = theme_box) %>%
  #   set_header_labels(
  #     'label' = "Criteria",
  #     'sample_size' = "N included",
  #     'n_removed' = "N excluded"
  #   ) %>%
  #   height(height = 1.5, part = 'header') %>%
  #   width(width = 1.15) %>%
  #   width(j = ~label, width = 4.5) %>%
  #   align(align = 'center', part = 'all') %>%
  #   align(j = 1, align = 'left', part = 'all')
  #
  # tbls_supp %<>% add_row(
  #   object = list(.tbl_exclusions),
  #   caption = "Participants included in the current analysis"
  # )

  # table s1: characteristics for SPs with stage 1 hypertension ----

  .tbl1_s1hyp <- tbl1_s1hyp %>%
    as_grouped_data(groups = 'label') %>%
    remove_empty('rows') %>%
    as_flextable(hide_grouplabel = TRUE) %>%
    add_header_row(values = c("", "Sub-groups"), colwidths = c(2, 4)) %>%
    theme_box() %>%
    set_header_labels(
      'level'                               = 'Characteristic',
      'Overall'                             = col_labels_s1h$ovrl,
      'Diabetes'                            = col_labels_s1h$diab,
      'Chronic kidney disease'              = col_labels_s1h$ckd,
      'Age 65+ years'                       = col_labels_s1h$age_gt65,
      'Any preceding condition'             = col_labels_s1h$any
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
    caption = "Characteristics of US adults with stage 1 hypertension, overall and with diabetes, chronic kidney disease, \u2265 65 years of age, or any of the three preceding conditions",
    reference = 'tab_risk_stg1'
  )

  # figure: inclusion / exclusion ----

  figs_supp %<>% add_row(
    object  = list('fig/include_exclude.png'),
    caption = "Flowchart showing the number of NHANES participants included in the current analyses.",
    legend = 'BP: blood pressure; NHANES: National Health and Nutrition Examination Survey. The Completed NHANES interview and exam cells include number with the response rate in parentheses.',
    reference = 'fig_include_exclude')

  # figure: cdfs of pcr_risk for all bp categories (A) ----

  figs_main %<>%
    add_row(
      object  = list(fig_hist_ovrl),
      caption = glue("Estimated distribution of 10-year predicted ",
                     "atherosclerotic cardiovascular disease risk ",
                     "among US adults with ",
                     "predicted risk < 10% overall and for those with ",
                     "diabetes, chronic kidney disease, \u2265 65 ",
                     "years of age, or any of the preceding conditions."),
      legend  = 'Results do not include data from survey participants with prevalent cardiovascular disease or 10-year predicted risk for atherosclerotic cardiovascular disease \u2265 10%.',
      reference = 'fig_hist_ovrl'
    )

  figs_supp %<>%
    add_row(
      object  = list(fig_hist_stg1),
      caption = glue("Estimated distribution of 10-year predicted ",
                     "cardiovascular risk among US adults with ",
                     "stage 1 hypertension and ",
                     "predicted risk < 10% overall and for those with ",
                     "diabetes, chronic kidney disease, \u2265 65 ",
                     "years of age, or any of the preceding conditions."),
      legend  = 'Results do not include data from survey participants with prevalent cardiovascular disease or 10-year predicted risk for cardiovascular disease \u2265 10%.',
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
      legend = 'Age at which 50% of the population is expected to have a  predicted 10-year risk for atherosclerotic cardiovascular disease \u2265 10%.',
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
        "chronic kidney disease, and with without",
        "diabetes or chronic kidney disease.",
        .sep = ' '),
      legend = 'Age at which 50% of the population is expected to have a predicted 10-year risk for atherosclerotic cardiovascular disease \u2265 10%.',
      reference = 'fig_risk_stg1'
    )


  # Add pre-caption ---------------------------------------------------------

  tbls_main %<>%
    mutate(
      pre_cap = glue("Table {1:nrow(tbls_main)}"),
      caption = glue("{pre_cap}: {caption}")
    )

  if(nrow(tbls_supp) > 0) tbls_supp %<>%
    mutate(
      pre_cap = glue("Table S{1:nrow(tbls_supp)}"),
      caption = glue("{pre_cap}: {caption}")
    )


  if(nrow(figs_main) > 0) figs_main %<>%
    mutate(
      pre_cap = glue("Figure {1:nrow(.)}"),
      caption = glue("{pre_cap}: {caption}")
    )

  if(nrow(figs_supp) > 0) figs_supp %<>%
    mutate(
      pre_cap = glue("Figure S{1:n()}"),
      caption = glue("{pre_cap}: {caption}")
    )

  font_size = 11
  font_name = "Calibri"

  # uniform font and font size for tables ----

  tbls_main %<>%
    mutate(
      object = map(
        .x = object,
        .f = ~ .x %>%
          font(fontname = font_name, part = 'all') %>%
          fontsize(size = font_size, part = 'all') %>%
          height(height = 2, part = 'footer')
      )
    )

  tbls_supp %<>%
    mutate(
      object = map(
        .x = object,
        .f = ~ .x %>%
          font(fontname = font_name, part = 'all') %>%
          fontsize(size = font_size, part = 'all')
      )
    )

  bind_rows(
    table_main = tbls_main,
    figure_main = figs_main,
    table_supplement = tbls_supp,
    figure_supplement = figs_supp,
    .id = 'split_me'
  ) %>%
    separate(split_me, into = c('object_type', 'location'))

}
