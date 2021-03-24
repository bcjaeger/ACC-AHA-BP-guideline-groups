
# TODO:

# look at characteristics of stage 1 hypertension with <10% risk and >= 10% risk
# look at age/sbp/dbp among sub-groups defined by ACC/AHA BP groups and diabetes/ckd/age 65+

# draft the take-home message
# format for JACC (start with methods)


# who has uncontrolled BP and is unaware
# who is taking BP meds and has uncontrolled BP

the_plan <- drake_plan(

  exams = c(2013, 2015, 2017),

  # phrases to be used consistently throughout project
  risk_10yr = '10-year predicted risk for ASCVD',
  risk_high = 'high ASCVD risk',

  # Demographics ----
  demo = clean_demo(exams),

  # Exams ----
  exam_bp = clean_exam_bp(exams),

  # Labs ----
  labs_acr     = clean_labs_acr(exams),
  labs_biopro  = clean_labs_biopro(exams),
  labs_fasting = clean_labs_fasting(exams),
  labs_ghb     = clean_labs_ghb(exams),
  labs_hdl     = clean_labs_hdl(exams),
  labs_tchol   = clean_labs_tchol(exams),

  # Questionnaires ----
  qx_medical_status         = clean_qx_medical_status(exams),
  qx_smoking_status         = clean_qx_smoking_status(exams),
  qx_health_insurance       = clean_qx_health_insurance(exams),
  qx_diabetes               = clean_qx_diabetes(exams),
  qx_high_blood_pressure    = clean_qx_high_bp(exams),
  qx_healthcare_utilization = clean_qx_healthcare_utilization(exams),

  data_pooled = reduce(
    .x = list(
      demo,
      exam_bp,
      labs_acr,
      labs_biopro,
      labs_fasting,
      labs_ghb,
      labs_hdl,
      labs_tchol,
      qx_medical_status,
      qx_smoking_status,
      qx_health_insurance,
      qx_diabetes,
      qx_high_blood_pressure,
      qx_healthcare_utilization
    ),
    .f = left_join,
    by = c('exam', 'seqn')
  ),

  fasted_hrs_lower = 8,
  fasted_hrs_upper = 24,
  gluc_cutpoint_fasted = 126,
  gluc_cutpoint_fed = 200,
  hba1c_cutpoint = 6.5,
  egfr_cutpoint = 60,
  acr_cutpoint = 30,

  data_derived = data_pooled %>%
    derive_diabetes(
      fasted_hrs_lower = fasted_hrs_lower,
      fasted_hrs_upper = fasted_hrs_upper,
      gluc_cutpoint_fasted = gluc_cutpoint_fasted,
      gluc_cutpoint_fed = gluc_cutpoint_fed,
      hba1c_cutpoint = hba1c_cutpoint
    ) %>%
    derive_egfrCKDepi(egfr_cutpoint = egfr_cutpoint) %>%
    derive_ascvd_risk_pcr() %>%
    derive_others(exams = exams, acr_cutpoint = acr_cutpoint),

  inclusion_by_exam = split(data_derived, data_derived$exam) %>%
    map_dfr(~include_exclude(.x)$tbl, .id = 'exam') %>%
    select(-n_removed) %>%
    pivot_wider(names_from = exam, values_from = sample_size),

  current_analysis = include_exclude(data_derived),

  include_tbls = tabulate_inclusion(data_derived),

  design_overall = svydesign(ids = ~ psu, strata = ~strata,
                             weights = ~wts_mec_2yr, nest = TRUE,
                             data = current_analysis$data),

  design_overall_supp = svydesign(
    ids = ~ psu,
    strata = ~strata,
    weights = ~wts_mec_2yr,
    nest = TRUE,
    data = mutate(current_analysis$data,
                  pcr_gteq_10 = pcr_gteq_10_yadlowsky,
                  pcr_highrisk = pcr_highrisk_yadlowsky)
  ),

  design_s1hyp = subset(design_overall, bp_cat == 'Stage 1 hypertension'),

  tbl1_variables = c(
    "Age, years" = "age",
    "Gender" = "sex",
    "Race / ethnicity" = "race_ethnicity",
    "Current smoker" = "smk_current",
    "Total cholesterol, mg/dl" = "chol_total_mgdl",
    "HDL-cholesterol, mg/dl" = "chol_hdl_mgdl",
    "Systolic blood pressure, mm Hg" = "bp_sys_mmhg",
    "Diastolic blood pressure, mm Hg" = "bp_dia_mmhg",
    "Antihypertensive medication use" = "meds_bp",
    "Diabetes" = "diabetes",
    "CKD" = "ckd",
    "Aged 65+ years" = "age_gt65",
    "Diabetes, chronic kidney disease, or age 65+ years" = 'any_ckd_diab_age65',
    "Clinical CVD" = "ever_had_ascvd"
  ),

  # Tables ----

  tbl1_overall = tabulate_characteristics(design_overall,
                                          tbl1_variables),

  tbl1_s1hyp = tabulate_characteristics(design_s1hyp,
                                        tbl1_variables),

  tbl_bpdist = tabulate_bpdist(design_overall),

  tbl_risk_overall = tabulate_risk_summary(design_overall),
  tbl_risk_overall_supp = tabulate_risk_summary(design_overall_supp),

  # Figures ----

  color_key = c("Overall" = "#5DA5DAFF",
                "Diabetes" = "#FAA43AFF",
                "Chronic kidney disease" = "#60BD68FF",
                "Aged 65+ years" = "#F15854FF",
                "Any listed condition" = "#B276B2FF",
                "No diabetes or chronic kidney disease" = "#8D4B08FF"),

  qts = seq(0.001, 0.999, by = 0.0001),

  fig_text = element_text(#family = 'Times',
                          size = 12,
                          color = 'black'),

  fig_hist = visualize_risk_hist(design_overall, qts, fig_text),

  age_breaks = seq(40, 79, by = .001),

  risk_models = estimate_risk(design_overall, age_breaks),

  fig_risk_ovrl_bnry = visualize_risk_bnry(risk_models,
                                           bp_level = 'ovrl',
                                           color_key = color_key,
                                           fig_text = fig_text),

  fig_risk_stg1_bnry = visualize_risk_bnry(risk_models,
                                           bp_level = 'stg1',
                                           color_key = color_key,
                                           fig_text = fig_text),


  inline = make_inline_results(design_overall, design_s1hyp, risk_models),

  fig_central_illustration = visualize_central_illustration(tbl_bpdist$table,
                                                            inline),

  report = compile_report(
    exams,
    fasted_hrs_lower,
    fasted_hrs_upper,
    gluc_cutpoint_fasted,
    gluc_cutpoint_fed,
    hba1c_cutpoint,
    egfr_cutpoint,
    acr_cutpoint,
    current_analysis,
    design_overall,
    tbl1_overall$table,
    tbl1_s1hyp$table,
    tbl_bpdist$table,
    tbl_risk_overall,
    tbl_risk_overall_supp,
    current_analysis$tbl,
    fig_hist$fig_3row$ovrl,
    fig_hist$fig_3row$stg1,
    fig_risk_ovrl_bnry,
    fig_risk_stg1_bnry,
    risk_10yr = risk_10yr,
    risk_high = risk_high
  ),

  # Report ----

  manuscript = target(
    command = {
      rmarkdown::render(knitr_in("doc/ACCAHA_BP_groups.Rmd"))
      file_out("doc/ACCAHA_BP_groups.docx")
    }
  ),

  seminar = target(
    command = {
      rmarkdown::render(knitr_in("docs/index.Rmd"))
      file_out("doc/seminar/ACCAHA_BP_groups.html")
    }
  ),

  readme = target(
    command = {
      rmarkdown::render(knitr_in("README.Rmd"))
      file_out("README.md")
    }
  )



)
