
# TODO:

# look at characteristics of stage 1 hypertension with <10% risk and >= 10% risk
# look at age/sbp/dbp among sub-groups defined by ACC/AHA BP groups and diabetes/ckd/age 65+

# draft the take-home message
# format for JACC (start with methods)


# who has uncontrolled BP and is unaware
# who is taking BP meds and has uncontrolled BP

the_plan <- drake_plan(

  exams = c(2013, 2015, 2017),
  decimals = c(1, 1, 1),

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

  data_derived = data_pooled %>%
    derive_diabetes() %>%
    derive_egfrCKDepi() %>%
    derive_ascvd_risk_pcr(set_miss_to_no = 'diabetes') %>%
    derive_others(exams = exams),

  current_analysis = include_exclude(data_derived),

  include_tbls = tabulate_inclusion(data_derived),

  design_overall = svydesign(ids = ~ psu, strata = ~strata,
                             weights = ~wts_mec_2yr, nest = TRUE,
                             data = current_analysis$data),

  design_s1hyp = subset(design_overall, bp_cat == 'Stage 1 hypertension'),

  tbl1_variables = c(
    "Age, years" = "age",
    "Gender" = "sex",
    "Race / ethnicity" = "race_ethnicity",
    "Total cholesterol, mg/dl" = "chol_total_mgdl",
    "HDL-cholesterol, mg/dl" = "chol_hdl_mgdl",
    "Systolic blood pressure, mm Hg" = "bp_sys_mmhg",
    "Diastolic blood pressure, mm Hg" = "bp_dia_mmhg",
    "Antihypertensive medication use" = "meds_bp",
    "Diabetes" = "diabetes",
    "Chronic kidney disease" = "ckd",
    "Aged 65+ years" = "age_gt65",
    "Current smoker" = "smk_current",
    "Prevalent CVD" = "ever_had_ascvd"
  ),

  # Tables ----

  tbl1_overall = tabulate_characteristics(design_overall,
                                          tbl1_variables,
                                          decimals),

  tbl1_s1hyp = tabulate_characteristics(design_s1hyp,
                                        tbl1_variables,
                                        decimals),

  tbl_bpdist = tabulate_bpdist(design_overall, decimals),

  tbl_risk_overall = tabulate_risk_summary(design_overall, decimals),

  # Figures ----

  color_key = c("Overall" = "#5DA5DAFF",
                "Diabetes" = "#FAA43AFF",
                "Chronic kidney disease" = "#60BD68FF",
                "Aged 65+ years" = "#F15854FF",
                "Any listed condition" = "#B276B2FF",
                "No diabetes or chronic kidney disease" = "#8D4B08FF"),

  qts = seq(0.001, 0.999, by = 0.0001),
  fig_hist_ovrl = visualize_risk_hist(design_overall, qts),
  fig_hist_stg1 = visualize_risk_hist(design_s1hyp, qts),

  age_breaks = seq(40, 79, by = .01),

  risk_models = estimate_risk(design_overall, age_breaks),

  fig_risk_ovrl_bnry = visualize_risk_bnry(risk_models,
                                           bp_level = 'ovrl',
                                           color_key = color_key),

  fig_risk_stg1_bnry = visualize_risk_bnry(risk_models,
                                           bp_level = 'stg1',
                                           color_key = color_key),

  # Report ----

  compile_report(
    exams,
    current_analysis,
    design_overall,
    tbl1_overall,
    tbl1_s1hyp,
    tbl_bpdist,
    tbl_risk_overall,
    current_analysis$tbl,
    fig_hist_ovrl$ovrl,
    fig_hist_stg1$stg1,
    fig_risk_ovrl_bnry,
    fig_risk_stg1_bnry
  )

)