##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data
derive_others <- function(data, exams,
                          acr_cutpoint=30) {

  data %>%
    mutate(
      # pooling weights for pooled analysis
      wts_mec_2yr = wts_mec_2yr / length(exams),
      # chronic kidney disease variables
      albuminuria = if_else(acr_mgg >= acr_cutpoint, "yes", "no"),
      ckd = if_else(albuminuria == "yes" | egfr_low == "yes", "yes", "no"),
      # update categories to align with acc/aha guidelines
      age_group = case_when(
        age < 40  ~ "20 to 39", # age < 20 will be excluded.
        age < 65  ~ "40 to 64",
        age < 75  ~ "65 to 74",
        age >= 75 ~ "75 and above"
      ),
      age_gt65 = if_else(age >= 65, "yes", "no"),
      age_40to79 = if_else(age >= 40 & age < 80, "yes", "no"),
      diabetes_and_ckd = if_else(ckd == "yes" & diabetes == "yes", "yes", "no"),
      no_diabetes_or_ckd = if_else(ckd == 'no' & diabetes == 'no', 'yes', 'no'),
      bp_cat = case_when(
        meds_bp == "yes" ~ 5,
        bp_sys_mmhg < 120 & bp_dia_mmhg < 80 ~ 1,
        bp_sys_mmhg < 130 & bp_dia_mmhg < 80 ~ 2,
        bp_sys_mmhg < 140 & bp_dia_mmhg < 90 ~ 3,
        bp_sys_mmhg >= 140 | bp_dia_mmhg >= 90 ~ 4,
        TRUE ~ NA_real_
      ),
      bp_cat = factor(
        bp_cat,
        levels = 1:5,
        labels = c(
          "Normal blood pressure",
          "Elevated blood pressure",
          "Stage 1 hypertension",
          "Stage 2 hypertension",
          "Taking antihypertensive medication")
      ),
      any_ckd_diab_age65 = if_else(
        ckd == "yes" | diabetes == "yes" | age >= 65,
        true = "yes", false = "no"
      ),
      # categorize pooled cohort risk
      pcr_gteq_10 = if_else(ascvd_risk_pcr >= 0.10, "yes", "no"),
      # set missing values to "no" for history of ASCVD
      ever_had_ascvd = replace(ever_had_ascvd, is.na(ever_had_ascvd), 'no'),
      # set high risk for participants with history of ASCVD
      pcr_highrisk = if_else(
        condition = pcr_gteq_10 == "yes" | ever_had_ascvd == "yes",
        true = "yes", false = "no"
      ),
      race_ethnicity = fct_collapse(
        race_ethnicity,
        Hispanic = c("Mexican American", "Other Hispanic")
      ),
      race = fct_collapse(
        race_ethnicity,
        Black = "Non-Hispanic Black",
        Not_Black = c("Hispanic",
                      "Non-Hispanic Asian",
                      "Non-Hispanic White",
                      "Other Race - Including Multi-Racial")
      ),
      race_ethnicity = factor(
        race_ethnicity,
        levels = c(
          "Non-Hispanic White",
          "Non-Hispanic Black",
          "Hispanic",
          "Non-Hispanic Asian",
          "Other Race - Including Multi-Racial"
        )
      ),
      race_ethnicity = fct_recode(
        race_ethnicity,
        "Other Race/ethnicity - Including Multi-Racial" = "Other Race - Including Multi-Racial"
      )
    ) %>%
    select(
      exam,
      seqn,
      psu,
      strata,
      wts_mec_2yr,
      sex,
      age,
      race_ethnicity,
      race,
      chol_total_mgdl,
      chol_hdl_mgdl,
      n_msr_sbp,
      n_msr_dbp,
      bp_sys_mmhg,
      bp_dia_mmhg,
      meds_bp,
      smk_current,
      age_group,
      age_40to79,
      age_gt65,
      diabetes,
      ckd,
      diabetes_and_ckd,
      no_diabetes_or_ckd,
      any_ckd_diab_age65,
      bp_cat,
      ever_had_ascvd,
      ascvd_risk_pcr,
      pcr_gteq_10,
      pcr_highrisk,
    ) %>%
    mutate_if(is.character, as.factor)

}
