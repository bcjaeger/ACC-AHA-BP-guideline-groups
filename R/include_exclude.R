##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data
include_exclude <- function(data) {

  e1 <- data
  n1 <- nrow(e1)
  # in exams

  e2 <- e1 %>% filter(wts_mec_2yr > 0)
  n2 <- nrow(e2)
  # valid weights

  e3 <- e2 %>% filter(age_40to79 == "yes")
  n3 <- nrow(e3)
  # 40 - 79 years old


  e4 <- e3 %>% filter(n_msr_sbp >= 3 & n_msr_dbp >= 3)
  n4 <- nrow(e4)
  # 3+ BP measurements

  data_excluded <- e4 %>%
    drop_na(
      sex,
      age,
      race_ethnicity,
      chol_total_mgdl,
      chol_hdl_mgdl,
      bp_sys_mmhg,
      meds_bp,
      diabetes,
      ckd,
      smk_current
    )

  n5 <- nrow(data_excluded)
  # complete data for pooled cohort risk

  tbl_excluded <- tibble(
    label = c(
      'Participated in 2013-2014, 2015-2016, or 2017-2018 exam',
      'Attended interview and exam',
      'Aged 40-79 years',
      '3 or more systolic and diastolic blood pressure measurements',
      'Complete data for variables used in the pooled cohort risk equations'
    ),
    sample_size = c(n1, n2, n3, n4, n5),
    n_removed = c(0, -diff(sample_size))
  )

  list(tbl = tbl_excluded, data = data_excluded)

}
