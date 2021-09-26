#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param design_overall
#' @return
#' @author bcjaeger
#' @export
tabulate_bpmeds <- function(design) {

  fix_names <- function(.x, first_name = 'meds_n_bp_classes'){
    if(ncol(.x) == 3){
      set_names(.x, c(first_name, 'level', 'n'))
    } else {
      set_names(.x, c(first_name, 'n'))
    }
  }

  formulae <- list(
    overall = ~ meds_n_bp_classes,
    diabetes = ~ meds_n_bp_classes + diabetes,
    ckd = ~ meds_n_bp_classes + ckd,
    # diabetes_and_ckd = ~ bp_cat + diabetes_and_ckd,
    age_group = ~ meds_n_bp_classes + age_gt65,
    any = ~ meds_n_bp_classes + any_ckd_diab_age65
  )

  # class_counts: determine the proportion of survey participants
  # taking 0, 1, 2, ... 6 classes of medication overall and among
  # study subgroups.

  .design <- subset(design, meds_bp == 'yes' & meds_n_bp_classes > 0)

  class_counts <- formulae %>%
    map(svytable, design = .design) %>%
    map(as_tibble) %>%
    map_dfr(fix_names, .id = 'variable') %>%
    filter(level == 'yes' | is.na(level)) %>%
    select(-level) %>%
    group_by(variable) %>%
    mutate(tbl_val = table_glue("{100 * n / sum(n)}%")) %>%
    select(variable, meds_n_bp_classes, tbl_val)

  # mm_smry: Median/mode summary of the number of medications taken

  rspec_2dig <- round_spec() %>%
    round_using_decimal(digits = 2)

  mm_design <- subset(design, meds_bp == 'yes'& meds_n_bp_classes > 0)

  mm_designs <- list(
    overall = mm_design,
    diabetes = subset(mm_design, diabetes == 'yes'),
    ckd = subset(mm_design, ckd == 'yes'),
    age_group = subset(mm_design, age_gt65 == 'yes'),
    any = subset(mm_design, any_ckd_diab_age65 == 'yes')
  )

  mean_smry <-
    map(mm_designs,
        .f = ~svymean(~meds_n_bp_classes,
                      design = .x,
                      na.rm = TRUE)
    ) %>%
    map_dfr(as_tibble, .id = 'variable') %>%
    rename(se = meds_n_bp_classes) %>%
    mutate(tbl_val = table_glue("{mean} ({se})", rspec = rspec_2dig),
           meds_n_bp_classes = 'Mean (standard error)') %>%
    select(variable, meds_n_bp_classes, tbl_val)

  median_smry <-
    map(mm_designs,
        .f = ~svyquantile(~meds_n_bp_classes,
                          design = .x,
                          quantiles = c(0.25, 0.50, 0.75),
                          na.rm = TRUE)
    ) %>%
    map_dfr(as_tibble, .id = 'variable') %>%
    rename(lwr = `0.25`,
           est = `0.5`,
           upr = `0.75`) %>%
    mutate(across(.cols = c(lwr, est, upr),
                  .fns = as.integer),
           tbl_val = table_glue("{est} ({lwr}, {upr})"),
           meds_n_bp_classes = 'Median (25th, 75th percentile)') %>%
    select(variable, meds_n_bp_classes, tbl_val)

  tbl_data <- bind_rows(
    counts = class_counts,
    smry = bind_rows(mean_smry, median_smry),
    .id = 'tabsec'
  )

  tbl_inline <- tbl_data %>%
    mutate(
      meds_n_bp_classes = recode(
        meds_n_bp_classes,
        "0" = "n_0",
        "1" = "n_1",
        "2" = "n_2",
        "3" = "n_3",
        "4" = "n_4",
        "5" = "n_5",
        "6" = "n_6",
        "Median (25th, 75th percentile)" = 'median_iqr',
        "Mean (standard error)" = 'mean_se'
      )
    ) %>%
    as_inline(tbl_variables = c('variable', 'meds_n_bp_classes'),
              tbl_value = 'tbl_val')

  list(table = tbl_data, inline = tbl_inline)

}
