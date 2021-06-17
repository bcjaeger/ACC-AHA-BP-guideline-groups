#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param exams
#' @return
#' @author bcjaeger
#' @export
clean_rx_statin <- function(exams) {

  if(any(exams %in% c('1999', '2001', '2003')))
    stop(
      'cannot get statin usage for exams prior to 2005',
      call. = FALSE
    )

  statins <- c("ATORVASTATIN",
               "ATORVAST",
               "FLUVASTATIN",
               "LOVASTATIN",
               "PITAVASTATIN",
               "PRAVASTATIN",
               "ROSUVASTATIN",
               "SIMVASTATIN")

  data_all <- map_dfr(
    .x = make_exam_files(exams, data_label = 'RXQ_RX'),
    .f = read_xpt,
    .id = 'exam'
  )

  data_all %>%
    select(
      exam,
      seqn = SEQN,
      rx_used_30days = RXDUSE,
      rx_name_generic = RXDDRUG,
      rx_code_generic = RXDDRGID,
      rx_seen_interview = RXQSEEN,
      rx_days_taken = RXDDAYS,
      rx_number_taken = RXDCOUNT
    ) %>%
    group_by(seqn) %>%
    summarize(
      exam = exam[1],
      rx_number_taken = rx_number_taken[1],
      statin = as.numeric(any(rx_name_generic %in% statins))
    )

}
