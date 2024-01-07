#' Specify options for parsing information from article
#'
#'@param journ_format Some preparation steps, like section detection depend on the journal. Currently only very few journals have heuristics implemented, call repbox_journ_list() for a list.
#'@param single_line_regs Consider an article table in which regressions seem to have multiple rows. Normally we would interpret this as a single regression for which the variable of interest and multiple control variables are shown. However, sometimes each row corresponds to a separate regression (usually varying the dependent variable). Examples are Tables 2 and 3 in aejapp_3_2_2. Table 1 in aejapp_3_2_2 shows multi-line regressions instead.
#'
#' If single_line_regs=FALSE we always assume that multiple rows correspond to single regression. If single_line_regs=TRUE we assume all tables show single line regression. If single line is a data frame it can have columns tabid and panel_num to specify which tables and possible panels are assumed to show single line regressions.
#' @param not_implemented_action "error", "warn", "msg", or "nothing". The text analysis currently uses crude heuristics to detect elements, like sections, page headers and footers, and footnotes for PDF. Currently the heuristics are implemented just for a few journals (AEA journals). If a feature is not implemented for the article's journal shall an error be thrown, a warining or nothing be done.
#' @param detect_problem "error", "warn", "msg", or "nothing"
repbox_art_opts = function(journ_format = NULL, overwrite=FALSE,single_line_regs = FALSE, few_integer_as_text_limit = 5, not_implemented_action=c("error","warn","msg","nothing")[3], detection_problem=c("error","warn","msg","nothing")[3]) {
  list(
    overwrite = overwrite,
    single_line_regs = single_line_regs,
    few_integer_as_text_limit = 5,
    not_implemented_action = not_implemented_action,
    detection_problem = detection_problem,
    journ_format=journ_format
  )
}
