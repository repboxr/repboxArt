#' Specify options for parsing information from article
#'
#'@param single_line_regs Consider an article table in which regressions seem to have multiple rows. Normally we would interpret this as a single regression for which the variable of interest and multiple control variables are shown. However, sometimes each row corresponds to a separate regression (usually varying the dependent variable). Examples are Tables 2 and 3 in aejapp_3_2_2. Table 1 in aejapp_3_2_2 shows multi-line regressions instead.
#'
#' If single_line_regs=FALSE we always assume that multiple rows correspond to single regression. If single_line_regs=TRUE we assume all tables show single line regression. If single line is a data frame it can have columns tabid and panel_num to specify which tables and possible panels are assumed to show single line regressions.
repbox_art_opts = function(overwrite=FALSE,single_line_regs = FALSE, few_integer_as_text_limit = 5) {
  list(
    overwrite = overwrite,
    single_line_regs = single_line_regs,
    few_integer_as_text_limit = 5
  )
}
