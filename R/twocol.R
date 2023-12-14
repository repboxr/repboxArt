# TO DO: NEEDS IMPLEMENTATION

art_has_two_col = function(project.dir, journ) {
  if (journ != "aer_pandp") return(FALSE)
  return(TRUE)
}

art_repair_two_col = function(line_df, journ) {
  artid = basename(project.dir)
  if (journ != "aer_pandp") {
    stop("Two col repair currently only implemented for aer_pandp")
  }
  art_repair_two_col_aer_pandp(line_df)
}


art_repair_two_col_aer_pandp = function(line_df) {
  restore.point("art_repair_tow_col_aer_pandp")

  junk_start = which(endsWith(line_df$trim_txt,"This article has been cited by:"))
  if (length(junk_start)>0) {
    line_df = line_df[1:(junk_start-1),]
  }

  line_df$two_col = !line_df$type %in% c("header", "footer")

  left_str = stri_sub(line_df$trim_txt,1,51) %>% trimws()
  right_str = stri_sub(line_df$trim_txt,54) %>% trimws()

  right_str[126]


  left_gaps_count = stri_count_regex(left_str, "(  )+")
  left_char_count = stri_count_regex(left_str, "[A-Za-z]")
  left_sym_count = stri_count_regex(left_str, "[^ ]")

  right_gaps_count = stri_count_regex(right_str, "(  )+")
  right_char_count = stri_count_regex(right_str, "[A-Za-z]")
  right_sym_count = stri_count_regex(right_str, "[^ ]")

  left_nice = (left_gaps_count == 0) + (left_char_count > 25)

  cbind(left_char_count, left_sym_count)



  return(line_df)

}
