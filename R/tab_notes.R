art_extract_paren_type_from_tab_notes = function(project_dir, tab_df=art_load_tab_df(project_dir), route=get_art_route()) {
  restore.point("art_extract_paren_type_from_tab_notes")

  if (is.null(tab_df)) return(tab_df)

  notes = tolower(tab_df$tabnotes)
  is_se = stringi::stri_detect_regex(notes, "standard error[ a-z]*parenthes")
  is_t = stringi::stri_detect_regex(notes, "[tz][- ](value|statistic)[ a-z]*parenthes")
  is_p = stringi::stri_detect_regex(notes, "[p][- ](value|statistic)[ a-z]*parenthes")

  tab_df$paren_type = case_when(
    is_se ~ "se",
    is_t ~ "t",
    is_p ~ "p",
    TRUE ~ NA_character_
  )
  parcels = list(art_tab_note_paren_type = list(art_tab_note_paren_type=tab_df))
  repdb_save_parcels(parcels, file.path(project_dir,"art","routes",route, "repdb"))
  tab_df

}
