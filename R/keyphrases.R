example = function() {
  make_phrases_def()
  p_def = get_phrases_def()
  project_dir = "~/repbox/projects_reg/testart"
  project_dir = "~/repbox/projects_reg/aejapp_3_2_2"
  project_dir = "~/repbox/projects_reg/aejapp_3_1_3"

  art_pdf_pages_to_parts(project_dir)
  art_phrase_analysis(project_dir)

  rstudioapi::filesPaneNavigate(project_dir)

  ind_tab = readRDS(file.path(project_dir,"art","regdb","ind_tab_type.Rds"))[[1]]
  View (ind_tab)

}

art_phrase_analysis = function(project_dir) {
  restore.point("art_phrase_analysis")
  res_text = art_text_parts_phrase_analysis(project_dir)
  res_tab = art_tab_phrase_analysis(project_dir)

  refs = res_text[c("tab_ref","fig_ref","col_ref")]
  saveRDS(refs, file.path(project_dir, "art","refs_tab_fig_col.Rds") )

  res = list(text = res_text$phrases_loc, tab=res_tab$phrases_loc)
  saveRDS(res, file.path(project_dir, "art","phrases.Rds"))


  # Save table type indicators in regdb
  parcels = list()
  tt_df = bind_rows(res_tab$tt_df, res_text$tt_df)
  coty_df = res_text$coty_df
  if (NROW(tt_df)==0 & NROW(coty_df)==0) return(parcels)

  library(repboxDB)
  specs = regdb_load_specs(libs="repboxArt")
  artid = basename(project_dir)

  tt_df$artid = artid
  coty_df$artid = artid

  # Count duplicates
  tt_df = regdb_select_fields(tt_df,"ind_tab_type", ignore="ind_counts")
  tt_df = tt_df %>%
    group_by(across()) %>%
    summarize(
      ind_counts = n()
    )

  coty_df = regdb_select_fields(coty_df,"ind_col_type", ignore="ind_counts")
  coty_df = coty_df %>%
    group_by(across()) %>%
    summarize(
      ind_counts = n()
    )


  regdb_check_data(tt_df,"ind_tab_type")
  parcels$ind_tab_type = list(ind_tab_type=tt_df)

  regdb_check_data(coty_df,"ind_col_type")
  parcels$ind_col_type = list(ind_col_type=coty_df)

  dir = file.path(project_dir, "art","regdb")
  regdb_save_parcels(parcels, dir)

}

art_tab_phrase_analysis = function(project_dir, tab_df=art_load_tab_df(project_dir), p_def = get_phrases_def()) {
  restore.point("art_tab_phrase_analysis")

  n = NROW(tab_df)
  if (n==0) return(NULL)
  txt = merge.lines(c(tab_df$tabtitle, tab_df$tabnotes, tab_df$tabsource))
  parts_loc = text_parts_to_loc(nchars=c(tab_df$nchar_title,tab_df$nchar_notes, tab_df$nchar_tabsource))
  parts_loc$tabrow = c(1:n,1:n,1:n)
  parts_loc$tabid = tab_df$tabid[parts_loc$tabrow]
  parts_loc$part = c(rep("title",n),rep("notes",n),rep("table",n))
  parts_loc$partind = 1:NROW(parts_loc)

  loc = txt_phrase_analysis(txt, add_sent_str = FALSE)
  if (NROW(loc)==0) {
    return(list(phrases_loc=NULL, tt_df=NULL))
  }

  loc$partind = map_loc_to_parent_loc(loc, parts_loc)
  loc = left_join(loc, rename(parts_loc, part_start=start, part_end=end), by="partind") %>% select(-partind)

  # Now match the keywords to indicators

  # Table type keyword definitions
  tt_def = p_def$tt_def

  tt_df = inner_join(loc, tt_def, by="phrase") %>%
    filter(search_part == "all" |
      search_part == part |
      (not_search_part != "" & not_search_part != part)
    ) %>%
    mutate(ind_type = paste0("keyword_", part), ind_val=1, artid=basename(project_dir)) %>%
    rename(tab_type = ind, ind_keyword=key )

  cols = c("tabid","tab_type", "ind_keyword","phrase", "part","sentence","ind_type", "ind_val")
  tt_df = tt_df[,cols]


  # To do: Add column references based on table notes...


  list(phrases_loc=loc, tt_df=tt_df)

}

art_text_parts_phrase_analysis = function(project_dir, text_df=art_load_text_parts(project_dir), p_def=get_phrases_def()) {
  restore.point("art_text_parts_phrase_analysis")
  txt = merge.lines(text_df$text)


  sent_loc = art_locate_sentences(txt)

  # Keyword phrases
  loc = txt_phrase_analysis(txt, sent_loc=sent_loc)
  loc
  if (is.null(loc)) return(NULL)

  parts_loc = text_parts_to_loc(text_df$nchar)
  loc$partind = map_loc_to_parent_loc(loc, parts_loc)
  loc$parttype = text_df$type[loc$partind]
  loc

  # References to table, figure or column
  ref_li = art_refs_analysis(txt, sent_loc=sent_loc,parts_loc = parts_loc)
  tab_ref = ref_li$tab_ref; col_ref = ref_li$col_ref; fig_ref = ref_li$fig_ref


  # Find closest table reference
  ref_df = bind_rows(
    tab_ref %>% mutate(id=tabid),
    fig_ref %>% mutate(id=figid, tabid=NA_character_),
    col_ref %>% mutate(id = as.character(col))
  ) %>% arrange(
    start
  )
  ref_sent = unique(ref_df$sentence)

  ref_ind = findInterval(loc$sentence, ref_sent)
  ref_ind[ref_ind == 0] = NA_integer_

  loc$ref_sentence = ref_sent[ref_ind]


  # Match table type keyword definitions
  tt_def = p_def$tt_def
  tt_df_no_ref = inner_join(loc, tt_def, by="phrase") %>%
    rename(tab_type = ind, ind_keyword=key )

  # Match ref_df
  tt_df = inner_join(tt_df_no_ref, ref_df %>% rename(ref_partind=partind, ref_sentence = sentence, ref_start=start, ref_end = end), by="ref_sentence") %>%
    mutate(
      ref_sent_dist = abs(sentence - ref_sentence),
      ref_char_dist = abs(start - ref_start),
      ind_val = 1 / (1+ref_sent_dist),
      ind_type = "keyword_ref"
    ) %>%
    filter(ref_sent_dist <= ref_max_dist)

  # Keep references from footnotes only if keyword and reference are in
  # the same footnote
  tt_df = tt_df %>%
    filter(parttype != "note" | partind == ref_partind)

  coty_df = filter(tt_df, reftype %in% c("col","cols")) %>%
    filter(ref_sent_dist <= 1)

  tt_df = filter(tt_df, reftype %in% c("tab"), ref_sent_dist <= 10)
  cols = c("tabid","tab_type", "ind_keyword","phrase", "sentence","ind_type", "ind_val", "ref_sentence","ref_sent_dist","ref_char_dist")
  tt_df = tt_df[,cols]

  cols = c("tabid","col", "tab_type", "ind_keyword","phrase", "sentence","ind_type", "ind_val", "ref_sentence","ref_sent_dist","ref_char_dist")
  coty_df = coty_df[,cols]


  # Map references to keyword phrases
  c(list(phrases_loc = loc, tt_df=tt_df, coty_df=coty_df), ref_li)
}

art_refs_analysis = function(txt,refs_types = c("tab","fig","col"), sent_loc=art_locate_sentences(txt), parts_loc=NULL) {
  restore.point("art_refs_analysis")

  tab_fig_loc = art_locate_tab_fig_refs(txt)
  col_loc = art_locate_col_refs(txt)
  tab_fig_loc$sentence = map_loc_to_parent_loc(tab_fig_loc, sent_loc)
  col_loc$sentence = map_loc_to_parent_loc(col_loc, sent_loc)
  tab_fig_loc$col = NA_integer_





  loc = bind_rows(tab_fig_loc, col_loc) %>%
    select(start, end,reftype = type, id = typeid, col=col, sentence, str=str) %>%
    arrange(sentence, reftype!="tab", reftype != "figure", start)

  if (!is.null(parts_loc)) {
    loc$partind = map_loc_to_parent_loc(loc, parts_loc)
  } else {
    loc$partind = NA_integer_
  }

  # Let us find the closest preceding table reference
  loc$prev_tab_row = cumsum(loc$reftype=="tab")
  loc$prev_fig_row = cumsum(loc$reftype=="fig")
  loc = loc %>%
    group_by(prev_tab_row) %>%
    mutate(
      prev_tabid = ifelse(prev_tab_row == 0, NA_character_, first(id)),
      prev_tab_sentence = ifelse(prev_tab_row == 0, NA_integer_, first(sentence)),
      prev_tab_partind = ifelse(prev_tab_row == 0, NA_integer_, first(partind)),
      prev_tab_start = ifelse(prev_tab_row == 0, NA_integer_, first(start))
    ) %>%
    ungroup()

  tab_ref = filter(loc, reftype=="tab") %>%
    select(start, end,reftype, tabid = id, sentence, partind, str)

  fig_ref = filter(loc, reftype=="fig") %>%
    select(start, end,reftype, figid = id, sentence, partind, str)

  col_ref = filter(loc, reftype %in% c("col","cols")) %>%
    mutate(tab_sent_dist = abs(prev_tab_sentence - sentence), tab_char_dist = abs(start-prev_tab_start)) %>%
    select(start, end,reftype, col, tabid = prev_tabid,  sentence, partind,  tab_char_dist, tab_sent_dist, tab_sentence = prev_tab_sentence, tab_partind=prev_tab_partind, str)

  if (NROW(col_ref)==0) {
    col_ref$tabid = character(0)
  }

  list(tab_ref=tab_ref, fig_ref=fig_ref, col_ref=col_ref)
}

txt_phrase_analysis = function(txt, p_def = get_phrases_def(), sent_loc = art_locate_sentences(txt),  add_sent_str = TRUE) {
  restore.point("txt_phrase_analysis")
  ltxt = tolower(txt)

  key_loc = bind_rows(
    txt_locate_keywords(txt,p_def$keywords_lu),
    txt_locate_keywords(ltxt,p_def$keywords_l),
    txt_locate_rx_keywords(txt, p_def$key_rx_lu),
    txt_locate_rx_keywords(ltxt, p_def$key_rx_l)
  )
  if (NROW(key_loc)==0) return(NULL)

  key_loc$sentence = map_loc_to_parent_loc(key_loc, sent_loc)

  loc = key_loc %>%
    select(start, end,sentence, phrase)
  if (add_sent_str)
    loc$sent_str = sent_loc$str[loc$sentence]
  return(loc)


}


txt_locate_keywords = function(txt, keywords) {
  restore.point("txt_locate_keywords")

  loc_li = stringi::stri_locate_all_fixed(txt, keywords,omit_no_match = TRUE)
  loc_df = do.call("rbind",loc_li) %>% as_tibble()
  if (NROW(loc_df)==0) return(NULL)

  names(loc_df) = c("start","end")
  loc_df$phrase = substring(txt, loc_df$start, loc_df$end)
  loc_df
}

# TO DO: Call this function
txt_locate_rx_keywords = function(txt, rx, left_space=FALSE, right_space=FALSE) {
  restore.point("txt_locate_rx_keywords")

  loc_li = stringi::stri_locate_all_regex(txt, rx,omit_no_match = TRUE)
  loc_df = do.call("rbind",loc_li) %>% as_tibble()
  if (NROW(loc_df)==0) return(NULL)
  names(loc_df) = c("start","end")

  phrase = substring(txt, loc_df$start, loc_df$end)

  # Replace other separators on left with [ ]
  phrase = gsub("^[\\n\\t]"," ", phrase)
  # Replace other separators on right with [ ]
  phrase = gsub("^[\\.\\:;,\\n]$"," ", phrase)
  loc_df$phrase = phrase

  loc_df
}


# Keywords with index
txt_locate_typed_keywords = function(txt, keywords) {
  if (missing(keywords)) {
    keywords = jsonlite::fromJSON(readLines(warn = FALSE,system.file("keywords/keywords.json", package="repboxArt")))

  }
  restore.point("txt_locate_keywords")

  keys = names(keywords)
  rx = lapply(keywords, function(words) {
    paste0("(",words,")", collapse="|")
  })

  res = stringi::stri_locate_all_regex(txt, rx,capture_groups = FALSE,omit_no_match = TRUE)

  res_df = lapply(seq_along(rx), function(i) {
    loc = res[[i]]
    if (NROW(loc)==0) return(NULL)
    tibble(start=loc[,1], end = loc[,2], key=keys[i])
  }) %>% bind_rows()

  res_df$str = substring(txt, res_df$start, res_df$end)
  res_df
}




text_parts_to_loc = function(nchars = nchar(part_txt),collapse_nchar=1) {
  cusu = cumsum(nchars+collapse_nchar)
  n = length(cusu)
  cusu[n] = cusu[n]-collapse_nchar
  parts_loc = tibble(start=c(1,cusu[-n]+1), end=cusu)
  parts_loc
}


make_phrases_def = function() {
  restore.point("art_make_phrases_def")
  dir = system.file("keywords", package="repboxArt")

  keywords = jsonlite::fromJSON(readLines(warn = FALSE,file.path(dir,"keywords.json")))

  temp_df = extract_all_to_index_df(keywords)
  keys = names(keywords)[temp_df[[1]]]
  kw_df = tibble(key=keys, phrase=temp_df$str)


  tt_li = yaml::yaml.load_file(file.path(dir,"tab_type_keywords.yml"))

  i = 1
  tt_def = lapply(seq_along(tt_li), function(i) {
    tt = tt_li[[i]]
    ref_max_dist = first.non.null(tt$ref_max_dist,5)
    tt = tt[setdiff(names(tt),c("ref_max_dist","label"))]

    df = lapply(seq_along(tt), function(j) {
      tt_j = tt[[j]]
      df = lapply(seq_along(tt_j), function(k) {
        tibble(key = names(tt_j)[k], phrase = tt_j[[k]])
      }) %>% bind_rows()
      df$search_part = names(tt)[j]
      df
    }) %>% bind_rows()
    df$ind = names(tt_li)[[i]]

    df$ref_max_dist = ref_max_dist
    df
  }) %>% bind_rows()

  rows = tt_def$key==".self"
  tt_def$key[rows] = tt_def$phrase[rows]

  tt_def$not_search_part = str.right.of(tt_def$search_part,"not_",not.found=rep("", NROW(tt_def)))


  keywords = unique(c(kw_df$phrase,kw_df$key, tt_def$phrase)) %>%
    setdiff("")

  # Later we may differentiate with keywords with spaces or not
  # space_keywords = keywords[keywords != trimws(keywords)]

  is_lowcase = keywords == tolower(keywords)
  left_space = startsWith(keywords," ")
  right_space = endsWith(keywords," ")
  has_space = left_space | right_space

  keywords_l = keywords[is_lowcase & !has_space]
  keywords_lu = keywords[!is_lowcase & !has_space]

  key_rx = trimws(keywords)
  key_rx[left_space] = paste0("[\\n\\t ]", key_rx[left_space])
  key_rx[right_space] = paste0(key_rx[right_space], "[\\.\\:,; \\n]")

  rows = is_lowcase & has_space
  key_rx_l = key_rx[rows]
  rows = !is_lowcase & has_space
  key_rx_lu = key_rx[rows]


  labels = sapply(tt_li, function(tt) tt$label)

  p_def = list(
    tt_labels = labels,
    text_phrases = kw_df,
    tt_def = tt_def,
    keywords_l = keywords_l,
    keywords_lu = keywords_lu,
    key_rx_l =key_rx_l,
    key_rx_lu = key_rx_lu
  )
  saveRDS(p_def, file.path(dir,"phrases_def.Rds"))
  options(repbox.art.text.phrases.def = p_def)

  invisible(p_def)
}

load_phrases_def = function() {
  dir = system.file("keywords", package="repboxArt")
  p_def = readRDS(file.path(dir,"phrases_def.Rds"))
  options(repbox.art.text.phrases.def = p_def)
  p_def
}

get_phrases_def = function() {
  p_def = getOption("repbox.art.text.phrases.def")
  if (is.null(p_def)) {
    p_def = load_phrases_def()
    options(repbox.art.text.phrases.def = p_def)
  }
  p_def
}
