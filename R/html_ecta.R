# Parse ecta HTML article

example = function() {
  library(rvest)
  project_dir = "~/repbox/projects_reg/testart"
  html.file = list.files(file.path(project_dir, "art","html"),glob2rx("ecta_*.html"), full.names = TRUE)[1]
  journ = str.left.of(basename(html.file),"_")
  html = rvest::read_html(html.file)

  res = art_html_to_parts(project_dir, html = html, journ=journ)
  text_df = res$text_df
  tab_df = res$tab_df

  rstudioapi::filesPaneNavigate(project_dir)
}

ecta_parse_html = function(project_dir, html = rvest::read_html(html.file), html.file=NULL, journ = "ecta") {
  restore.point("parse_ecta_html")
  library(rvest)
  library(xml2)
  body = html %>% rvest::html_node(".article__body")
  if (journ=="ecta") {
    nodes = body %>% rvest::html_elements(".article-section__abstract, .section1, .section2, .section3, .figure, .article-table-content, div.article__content p, .noteLink")
  }

  df = tibble(tag = xml_name(nodes), xpath=xml_path(nodes), classes = html_attr(nodes,"class"),  text = html_text2(nodes), id=html_attr(nodes,"id"))

  df = df %>%
    mutate(is_figure = tag == "figure") %>%
    mutate(is_table = tag == "div" & startsWith(classes,"article-table")) %>%
    mutate(
      type = case_when(
        has.substr(classes, "abstract") ~ "abs",
        tag == "p" ~ "p",
        tag == "h2" ~ "sec1",
        tag == "h3" ~ "sec2",
        tag == "h4" ~ "sec3",
        is_figure ~ "fig",
        is_table ~ "tab",
        startsWith(classes, "noteLink") ~ "note",
        TRUE ~ "unknown"
      )
    )

  # Map footnotes


  # Reference in text
  note_rows = which(df$type=="note")
  note_ids = str.left.of(df$id[note_rows],"_")

  # Container
  fn_nodes = html %>% html_elements(".footNotePopup__item")
  # ecta200378-note-0010

  fn_ids = html_attr(fn_nodes, "id")
  fn_txt = html_text2(fn_nodes)
  ma = match(note_ids, fn_ids)
  note_txt = paste0("\n[Footnote ", fn_txt[ma],"]\n")
  df$text[note_rows] = note_txt


  # Removes duplicated table entries
  df$.ROW = 1:NROW(df)
  dupl = duplicated(select(df, tag, type, text))
  df = df[!dupl | df$type=="note",]

  df = remove_nested_html_elements(df, which(df$tag == "div"))
  df = text_df_standardize(df)
  nodes = nodes[df$.ROW]


  inds = which(df$type == "tab")
  tab_df = bind_rows(lapply(seq_along(inds), function(i) {
    ecta_parse_html_table(nodes[[inds[i]]],i)
  }))

  list(text_df=df, tab_df = tab_df)

}


ecta_parse_html_table = function(node, tab_counter) {
  restore.point("ecta_parse_html_table")

  tabtitle = html_node(node, ".article-table-caption") %>% html_text2()
  tabname = html_node(node, ".table-caption__label") %>% html_text2()
  tabid = tabname_to_tabid(tabname)

  romans = c("I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII","XIII","XIV","XV","XVI","XVII","XVIII","XIX","XX","XXI","XXII","XXIII","XXIV","XXV","XXVI","XXVII")

  ma = match(tabid, romans)
  if (!is.na(ma)) {
    tabid = as.character(ma)
    tabname = stri_replace_first_fixed(tabname, romans[ma], tabid)
    tabtitle = stri_replace_first_fixed(tabtitle, romans[ma], tabid)
  }
  tabname = stri_replace_first_fixed(tabname, "TABLE","Table")
  tabtitle = stri_replace_first_fixed(tabtitle, "TABLE","Table")


  tabnotes = html_node(node, ".article-section__table-footnotes") %>% html_text2()

  tab_node = html_node(node, "table")

  #tab_rows = html_nodes(node, "table > tr, table > thead > tr, table > tbody > tr")
  tab_rows = html_nodes(tab_node, "tr, thead > tr, tbody > tr")

  cell_df = html_table_cells_from_all_tr(tab_rows)

  tabsource = as.character(tab_node) %>% merge.lines()

  tibble(tabid=tabid, tab_counter=tab_counter, tabtitle = tabtitle, tabnotes = tabnotes, cell_df = list(cell_df), tabsource = tabsource) %>%
    art_html_tab_standardize()
}

