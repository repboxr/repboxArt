# Parse ms HTML article

ms_parse_html = function(project_dir, html = rvest::read_html(html.file), html.file=NULL, journ = "ms") {
  restore.point("parse_ms_html")
  library(rvest)
  library(xml2)
  body = html %>% rvest::html_node(".article__body")



  if (journ=="ms") {
    nodes = body %>% rvest::html_elements(".hlFld-Abstract, .article-section__title, figure, .tableWrapper, div.hlFld-Fulltext p, .fn")
  }

  df = tibble(tag = xml_name(nodes), xpath=xml_path(nodes), classes = html_attr(nodes,"class"),  text = html_text2(nodes), id=html_attr(nodes,"id"), href=html_attr(nodes, "href"))

  df = df %>%
    mutate(is_figure = tag == "figure") %>%
    mutate(is_table = startsWith(classes,"tableWrapper")) %>%
    mutate(
      type = case_when(
        has.substr(classes, "Abstract") ~ "abs",
        tag == "p" ~ "p",
        tag == "h2" ~ "sec1",
        tag == "h3" ~ "sec2",
        tag == "h4" ~ "sec3",
        is_figure ~ "fig",
        is_table ~ "tab",
        classes == "ref fn" ~ "note",
        TRUE ~ "unknown"
      )
    )

  # Check wrong / empty HTML
  if (NROW(df)<=4) return(NULL)
  if (!any(df$type=="p")) return(NULL)

  # Map footnotes


  # Reference in text
  note_rows = which(df$type=="note")
  note_ids = substring(df$href[note_rows],2)

  # Container
  fn_nodes = html %>% html_elements(".NLM_fn")
  # ms200378-note-0010

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
    ms_parse_html_table(nodes[[inds[i]]],i)
  }))

  list(text_df=df, tab_df = tab_df)

}


ms_parse_html_table = function(node, tab_counter) {
  restore.point("ms_parse_html_table")

  tabtitle = html_node(node, "strong") %>% html_text2()
  tabname = html_node(node, ".captionLabel") %>% html_text2()
  tabid = tabname_to_tabid(tabname)

  tabnotes = html_node(node, ".footnote") %>% html_text2()

  tab_node = html_node(node, "table")

  #tab_rows = html_nodes(node, "table > tr, table > thead > tr, table > tbody > tr")
  tab_rows = html_nodes(tab_node, "tr, thead > tr, tbody > tr")


  cell_df = html_table_cells_from_all_tr(tab_rows)

  tabsource = as.character(tab_node) %>% merge.lines()

  tibble(tabid=tabid, tab_counter=tab_counter, tabtitle = tabtitle, tabnotes = tabnotes, cell_df = list(cell_df), tabsource = tabsource) %>%
    art_html_tab_standardize()
}

