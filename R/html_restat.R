# Parse Restud HTML article

example = function() {
  library(rvest)
  project_dir = "~/repbox/projects_reg/testart"
  html.file = list.files(file.path(project_dir, "art","html"),glob2rx("restat_*.html"), full.names = TRUE)[1]
  journ = str.left.of(basename(html.file),"_")
  html = rvest::read_html(html.file)

  res = art_html_to_parts(project_dir, html = html, journ=journ)
  text_df = res$text_df
  tab_df = res$tab_df

  rstudioapi::filesPaneNavigate(project_dir)
}

# Similar but not identical to restud_parse
# Paragraphs and footnotes have different classes
restat_parse_html = function(project_dir, html = rvest::read_html(html.file), html.file=NULL, journ = "restud") {
  restore.point("parse_restat_html")
  library(rvest)
  library(xml2)
  body = html %>% rvest::html_node(".article-body")

  nodes = body %>% rvest::html_elements(".abstract-title, .section-title, .fig, .table-wrap, .table-full, .chapter-para, div.article-section-wrapper p, sup .xref-fn")

  df = tibble(tag = xml_name(nodes), xpath=xml_path(nodes), classes = html_attr(nodes,"class"),  text = html_text2(nodes))

  df = df %>%
    mutate(is_figure = tag == "div" & startsWith(classes,"fig")) %>%
    mutate(is_table = tag == "div" & startsWith(classes,"table")) %>%
    mutate(
      type = case_when(
        startsWith(classes, "abstract") ~ "abs",
        tag == "p" ~ "p",
        tag == "h2" ~ "sec1",
        tag == "h3" ~ "sec2",
        tag == "h4" ~ "sec3",
        is_figure ~ "fig",
        is_table ~ "tab",
        tag == "a" ~ "note",
        TRUE ~ "unknown"
      )
    )

  # Map footnotes
  note_rows = which(df$type=="note")
  note_ids = id=html_attr(nodes[note_rows],"reveal-id")

  fn_nodes = html %>% html_elements(".fn")
  fn_ids = html_attr(fn_nodes, "content-id")
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
  i = 1
  tab_df = bind_rows(lapply(seq_along(inds), function(i) {
    # same table structure as restud
    restud_parse_html_table(nodes[[inds[i]]],i)
  }))

  list(text_df=df, tab_df = tab_df)

}
