# Parse jpe HTML article

example = function() {
  library(rvest)
  project.dir = "~/repbox/projects_reg/testart"
  html.file = list.files(file.path(project.dir, "art","html"),glob2rx("jpe_*.html"), full.names = TRUE)[1]
  html = rvest::read_html(html.file, encoding="UTF-8")

  res = art_html_to_parts(project.dir, html = html, journ="jpe")
  text_df = res$text_df
  tab_df = res$tab_df

  rstudioapi::filesPaneNavigate(project.dir)
}

jpe_parse_html = function(project.dir, html = rvest::read_html(html.file), html.file=NULL, from.restat = FALSE, journ="jpe") {
  restore.point("parse_jpe_html")
  library(rvest)
  library(xml2)
  body = html %>% rvest::html_node("article")

  nodes = body %>% rvest::html_elements(".section__title, .article-section__title, .section p, .abstractSection p, figure, .article-table-content, .NLM_fn")

  df = tibble(tag = xml_name(nodes), xpath=xml_path(nodes), classes = html_attr(nodes,"class"),  text = html_text2(nodes), id = html_attr(nodes,"id"))

  df = df %>%
    mutate(is_figure = tag == "figure") %>%
    mutate(is_table = tag == "div" & startsWith(classes,"article-table")) %>%
    mutate(
      type = case_when(
        startsWith(text, "Abstract") ~ "abs",
        tag == "p" ~ "p",
        tag == "h2" ~ "sec1",
        tag == "h3" ~ "sec2",
        tag == "h4" ~ "sec3",
        is_figure ~ "fig",
        is_table ~ "tab",
        tag == "span" ~ "note",
        TRUE ~ "unknown"
      )
    )

  # Removes duplicated table entries
  df$.ROW = 1:NROW(df)
  dupl = duplicated(select(df, tag, type, text))
  df = df[!dupl,]

  df = remove_nested_html_elements(df, which(df$tag == "div"))
  df = text_df_standardize(df)
  nodes = nodes[df$.ROW]

  inds = which(df$type == "tab")
  tab_df = bind_rows(lapply(seq_along(inds), function(i) {
    # same table structure as jpe
    jpe_parse_html_table(nodes[[inds[i]]],i)
  }))

  list(text_df=df, tab_df = tab_df)

}

jpe_parse_html_table = function(node, tab_counter) {
  restore.point("jpe_parse_html_table")

  tabname = html_node(node, ".captionLabel") %>% html_text2()
  tabname = gsub("Â","", tabname) %>% trimws()
  tabid = tabname_to_tabid(tabname)
  tabtitle = html_node(node, "figcaption") %>% html_text2()
  tabtitle = gsub("Â","", tabtitle) %>% trimws()


  tabnotes = html_node(node, ".tableFooter") %>% html_text2()

  tab_node = html_node(node, "table")

  tab_rows = html_nodes(node, "table > tr, table > thead > tr, table > tbody > tr")

  cell_df = html_table_cells_from_all_tr(tab_rows)

  tabsource = as.character(tab_node) %>% merge.lines()

  tab_df = tibble(tabid=tabid, tab_counter=tab_counter, tabtitle = tabtitle, tabnotes = tabnotes, cell_df = list(cell_df), tabsource = tabsource) %>%
    art_html_tab_standardize()
  tab_df

}

