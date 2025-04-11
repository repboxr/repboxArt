# General helper functions to parse articles in HTML format
# In addition we have specialized functions for different journals



example = function() {
  library(rvest)
  project_dir = "~/repbox/projects_reg/testart"
  html.file = list.files(file.path(project_dir, "art","html"),glob2rx("jpe_*.html"), full.names = TRUE)[1]
  journ = str.left.of(basename(html.file),"_")

  # More robust encoding detection
  res = guess_encoding_and_read_html(html.file)
  html=res$doc
  #html = rvest::read_html(html.file)

  res = art_html_to_parts(project_dir, html = html, journ=journ)
  text_df = res$text_df
  tab_df = res$tab_df

  rstudioapi::filesPaneNavigate(project_dir)
}

art_html_to_parts = function(project_dir) {
  restore.point("art_html_to_parts")
  journ = str.left.of(basename(project_dir),"_")
  html.file = art_get_html_files(project_dir)[1]

  # More robust encoding detection
  res = guess_encoding_and_read_html(html.file)
  html=res$doc

  #html = rvest::read_html(html.file,encoding = "UTF-8")

  if (journ=="jpe" | journ=="jole") {
    res = jpe_parse_html(project_dir, html, html.file, journ)
  } else if (journ=="restat") {
    res = restat_parse_html(project_dir, html, html.file, journ)
  } else if (journ=="ecta") {
    res = ecta_parse_html(project_dir, html, html.file, journ)
  } else if (journ=="ms") {
    res = ms_parse_html(project_dir, html, html.file, journ)
  } else {
    # restud, qje, jeea have all very similar HTML format
    res = restud_parse_html(project_dir, html, html.file, journ)
  }

  if (is.null(res)) return(list())

  text_df = res$text_df; tab_df = res$tab_df

  # Some general adaptions
  text_df$nchar = nchar(text_df$text)


  # Text of tables (titles, notes, content) will be analyzed separately...
  tab_text = paste0("[Insert Table ", tab_df$tabid, " here]")
  text_df$text[which(text_df$type=="tab")]= tab_text

  tab_df$nchar_title = nchar(tab_df$tabtitle)
  tab_df$nchar_tabsource = nchar(tab_df$tabsource)
  tab_df$nchar_notes = nchar(tab_df$tabnotes)

  # Store results
  out.dir = file.path(project_dir, "art")
  if (!dir.exists(out.dir)) {
    dir.create(out.dir)
  }

  save_rds_create_dir(text_df, file.path(project_dir, "art/routes/html", "text_parts.Rds"))
  save_rds_create_dir(text_df, file.path(project_dir, "art/", "text_parts.Rds"))
  save_rds_create_dir(tab_df, file.path(project_dir, "art/routes/html", "arttab.Rds"))
  save_rds_create_dir(tab_df, file.path(project_dir, "art", "arttab.Rds"))
  writeLines(text_df$text, file.path(project_dir, "art/routes/html","art_text.txt"))


  tab_df$pdf_file = NA_character_
  tab_df$html_file = html.file
  tab_df$url_org_tab = NA_character_
  tab_df$start_page = NA

  #saveRDS(text_df, file.path(out.dir,"text_parts.Rds"))
  #saveRDS(tab_df, file.path(out.dir, "arttab.Rds"))
  #writeLines(text_df$text, file.path(out.dir,"art_text.txt"))
  #invisible(list(text_df=text_df, tab_df=tab_df))
  parcels = art_save_repdb_tab(project_dir, tab_df, route="html")
  parcels
}


# TO DO: Move footnotes to end of df:
# This seems useful for mapping table references since footnotes
# may refer to new tables. The idea is that footnotes are mainly self-contained.
text_df_standardize = function(df, reduce_cols = TRUE) {
  restore.point("text_df_standardize")
  df = text_df_add_section_cols(df)

  # Move footnotes to back, so that they don't confuse table references etc
  # But we keep the original section of footnotes
  #df$.ORG.ROW = df$.ROW
  #df$.ROW = ifelse(df$type=="note", df$.ROW + 1e9, df$.ROW)
  df = df %>% arrange(type == "note", .ROW)
  #df$.ROW = seq_len(NROW(df))


  # Add type counter
  df$type_counter = NA_integer_
  for (type in unique(df$type)) {
    rows = which(df$type == type)
    df$type_counter[rows] = seq_along(rows)
  }

  df$partind = seq_len(NROW(df))

  cols = c("partind","type","type_counter","tag","sec1","sec2","sec3", "text", ".ROW")
  abs_row = which(df$type=="abs")[1]
  sec_row = which(startsWith(df$type,"sec"))[1]
  if (!is.null(abs_row) & !is.null(sec_row)) {
    if (sec_row > abs_row) {
      df$sec1[(abs_row):(sec_row-1)] = 0
    }
  }

  if (!reduce_cols) {
    cols = union(cols, colnames(df))
  }
  df[,cols]
}

remove_nested_html_elements = function(df, container.rows) {
  restore.point("remove_nested_html_elements")
  keep = rep(TRUE, NROW(df))
  for (crow in container.rows) {
    cpath = df$xpath[crow]

    look.ahead = pmin(10, NROW(df)-crow)
    if (look.ahead<=0) next
    look.rows = crow+(1:look.ahead)

    is_child = startsWith(df$xpath[look.rows], cpath)
    keep[look.rows[is_child]] = FALSE

    if (!all(is_child)) next

    look.ahead = NROW(df)-crow
    if (look.ahead<=10) next
    look.rows = crow+(11:look.ahead)
    is_child = startsWith(df$xpath[look.rows], cpath)
    keep[look.rows[is_child]] = FALSE
  }
  df[keep,]
}

text_df_add_section_cols = function(df) {
  restore.point("text_df_add_section_cols")

  df$sec3 = df$sec2 = df$sec1 = 0

  sec1.rows = which(df$type == "sec1")
  df$sec1[sec1.rows] = seq_along(sec1.rows)

  ranges = c(sec1.rows, NROW(df)+1)
  for (i in seq_along(sec1.rows)) {
    df$sec1[ranges[i]:(ranges[i+1]-1)] = i
  }

  # Subsections end when there is either a new subsection or a new section!
  sec2.rows = which(df$type == "sec2")

  sec2ids = df %>%
    filter(type == "sec2") %>%
    group_by(sec1) %>%
    transmute(
      sec2 = sec1 + seq_len(n()) / 100
    )


  #df$sec2[sec2.rows] = seq_along(sec2.rows)
  starts = sec2.rows
  all.ends = unique(sort(c(sec2.rows, sec1.rows, NROW(df)+1)))
  ma = match(starts, all.ends)
  ends = all.ends[ma+1]-1



  for (i in seq_along(starts)) {
    df$sec2[starts[i]:ends[i]] = sec2ids$sec2[i]
  }

  # Subsections end when there is either a new subsection or a new section!
  sec3.rows = which(df$type == "sec3")
  #df$sec3[sec3.rows] = seq_along(sec3.rows)

  sec3ids = df %>%
    filter(type == "sec3") %>%
    group_by(sec1, sec2) %>%
    transmute(
      sec3 = ifelse(sec2==0, sec1 + seq_len(n()) / 10000, sec2 + seq_len(n()) / 10000)
    )


  starts = sec3.rows
  all.ends = unique(sort(c(sec3.rows, sec2.rows, sec1.rows, NROW(df)+1)))
  ma = match(starts, all.ends)
  ends = all.ends[ma+1]-1

  for (i in seq_along(starts)) {
    df$sec3[starts[i]:ends[i]] = sec3ids$sec3[i]
  }

  # Now standardize sec2 and sec3 numbers
  # We assume that no section has more than 99 subsections

  df = df %>% mutate(
    sec1 = ifelse(sec1==0, NA_integer_, sec1),
    sec2 = ifelse(sec2==0, NA_integer_, sec2),
    sec3 = ifelse(sec3==0, NA_integer_, sec3)
  )
  df
}

html_table_cells_from_all_tr = function(tab_rows) {
  i = 1
  cell_df = lapply(seq_along(tab_rows), function(i) {
    row_df = html_table_cells_from_tr(tab_rows[[i]],i)
  }) %>% bind_rows()
  cell_df
}


html_table_cells_from_tr = function(tab_row, row_num) {
  restore.point("html_extract_table_row_cells")
  cells = html_children(tab_row)
  tags = xml_name(cells)
  use = tags %in% c("th","td")
  cells = cells[use]
  tags = tags[use]

  cells2 = html_elements(tab_row, "th, td")

  if (length(cells2) != length(cells)) {
    restore.point("skjdhjkshdjkhsjdhjk")
    stop("We cannot yet deal with nested HTML tables.")
  }
  num_cols = html_attr(cells, "colspan") %>% as.numeric()
  num_cols[is.na(num_cols)] = 1

  row_df = tibble(row=row_num, cell_pos = seq_along(tags),colspan = num_cols) %>%
    mutate(col_end = cumsum(colspan),
           col_start = col_end - colspan+1,
           text = html_text2(cells)) %>%
    select(row, col = col_start, col_end, everything())
  row_df
}


art_get_html_files = function(project_dir, full.names=TRUE) {
  dir = file.path(project_dir,"art","html")
  list.files(dir, glob2rx("*.html"),full.names = full.names)
}


art_html_tab_standardize = function(tab_df) {
  restore.point("art_html_tab_standardize")
  tab_df$row_df = tab_df$panel_df = vector("list",NROW(tab_df))

  for (i in seq_rows(tab_df)) {
    res = html_tab_cell_row_panel_df(tab_df[i,])
    tab_df$cell_df[[i]] = res$cell_df
    tab_df$row_df[[i]] = res$row_df
    tab_df$panel_df[[i]] = res$panel_df
  }
  tab_df
}

# Normalize cell_df for Table from HTML
html_tab_cell_row_panel_df = function(tab) {
  restore.point("html_tab_cell_row_panel_df")

  cell_df = tab$cell_df[[1]]

  res = refine_cell_df_and_add_panel_info(cell_df)

  return(res)
  # # We only extract number if it is at the beginning
  # # of in a bracket
  # fp = "([+\\-−]|([+\\-−] ))?(([0-9]*(,[0-9][0-9][0-9])+|([0-9]+))([.][0-9]*)?|[.][0-9]+)"
  # fp_start = paste0("^", fp)
  #
  # cell_df = cell_df %>% mutate(
  #   nchar = nchar(text),
  #   # A num column should not start with any other text strings
  #   num_str = text %>%
  #     stri_replace_all_regex("[\\(\\)\\[\\]\\{\\}, \\*]","") %>%
  #     stri_replace_all_fixed("−","-") %>%
  #     stri_extract_first_regex(fp_start),
  #   num = suppressWarnings(as.numeric(num_str)),
  #   type = case_when(
  #     !is.na(num)~"num",
  #     nchar == 0 ~ "empty",
  #     TRUE ~ "text"
  #   ),
  #   num_deci = nchar(str.right.of(num_str,".", not.found=rep("", n()))),
  #   has_paren = stri_detect_fixed(text,"("),
  #   has_bracket = stri_detect_fixed(text,"["),
  #   has_curley = stri_detect_fixed(text,"{"),
  #   # Something like (1), (5)
  #   is_int_header = type == "num" & num_deci==0 & has_paren & num <= 30 & num > 0,
  #   stars_str = find_stars_str(text),
  #   is_panel_title = startsWith(text,"Panel") & colspan > 1,
  #   paren_type = case_when(
  #     has_paren ~ "(",
  #     has_bracket ~ "[",
  #     has_curley ~ "{",
  #     TRUE ~ ""
  #   )
  # )
  #
  # row_df = cell_df %>%
  #   group_by(row) %>%
  #   summarise(
  #     is_int_header_block = sum(is_int_header)>0 & sum(!is_int_header==0),
  #     is_empty_row = all(type=="empty"),
  #     is_num_row = any(type=="num"),
  #     rowname = case_when(
  #       first(type)== "text" ~ first(text),
  #       first(type)=="empty" & nth(type,2,default="") == "text" ~ nth(text,2),
  #       TRUE ~ ""
  #     ),
  #     .has_panel_title = any(is_panel_title),
  #     .text = paste0(text, collapse=""),
  #
  #   ) %>%
  #   ungroup() %>%
  #   mutate(
  #     num_row_block = rle_block( (is_num_row | is_empty_row) + is_int_header_block, ignore_val=FALSE),
  #     panel_num = rle_block(.has_panel_title)-1
  #   )
  #
  # panel_df = filter(row_df, .has_panel_title) %>%
  #   select(panel_title=.text, panel_num)
  #
  # cols = setdiff(colnames(row_df),c(".text",".has_panel_title"))
  # row_df = row_df[,cols]
  #
  # cell_df = left_join(cell_df, select(row_df, row, num_row_block, panel_num), by="row")
  #
  # list(cell_df=cell_df, row_df=row_df, panel_df=panel_df)

}

# Convert cell_df to a simple HTML table
cell_df_to_tabhtml = function(cell_df, color_by=NULL) {

  if (!is.null(color_by)) {
    val = cell_df[[color_by]]
    vals = unique(val)
    color_ind = match(cell_df[[color_by]], vals)
    colors = repboxHtml::cmd_ind_colors(length(vals))

    color = colors[color_ind]
    color[is.na(val)] = "#efefef"
    cell_df$td_style = paste0(' style = "background-color: ', color,';"')
  } else {
    cell_df$td_style = ""
  }

  tr_df = cell_df %>%
    group_by(row) %>%
    summarize(
      html = paste0('<tr data-row="', first(row),'">\n',
                    paste0('  <td data-row="', row,'" data-col="',col,'"', td_style,ifelse(is.true(colspan>1),paste0(' colspan="', colspan,'"'), ''), '>',text,                                   "</td>", collapse = "\n"),
                    '\n</tr>')
    )
  tabhtml = paste0("<table>\n",paste0(tr_df$html, collapse="\n"),"</table>")
  tabhtml
}

show_cell_df_html = function(cell_df, color_by=NULL, temp_dir = "~/repbox/temp") {
  html = cell_df_to_tabhtml(cell_df, color_by = color_by)
  file = paste0(temp_dir,"/temp_table.html")
  writeLines(html, file)
  browseURL(file)

}


