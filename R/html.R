# General helper functions to parse articles in HTML format
# In addition we have specialized functions for different journals



example = function() {
  library(rvest)
  project_dir = "~/repbox/projects_reg/testart"
  html.file = list.files(file.path(project_dir, "art","html"),glob2rx("jpe_*.html"), full.names = TRUE)[1]
  journ = str.left.of(basename(html.file),"_")
  html = rvest::read_html(html.file)

  res = art_html_to_parts(project_dir, html = html, journ=journ)
  text_df = res$text_df
  tab_df = res$tab_df

  rstudioapi::filesPaneNavigate(project_dir)
}

art_html_to_parts = function(project_dir, html = rvest::read_html(html.file), html.file=NULL, journ = str.left.of(basename(projec.dir,"_"))) {
  restore.point("article_parse_html")
  if (journ=="jpe") {
    res = jpe_parse_html(project_dir, html, html.file, journ)
  } else if (journ=="restat") {
    res = restat_parse_html(project_dir, html, html.file, journ)
  } else {
    # restud, qje, jeea have all very similar HTML format
    res = restud_parse_html(project_dir, html, html.file, journ)
  }
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



  saveRDS(text_df, file.path(out.dir,"text_parts.Rds"))
  saveRDS(tab_df, file.path(out.dir, "arttab.Rds"))
  writeLines(text_df$text, file.path(out.dir,"art_text.txt"))
  #invisible(list(text_df=text_df, tab_df=tab_df))
  parcels = art_save_regdb_tab(project_dir, tab_df)
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

