plines_to_lines = function(plines, pages, page_df) {
  add = page_df$page_start_line[pages]-1
  plines + add
}

lines_to_plines = function(lines,pages=lines_to_pages(lines, page_df), page_df) {
  restore.point("lines_to_plines")
  lines - page_df$page_start_line[pages] +1
}

lines_to_pages = function(lines, page_df) {
  restore.point("lines_to_plines")
  last = NROW(page_df)
  pstart = c(page_df$page_start_line,page_df$page_start_line[last]+page_df$num_lines[last]+1)
  findInterval(lines, pstart)
}
