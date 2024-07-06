# Add panel and row_block information to extracted tables
#
# Normalize cell_df for tables extracted from HTML or PDF
refine_cell_df_and_add_panel_info = function(cell_df) {
  restore.point("refine_cell_df_and_add_panel_info")

  if (!has.col(cell_df,"num_deci")) {
    # We only extract number if it is at the beginning
    # of in a bracket
    fp = "([+\\-−]|([+\\-−] ))?(([0-9]*(,[0-9][0-9][0-9])+|([0-9]+))([.][0-9]*)?|[.][0-9]+)"
    fp_start = paste0("^", fp)

    cell_df = cell_df %>% mutate(
      nchar = nchar(text),
      # A num column should not start with any other text strings
      num_str = text %>%
        stri_replace_all_regex("[\\(\\)\\[\\]\\{\\}, \\*]","") %>%
        stri_replace_all_fixed("−","-") %>%
        stri_extract_first_regex(fp_start),
      num = suppressWarnings(as.numeric(num_str)),
      type = case_when(
        !is.na(num)~"num",
        nchar == 0 ~ "empty",
        TRUE ~ "text"
      ),
      num_deci = nchar(str.right.of(num_str,".", not.found=rep("", n())))
    )
  }

  cell_df = cell_df %>% mutate(
    has_paren = stri_detect_fixed(text,"("),
    has_bracket = stri_detect_fixed(text,"["),
    has_curley = stri_detect_fixed(text,"{"),
    # Something like (1), (5)
    is_int_header = type == "num" & num_deci==0 & has_paren & num <= 30 & num > 0,
    stars_str = find_stars_str(text),
    is_panel_title = startsWith(text,"Panel") & colspan > 1,
    paren_type = case_when(
      has_paren ~ "(",
      has_bracket ~ "[",
      has_curley ~ "{",
      TRUE ~ ""
    )
  )

  # TO DO: Currently panel titles get a separate panel number. Need to correct!
  row_df = cell_df %>%
    group_by(row) %>%
    summarise(
      is_int_header_block = sum(is_int_header)>0 & sum(!is_int_header==0),
      is_empty_row = all(type=="empty"),
      is_num_row = any(type=="num"),
      rowname = case_when(
        first(type)== "text" ~ first(text),
        first(type)=="empty" & nth(type,2,default="") == "text" ~ nth(text,2),
        TRUE ~ ""
      ),
      .has_panel_title = any(is_panel_title),
      .text = paste0(text, collapse=""),

    ) %>%
    ungroup() %>%
    mutate(
      num_row_block = rle_block( (is_num_row | is_empty_row) + is_int_header_block, ignore_val=FALSE),
      panel_num = rle_block(.has_panel_title)-1
    )

  panel_df = filter(row_df, .has_panel_title) %>%
    select(panel_title=.text, panel_num)

  cols = setdiff(colnames(row_df),c(".text",".has_panel_title"))
  row_df = row_df[,cols]

  cell_df = left_join(cell_df, select(row_df, row, num_row_block, panel_num), by="row")

  list(cell_df=cell_df, row_df=row_df, panel_df=panel_df)
}
