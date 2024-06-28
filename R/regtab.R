example = function() {
  make_phrases_def()
  p_def = get_phrases_def()


  project_dir = "~/repbox/projects_reg/aejpol_3_4_8"
  project_dir = "~/repbox/projects_reg/aejapp_3_2_2"
  #project_dir = "~/repbox/projects_reg/aejapp_3_1_3"

  opts = repbox_art_opts(single_line_regs = tibble(tabid=c("2","3")))
  reg_df = art_tabs_to_regs(project_dir, opts)

  rstudioapi::filesPaneNavigate(project_dir)

  art_extract_raw_tabs(project_dir,overwrite = TRUE)
  art_pdf_pages_to_parts(project_dir)

  #reg_df = tab_df$coef_df[[7]]
}

art_tabs_to_regs = function(project_dir, opts = repbox_art_opts(), parcels=list(), route=get_art_route()) {
  restore.point("art_tabs_to_regs")
  tab_df = art_load_tabs(project_dir)
  #tab_df = tab_df[4,]
  if (NROW(tab_df)==0) return(NULL)

  parcels = load_art_route_parcels(project_dir, "art_tab_cell",route, parcels = parcels)

  #cell_df = tab_df_to_cell_df(tab_df)
  cell_df = parcels$art_tab_cell$art_tab_cell
  cell_df$.ROW = seq_rows(cell_df)
  row_df = tab_df_to_row_df(tab_df)

  pair_df = cell_df_find_num_paren_pairs(cell_df)
  if (NROW(pair_df)==0) return(parcels)

  pair_df = left_join(pair_df, select(row_df,tabid,row, lab1=rowname), by=c("tabid","row"))
  pair_df = left_join(pair_df, select(row_df,paren_row=row, tabid, lab2=rowname), by=c("tabid","paren_row"))
  pair_df = pair_df %>%
    mutate(label = case_when(
      row == paren_row ~ lab1,
      lab2 == "" ~ lab1,
      lab1 == "" ~ lab2,
      TRUE ~ paste(lab1, lab2)
    )) %>%
    mutate(stars_str = case_when(
      coef_stars_str == "" ~ paren_stars_str,
      TRUE ~ coef_stars_str    )) %>%
    select(-lab1, -lab2, -coef_stars_str, -paren_stars_str) %>%
    # This coef_pos computation is just temporary
    # since in the next step some wrong coef candidates
    # might ve dropped
    group_by(tabid, panel_num,num_row_block, col, paren_pos) %>%
    mutate(.coef_pos = my_rank(row)) %>%
    ungroup()


  # Remove some pairs that are unlikely regression coefficients
  pair_df = pair_df %>%
    filter(
      # These integers likely just describe some column names
      # 100 1000 or 10000 can be part of column title (in 1000)
      !(.coef_pos = 1 & coef_num_deci == 0 & paren_num_deci==0 & (coef < 20 | coef %in% c(100,1000,10000, 100000)) & (paren < 20 | paren %in% c(100,1000,10000, 100000)))
      #!(label == "")
    ) %>%
    select(-.coef_pos)


  if (NROW(pair_df)==0) return(parcels)

  # After matching is done, we might detect that in a table
  # Each row (including the paren_row below) corresponds
  # to a separate regression. That is sometimes the case
  # in newer tables that tend not to show control variables.
  # An example are tables 2 and 3 in aejapp_3_2_2
  #
  # Then one can specify that in a rerun with the option
  # single_line_regs. Here we deal with that case
  slr = opts$single_line_regs
  if (identical(slr, TRUE)) {
    # All article regressions are considered to be single line regressions
    pair_df = pair_df %>% mutate(
      reg_row_block = seq_len(n())
    )
  } else if (is.data.frame(slr)) {
    # Only certain regressions (tables or panels) are
    # considered single line regressions
    by_cols = colnames(slr)
    slr$single_line_reg = TRUE
    pair_df = left_join(pair_df, slr, by=by_cols)
    pair_df = pair_df %>% mutate(
      reg_row_block = num_row_block*10000L + ifelse(is.true( single_line_reg),row,0L)
    )
  } else {
    pair_df = pair_df %>% mutate(
      reg_row_block = num_row_block
    )
  }


  # Nest coef_df and create helper cols
  reg_df = pair_df %>%
    group_by(tabid, panel_num, num_row_block, reg_row_block, col, paren_pos) %>%
    mutate(
      first_coef_row = min(row),
      last_coef_row = max(row),
      ncoef = n()
    ) %>%
    tidyr::nest(coef_df = c(row,label, coef_str:coef_num_deci, paren_row, paren_str:paren_num_deci,  stars_str)) %>%
    group_by(tabid, col) %>%
    mutate(
      last_cand_stat_row = lead(first_coef_row-1) %>% na.val(Inf)
    )

  # Add regcol and regid
  reg_df$regid = seq_rows(reg_df)

  cols = sort(unique(reg_df$col))
  reg_df$regcol = match(reg_df$col, cols)

  stat_df = art_extract_regstats(cell_df, row_df, reg_df)


  cols = c("tabid", "panel_num","regid", "regcol", "paren_pos", "ncoef",  "coef_df", "col", "paren_col")
  reg_df = reg_df[,cols]


  # Save in internal format
  saveRDS(reg_df, file.path(project_dir,"art/routes",route,"reg_df.Rds"))

  # Save in repdb format
  new_parcels = art_reg_save_repdb(project_dir, reg_df, stat_df, cell_df, route=route)

  parcels[names(new_parcels)] = new_parcels

  invisible(parcels)
}


art_extract_regstats = function(cell_df, row_df, reg_df) {
  restore.point("art_extract_reg_stats")

  cell_df$cell_ind = seq_rows(cell_df)
  # Match regression statistics: currently num_obs and r2
  row_df = row_df %>% mutate(phrase = tolower(stri_replace_all_regex(rowname,"[^A-Za-z0-9 ]","")))
  key_def = art_reg_stats_phrases()

  st_df = inner_join(select(row_df, tabid, row, tabid, panel_num, num_row_block,phrase),key_def,  by=c("phrase"))

  i = 19
  # Find matching regressions and cells
  stat_df = lapply(seq_rows(st_df), function(i) {
    reg_rows = which(
      reg_df$last_coef_row < st_df$row[i] &
      reg_df$tabid == st_df$tabid[i] &
      #  We want to match also stats that are actually in a later
      # panel since sometimes rows like
      # controls: yes yes yes
      # force a new panel
      reg_df$panel_num <= st_df$panel_num[i] &
      #reg_df$panel_num == st_df$panel_num[i]) &

      # This condition rules out double assignments of the statistic
      # to multiple regressions
      reg_df$last_cand_stat_row >= st_df$row[i]
    )

    if (length(reg_rows)==0) return(NULL)

    cell_rows = which(cell_df$row == st_df$row[i] & cell_df$tabid == st_df$tabid[i])

    # Now match to the value in cell_df
    rc_rows = cell_rows[match(reg_df$col[reg_rows], cell_df$col[cell_rows])]

    tibble(stat_name = st_df$key[i], regid=reg_df$regid[reg_rows], cell_ind= rc_rows)
#    , reg_panel_num = reg_df$panel_num[reg_rows], stat_panel_num =st_df$panel_num[i])
  }) %>% bind_rows()

  if (NROW(stat_df)==0) return(NULL)

  stat_df = left_join(stat_df, cell_df, by="cell_ind")

  # Some stat cells may not contain proper numbers
  # Reasons could be misalignment of columns (see aejapp_3_2_2 Table1)
  # We will then ignore those stats
  stat_df = filter(stat_df, !is.na(num))
  if (NROW(stat_df)==0) return(NULL)

  cols = c("stat_name", "regid", "row", "col","num_str", "num", "num_deci","cell_ind")
  stat_df[,cols]


}


art_reg_stats_phrases = function() {
  bind_rows(
    tibble(key="nobs", phrase = c("obs","observation","observations","n","no obs","n observations","number of observations")),
    tibble(key="r2", phrase = c("r2","rsquared","r squared")),
    tibble(key="adj_r2", phrase = c("adjusted r2", "r2 adj","r2 adjusted","adj r2","adjusted r 2","adj r 2")),
    tibble(key="ncluster", phrase = c("clusters")),
    tibble(key="r2_o", phrase = c("r2 overall")),
    tibble(key="r2_w", phrase = c("r2 within")),
    tibble(key="r2_b", phrase = c("r2 between")),
    tibble(key="pseudo_r2", phrase = c("pseudo r2")),
    tibble(key="F", phrase="fstatistic"),
    tibble(key="loglik", phrase = c("log likelihood")),
    tibble(key="pbar", phrase = c("predicted probability"))
  )



}

tab_df_to_cell_df = function(tab_df) {
  cell_df = bind_rows_with_parent_fields(tab_df,"cell_df","tabid")
  cell_df$.ROW = seq_rows(cell_df)
  cell_df
}

tab_df_to_row_df = function(tab_df) {
  row_df = bind_rows_with_parent_fields(tab_df,"row_df","tabid")
  row_df$.ROW = seq_rows(row_df)
  row_df
}


cell_df_find_num_paren_pairs = function(cell_df) {
  restore.point("cell_df_find_num_paren_pairs")
  if (NROW(cell_df)==0) return(NULL)
  num_cell_df = filter(cell_df, type=="num") %>%
    mutate(has_paren = paren_type %in% c("(","["))

  pair1 = num_cell_df %>%
    group_by(tabid, col, panel_num, num_row_block) %>%
    mutate(
      is_pair = is.true(lead(row)==(row+1) & (!has_paren) & (lead(has_paren))),
      .PAREN.ROW = lead(.ROW),
      paren_pos = "below"
    ) %>%
    filter(is_pair)

  pair2 = num_cell_df %>%
    group_by(tabid, row, panel_num, num_row_block) %>%
    mutate(
      is_pair = is.true(lead(col)==(col+1) & (!has_paren) & (lead(has_paren))),
      .PAREN.ROW = lead(.ROW),
      paren_pos = "beside"
    ) %>%
    filter(is_pair)

  pair = bind_rows(pair1, pair2)

  coef_part = select(pair, tabid, panel_num, num_row_block, paren_pos, row, col, coef_str=text, coef_num_str=num_str, coef=num, coef_num_deci=num_deci, coef_stars_str=stars_str)

  paren_part = select(num_cell_df[match(pair$.PAREN.ROW, num_cell_df$.ROW),], paren_row=row, paren_col=col, paren_str=text, paren_num_str=num_str, paren=num, paren_num_deci=num_deci, paren_stars_str=stars_str)
 bind_cols(coef_part, paren_part)
}


make_art_small_reg = function(reg_df) {
  restore.point("make_art_small_reg")
  max_regid = max(reg_df$regid)
  small_reg_df = reg_df %>%
    filter(ncoef > 1) %>%
    mutate(
      big_regid = regid,
      coef_pos = lapply(ncoef, seq_len)
    ) %>%
    unnest(coef_pos) %>%
    select(artid, regid, big_regid, coef_pos)
  small_reg_df$regid = seq_rows(small_reg_df) + max_regid
  small_reg_df
}

art_reg_save_repdb = function(project_dir, reg_df, stat_df, cell_df, route=get_art_route()) {
  restore.point("art_reg_save_repdb")
  library(repboxDB)
  specs = repdb_load_specs(libs="repboxArt")

  parcels = list()
  artid = basename(project_dir)
  reg_df = ungroup(reg_df)
  reg_df$artid = artid


  repdb_check_data(reg_df,"art_reg")
  parcels$art_reg = list(art_reg=reg_df)

  small_reg_df = make_art_small_reg(reg_df)
  repdb_check_data(small_reg_df,"art_small_reg")
  parcels$art_reg$art_small_reg = small_reg_df


  co = bind_rows_with_parent_fields(reg_df, "coef_df",c("artid","regid","col","paren_col"))

  if (NROW(co)>0) {
   co = co  %>%
      group_by(regid) %>%
      mutate(coef_pos = my_rank(row)) %>%
      ungroup()

   co = co %>%
     rename(coef_cell_row = row, coef_cell_col=col, paren_cell_row = paren_row, paren_cell_col=paren_col)
  } else {
    co=NULL
  }

  # join cellids for coef and paren
  co = co %>%
    left_join(select(reg_df, regid, tabid), by="regid") %>%
    left_join(select(cell_df, coef_cellid=cellid, tabid,coef_cell_col=col, coef_cell_row=row ), by=c("tabid","coef_cell_col","coef_cell_row")) %>%
    left_join(select(cell_df, paren_cellid=cellid, tabid,paren_cell_col=col, paren_cell_row=row ), by=c("tabid","paren_cell_col","paren_cell_row"))

  repdb_check_data(co,"art_regcoef")

  parcels$art_reg$art_regcoef = co


  if (NROW(stat_df) > 0) {
    stat_df$artid = artid

    # join cellid
    stat_df = stat_df %>%
      left_join(select(reg_df, regid, tabid), by="regid") %>%
      left_join(select(cell_df, cellid, tabid,col, row ), by=c("tabid","col","row"))

  } else {
    stat_df = NULL
  }



  repdb_check_data(stat_df, "art_regstat")
  parcels$art_reg$art_regstat = stat_df


  dir = file.path(project_dir,"art","routes", route, "repdb")
  repdb_save_parcels(parcels, dir)

  #rstudioapi::filesPaneNavigate(file.path(project_dir, "repdb"))

  invisible(parcels)
}




old.match_stat_to_reg_df = function(reg_df, cell_df, stats_df, key) {
  restore.point("match_stat_to_reg_df")
  st_df = stats_df[stats_df$key==key,]
  reg_df[[key]]= NA_real_
  if (NROW(st_df)==0) {
    return(reg_df)
  }

  i = 1
  for (i in 1:NROW(st_df)) {
    reg_rows = which(reg_df$last_coef_row < st_df$row[i] &
                       reg_df$last_cand_stat_row >= st_df$row[i] &
                       reg_df$panel_num == st_df$panel_num[i] &
                       reg_df$tabid == st_df$tabid[i])

    if (length(reg_rows)==0) next

    cell_rows = which(cell_df$row == st_df$row[i] & cell_df$tabid == st_df$tabid[i])

    # Now match to the value in cell_df
    rc_rows = cell_rows[match(reg_df$col[reg_rows], cell_df$col[cell_rows])]

    reg_df[[key]][reg_rows] = cell_df$num[rc_rows]
  }

  key_str = paste0(key,"_str")
  reg_df[[key_str]] = as.character(reg_df[[key]]) %>% stri_replace_all_regex("[ ,+]*","")
  reg_df[[key]] = as.numeric(reg_df[[key_str]])
  key_deci = paste0(key,"_num_deci")
  reg_df[[key_deci]] = as.numeric(reg_df[[key_str]])





  reg_df
}
