example = function(){
  project_dir = "~/repbox/projects_reg/aejpol_3_4_8"
  project_dir = "~/repbox/projects_reg/testart"
  project_dir = "~/repbox/projects_reg/aejapp_3_1_3"
  project_dir = "~/repbox/projects_reg/aejapp_3_2_2"
  art_extract_raw_tabs(project_dir, overwrite=TRUE, by_page=FALSE)
}

art_load_tabs = function(project_dir) {
  restore.point("art_load_tabs")

  tab.file = file.path(project_dir, "art", "arttab.Rds")
  if (!file.exists(tab.file)) return(NULL)
  readRDS(tab.file)
}

art_extract_pdf_tabs = function(project_dir, overwrite=FALSE, by_page=FALSE, page_df=NULL) {
  restore.point("art_extract_tabs")

  tab.file = file.path(project_dir, "art", "arttab.Rds")
  if (file.exists(tab.file) & !overwrite) return(list())


  if (is.null(page_df)) {
    page_df = art_load_txt_pages(project_dir)
  }
  if (is.null(page_df)) {
    cat("\nNo pages extracted for article in ", project_dir,"\n")
    return(list())
  }

  raw = art_extract_pdf_raw_tabs(project_dir, overwrite, by_page, page_df=page_df)

  tab_df = ExtractSciTab::refine_raw_tab(raw)

  # We overwrite the panel and num_row_block definitions
  # to be more aligned with other table extraction methods
  # e.g. from HTML or later using AI
  i = 1
  for (i in seq_len(NROW(tab_df))) {
    res = refine_cell_df_and_add_panel_info(tab_df$cell_df[[i]])
    tab_df$cell_df[[i]] = res$cell_df
    tab_df$row_df[[i]] = res$row_df
    tab_df$panel_df[[i]] = res$panel_df
  }


  tab_df$start_page = lines_to_pages(tab_df$start_line,page_df)
  tab_df$end_page = lines_to_pages(tab_df$end_line, page_df)
  tab_df$start_pline = lines_to_plines(tab_df$start_line, tab_df$start_page, page_df)
  tab_df$end_pline = lines_to_plines(tab_df$end_line, tab_df$end_page, page_df)

  art = readRDS.or.null(file.path(project_dir, "repdb","art.Rds"))[[1]]
  pdf_url = art$pdf_url


  tab_df$pdf_file = page_df$source_file[tab_df$start_page]
  tab_df$html_file = ""
  if (!is_empty(pdf_url)) {
    tab_df$url_org_tab = paste0(pdf_url,"#page=", tab_parts$start_page)
  } else {
    tab_df$url_org_tab = ""
  }

  save_rds_create_dir(tab_df, file.path(project_dir, "art/routes/pdf", "arttab.Rds"))
  parcels = art_save_repdb_tab(project_dir=project_dir,tab_df = tab_df,route = "pdf")
  #saveRDS(tab_df,paste0(project_dir,"/art/arttab.Rds"))
  parcels
}

# Extract tabs using ExtractSciTab. Later we will modify it.
art_extract_pdf_raw_tabs = function(project_dir, overwrite=FALSE, by_page=FALSE, page_df=NULL, save=TRUE) {
  restore.point("art_extract_raw_tabs")
  raw_file = file.path(project_dir, "art", "art_tab_raw.Rds")
  if (file.exists(raw_file) & !overwrite) return(readRDS(raw_file))

  if (is.null(page_df)) {
    page_df = art_load_txt_pages(project_dir)
  }
  if (is.null(page_df)) {
    cat("\nNo pages extracted for article in ", project_dir,"\n")
    return(NULL)
  }

  raw = ExtractSciTab::extract_raw_tables_from_text(txt = merge.lines(page_df$txt))
  if (save) {
    saveRDS(raw,raw_file)
  }
  invisible(raw)
}

find_stars_str = function(big_str) {
  restore.point("find_stars_str")
  if (length(big_str)==0) return(NULL)
  digit_pos = stringi::stri_locate_last_regex(big_str, "[0-9]")[,1]
  digit_pos[is.na(digit_pos)] = 0
  right_str =  substring(big_str,digit_pos+1)
  right_str = gsub(" ", "", right_str)
  as.vector(stringi::stri_match_first_regex(right_str, "[*]+")) %>% na.val("")
}

art_save_repdb_tab = function(project_dir, tab_df, do_save=TRUE, route=get_art_route()) {
  restore.point("save_repdb_art_tab")
  library(repboxDB)

  artid = basename(project_dir)
  tab_df = tab_df %>% mutate(
    artid = artid,
    tabpos = tab_counter,
    num_panels = NROW(panel_df)

  )

  if (!has.col(tab_df,"tabnotes")) {
    tab_df$tabnotes = NA_character_
  }

  repdb_check_data(tab_df, "art_tab_source")
  repdb_check_data(tab_df, "art_tab")
  cell_df = bind_rows_with_parent_fields(tab_df, "cell_df",c("artid","tabid"))

  # Non-0 cellid will only be assigned to "num" cells
  cell_df$cellid = cumsum(cell_df$type=="num") * (cell_df$type=="num")

  repdb_check_data(cell_df, "art_tab_cell")


  parcels = list(art_tab = list(art_tab=tab_df), art_tab_cell=list(art_tab_cell=cell_df), art_tab_source = list(art_tab_source=tab_df))
  if (do_save) {
    repdb_save_parcels(parcels, file.path(project_dir,"art", "routes", route, "repdb"))
  }

  parcels

}



tabname_to_tabid = function(tabname) {
  restore.point("tabname_to_tabid")
  tabid = str.right.of(tabname, "able ") %>% trimws()
  tabid = gsub("\\.$","",tabid) %>% trimws()

  rows = which(startsWith(tolower(tabname),"appendix"))
  tabid[rows] = paste0("app.",tabid[rows])

  # Update: remove spaces and other special letter
  # We will later use tabid inside HTML ids: so it should
  # not have any special symbols anymore
  tabid = stringi::stri_replace_all_regex(tabid, "[^a-zA-Z0-9_]","")
  tabid = stringi::stri_replace_all_fixed(tabid, "TABLE","")
  tabid
}

tabtitle_to_tabid = function(tabtitle) {
  tabname_to_tabid(tabtitle)
}

