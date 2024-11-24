example = function() {
  library(repboxRun)
  library(repboxArt)
  library(repboxEJD)

  artid = "aejapp_1_3_6"
  projects.dir = "~/repbox/projects_test"
  #projects.dir = "~/repbox/projects_gha"
  #repbox_init_ejd_project(artid=artid, projects.dir=projects.dir)

  project_dir = paste0("~/repbox/projects_test/", artid)

  #project_dir = "/home/rstudio/repbox/projects_gha/jeea_18_2_13"
  #project_dir = "/home/rstudio/repbox/projects_gha/aejapp_1_1_10"
  #project_dir = "/home/rstudio/repbox/projects_gha/jep_30_3_1"

  steps = repbox_steps_from(art=TRUE, reproduction=FALSE)

  art_opts = repbox_art_opts(overwrite = TRUE)

  repboxRun::repbox_run_project(project_dir, lang="stata", steps=steps, opts=repbox_run_opts(art_opts = art_opts))

  #route_art_tab_finish_route(project_dir, "pdf", opts=art_opts)
  rstudioapi::filesPaneNavigate(project_dir)

  cell_df = get_art_tab_cell_with_reg_info(project_dir)

  cell_df = cell_df %>% filter(tabid=="8")
  names(cell_df)
  show_cell_df_html(cell_df, color_by="regid")
  show_cell_df_html(cell_df, color_by="num_row_block")

}

art_update_project = function(project_dir, overwrite = FALSE, opts = repbox_art_opts(overwrite=overwrite)) {
  restore.point("art_update_project")
  opts$project_dir = project_dir
  repbox_set_current_project_dir(project_dir)
  overwrite = opts$overwrite
  art_ensure_correct_dirs(project_dir)
  art = art_save_basic_info(project_dir)

  parcels = list()

  existing_routes = NULL
  if (art_has_pdf(project_dir) & "pdf" %in% opts$create_routes) {
    cat("\n  1. Transform pdf and extract tables...")
    set_art_route("pdf")
    existing_routes = c(existing_routes, "pdf")
    art_pdf_to_txt_pages(project_dir, overwrite = overwrite)
    parcels = art_extract_pdf_tabs(project_dir, overwrite = overwrite)
    art_pdf_pages_to_parts(project_dir, opts=opts)
  }
  if (art_has_html(project_dir) & "html" %in% opts$create_routes) {
    cat("\n  1. Transform html and extract tables...")
    set_art_route("html")
    res = art_html_to_parts(project_dir)
    if (length(res)>0) {
      existing_routes = c("html", existing_routes)
    } else {
      cat("\nArticle HTML for ", project_dir, " seems invalid.\n")
    }
  }
  if (!art_has_pdf(project_dir) & !art_has_html(project_dir)) {
    cat("\n No PDF or HTML file of article exists.")
    return(invisible(list()))
  }
  if (length(existing_routes)==0) {
    cat("\n No PDF or HTML file of article used.")
    return(invisible(list()))
  }

  route = intersect(opts$preferred_route,existing_routes)
  if (length(route)==0) {
    route = existing_routes[1]
  } else {
    route = route[1]
  }
  set_art_route(route)

  cat("\n  2. Extract regression results from tables...")
  art_extract_paren_type_from_tab_notes(project_dir, route=route)

  parcels = art_tabs_to_regs(project_dir, opts=opts, parcels=parcels)
  cat("\n  3. Extract key phrases and map to references to figures, tables and columns...")
  art_phrase_analysis(project_dir)

  activate_art_route(project_dir,route)

  cat("\nDone with", project_dir,"\n")
  invisible(parcels)
}


art_has_html = function(project_dir) {
  dir = file.path(project_dir, "art","html")
  if (!dir.exists(dir)) return(FALSE)

  files = list.files(dir, glob2rx("*.html"))
  length(files)>=1
}

art_has_pdf = function(project_dir) {
  dir = file.path(project_dir, "art","pdf")
  if (!dir.exists(dir)) return(FALSE)

  files = list.files(dir, glob2rx("*.pdf"))
  length(files)>=1
}


art_ensure_correct_dirs = function(project_dir) {
  restore.point("art_ensure_correct_dirs")

  art.dir = file.path(project_dir, "art")
  if (!dir.exists(art.dir)) dir.create(art.dir)
  pdf.dir = file.path(art.dir, "pdf")
  if(!dir.exists(pdf.dir)) {
    dir.create(pdf.dir)
  }
  old.pdf.dir = file.path(project_dir, "pdf")
  if (dir.exists(old.pdf.dir)) {
    pdf.files = list.files(old.pdf.dir, glob2rx("*.pdf"), ignore.case=TRUE, full.names=TRUE)
    try(file.copy(pdf.files, pdf.dir,overwrite = FALSE))
    file.remove(pdf.files)
    unlink(old.pdf.dir)
  }
}

version_repbox_art = function() {
  return(1L)
}

art_save_basic_info = function(project_dir) {
  restore.point("art_save_basic_info")
  library(repboxDB)
  ejd.file = file.path(project_dir, "meta", "art_meta.Rds")
  if (!file.exists(ejd.file)) {
    art = list()
  } else {
    art = readRDS(ejd.file)
  }
  art = set_missing_fields(art,
    artid = id,
    version_repbox_art = version_repbox_art(),
    appendix_pdf_url = NA,
    pdf_url = NA,
    html_url = NA,
    article_doi = NA,
    num_appendix_pdf = NA,
    num_appendix_html = NA,
    is_open_access = NA,
    art_license_type = NA
  )
  art = create.missing.cols(art,c("title", "journ", "vol", "issue", "year", "date", "article_url", "authors", "num_authors", "abstract"), NA)

  art$has_art_pdf = art_has_pdf(project_dir)
  art$has_art_html = art_has_html(project_dir)


  spec = repdb_get_spec("art")
  repdb_check_data(art, "art")
  parcels = list(art=list(art=art))
  repdb_save_parcels(parcels, file.path(project_dir, "repdb"))
  invisible(art)

}
