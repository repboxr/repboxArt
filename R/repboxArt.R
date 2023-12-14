example = function() {
  library(repboxArt)
  project.dir = "/home/rstudio/repbox/projects_reg/testart"
  project.dir = "~/repbox/projects_reg/aejapp_3_4_9"
  project.dir = "~/repbox/projects_reg/aejapp_3_2_2"
  overwrite = !TRUE
  art_update_project(project.dir, overwrite=overwrite)

  rstudioapi::filesPaneNavigate(project.dir)
  library(repboxHtml)
  project.dir = "~/repbox/projects_reg/aejapp_3_2_2"
  repbox_project_html(project.dir)

  html.dir = file.path(project.dir,"reports")
  rstudioapi::filesPaneNavigate(html.dir)


}

art_update_project = function(project.dir, overwrite = FALSE, opts = repbox_art_opts(overwrite=overwrite)) {
  restore.point("art_update_project")

  overwrite = opts$overwrite
  art_ensure_correct_dirs(project.dir)
  art = art_save_basic_info(project.dir)

  parcels = list()

  if (art_has_pdf(project.dir)) {
    cat("\n  1. Transform pdf and extract tables...")
    art_pdf_to_txt_pages(project.dir, overwrite = overwrite)
    art_extract_raw_tabs(project.dir, overwrite = overwrite)
    parcels = art_pdf_pages_to_parts(project.dir, opts=opts)
  }
  if (art_has_html(project.dir)) {
    cat("\n  1. Transform html and extract tables...")
    art_html_to_parts(project.dir)
  }
  if (!art_has_pdf(project.dir) & !art_has_html(project.dir)) {
    cat("\n No PDF or HTML file of article exists.")
    return(invisible())
  }
  cat("\n  2. Extract regression results from tables...")
  art_extract_paren_type_from_tab_notes(project.dir)

  parcels = art_tabs_to_regs(project.dir, opts=opts, parcels=parcels)
  cat("\n  3. Extract key phrases and map to references to figures, tables and columns...")
  art_phrase_analysis(project.dir)


  cat("\nDone with", project.dir,"\n")
  invisible(parcels)
}


art_has_html = function(project.dir) {
  dir = file.path(project.dir, "art","html")
  if (!dir.exists(dir)) return(FALSE)

  files = list.files(dir, glob2rx("*.html"))
  length(files)>=1
}

art_has_pdf = function(project.dir) {
  dir = file.path(project.dir, "art","pdf")
  if (!dir.exists(dir)) return(FALSE)

  files = list.files(dir, glob2rx("*.pdf"))
  length(files)>=1
}


art_ensure_correct_dirs = function(project.dir) {
  restore.point("art_ensure_correct_dirs")

  art.dir = file.path(project.dir, "art")
  if (!dir.exists(art.dir)) dir.create(art.dir)
  pdf.dir = file.path(art.dir, "pdf")
  if(!dir.exists(pdf.dir)) {
    dir.create(pdf.dir)
  }
  old.pdf.dir = file.path(project.dir, "pdf")
  if (dir.exists(old.pdf.dir)) {
    pdf.files = list.files(old.pdf.dir, glob2rx("*.pdf"), ignore.case=TRUE, full.names=TRUE)
    try(file.copy(pdf.files, pdf.dir,overwrite = FALSE))
    file.remove(pdf.files)
    unlink(old.pdf.dir)
  }
}

version_repbox_art = function() {
  return(0)
}

art_save_basic_info = function(project.dir) {
  restore.point("art_save_basic_info")
  library(repboxDB)
  ejd.file = file.path(project.dir, "meta", "ejd_art.Rds")
  if (!file.exists(ejd.file)) return(NULL)
  art = readRDS(ejd.file)
  art = art %>%
    mutate(
      artid = id,
      version_repbox_art = version_repbox_art(),
      appendix_pdf_url = NA,
      pdf_url = NA,
      html_url = NA,
      article_doi = NA,
      data_doi = NA,
      data_repo = repo
    )
  if (art$journ %in% c("aer","jep","aejmac","aejapp","aejmic","aejpol","pandp")) {
    #https://www.aeaweb.org/articles?id=10.1257/app.3.2.34
    #https://www.aeaweb.org/articles/pdf/doi/10.1257/app.3.2.34
    art$pdf_url = stri_replace_first_fixed(art$article_url, "/articles?id=","/articles/pdf/doi/")
    # 10.1257/app.3.2.34
    art$article_doi = str.right.of(art$article_url, "/articles?id=")
    if (is.na(art$repo)) {
      # Old data URL don't work anymore
      art$data_url = NA
    }
  }
  if (isTRUE(art$repo == "oi")) {
    # Don't see a fixed rule to map data DOI with data URL
    # I would need to better parse the OpenICPSR page to extract the DOI
    art$data_doi = NA
  } else if (!is.na(art$repo)) {
    stop("We should add code that generates the data doi based on the URL for the repo.")
  }

  spec = regdb_get_spec("art")
  regdb_check_data(art, "art")
  parcels = list(art=list(art=art))
  regdb_save_parcels(parcels, file.path(project.dir, "art","regdb"))
  invisible(art)

}