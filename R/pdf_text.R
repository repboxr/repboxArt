# Copy articles text to supplement
# and run table extraction
# repbox_match then contains functions to compare results

example = function() {
  library(repbox)
  project_dir = "~/repbox/projects_gha/jeea_19_5_11"
  art_ensure_correct_dirs(project_dir)
  art_pdf_to_txt_pages(project_dir, overwrite=TRUE)

  rstudioapi::filesPaneNavigate(project_dir)

  convert.art.pdf("/home/rstudio/statabox/supp/aejapp_12_4_4")

  dir = "~/statabox/supp/aer_vol_106_issue_5_article_43"
  repbox.extract.art.tabs(dir, TRUE)
  repbox_match(dir,force = TRUE)
  force=FALSE
  copy.pdf.and.extract.tabs(parent.dir, pdf.dir, force=TRUE, overwrite=TRUE)
}

art_get_pdf_files = function(project_dir, full.names=TRUE) {
  pdf.dir = file.path(project_dir,"art","pdf")
  list.files(pdf.dir, glob2rx("*.pdf"),full.names = full.names)
}

art_pdf_to_txt_pages = function(project_dir, overwrite = FALSE, save_txt_file=TRUE) {
  restore.point("art_all_pdf_to_txt_pages")
  pdf.files = art_get_pdf_files(project_dir)
  out.file = file.path(project_dir,"art","txt_pages.Rds")
  if (file.exists(out.file) & !overwrite) return()

  page_li = lapply(pdf.files, pdf_to_txt_pages)
  page_df = bind_rows(page_li)
  if (NROW(page_df)>0) {
    page_df$page_start_line = c(1, (cumsum(page_df$num_lines)+1)[-NROW(page_df)])
    # If we have multiple files just continue page numbering
    # Makes it easier to convert between page lines and other lines
    page_df$page_in_file = page_df$page
    page_df$page = 1:NROW(page_df)
  }



  saveRDS(page_df, out.file)
  if (save_txt_file & NROW(page_df)>0) {
    writeLines(page_df$txt, file.path(project_dir,"art","art.txt"))
  }
  invisible(page_df)

}

pdf_to_txt_pages = function(pdf.file, from.page=1, to.page=200, enc="UTF-8") {
  restore.point("pdf_to_txt_by_page")
  page = 1
  txt_vec = rep("", to.page)
  txt_lines = rep(NA, to.page)

  counter = 0
  for (page in from.page:to.page) {
    cmd = paste0('pdftotext -q -f ', page, ' -l ', page, ' -layout "',pdf.file,'" -')
    res = suppressWarnings(system(cmd,intern = TRUE))
    if (length(res)==0) break
    res = first_repair_art_pdf_text(res)
    counter = counter+1
    txt_lines[counter] = length(res)
    txt_vec[counter] = merge.lines(res)

  }
  if (counter==0) return(NULL)

  page_df = tibble(source_file = basename(pdf.file), page = from.page:(from.page+counter-1), txt = txt_vec[1:counter], num_lines = txt_lines[1:counter])
  page_df$txt = substitute_wrong_pdf_txt_chars(page_df$txt)
  page_df
}


first_repair_art_pdf_text = function(txt) {
  restore.point("first_repair_art_pdf_text")

  trim_txt = trimws(txt)
  # Remove download info in JEEA text
  # and empty rows above it
  # This info can even destroy tables...
  rows = which(startsWith(trim_txt, "Downloaded from https://academic.oup.com"))

  if (length(rows)>0) {
    restore.point("first_repair_art_pdf_text2")
    rem_rows = txt

    filled_rows = which(trim_txt != "")
    inds = match(rows, filled_rows)
    start = ifelse(inds > 1,
      filled_rows[inds-1]+1,
      1
    )
    del_rows = unlist(lapply(seq_along(rows), function(i) start[i]:rows[i]))
    txt = txt[-del_rows]
  }

  return(txt)
}

substitute_wrong_pdf_txt_chars = function(txt) {
  txt = gsub("⫺","-",txt, fixed=TRUE)
  txt = gsub("�","-",txt, fixed=TRUE)
  txt = gsub("â€‹","  ",txt, fixed=TRUE)
  txt
}



art_load_txt_pages = function(project_dir) {
  restore.point("art_load_txt_pages")
  out.file = file.path(project_dir,"art","txt_pages.Rds")
  if (!file.exists(out.file)) return(NULL)
  readRDS(out.file)
}
