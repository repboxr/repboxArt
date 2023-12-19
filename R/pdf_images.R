# Attempts to extract image information from a PDF
# not yet well working
repbox.extract.pdf.images = function(project_dir) {
  pdf.file=list.files(paste0(project_dir,"/pdf"), glob2rx("*.pdf"), full.names = TRUE)[1]
  img.dir = paste0(project_dir,"/repbox/pdf-images")
  if (!dir.exists(img.dir))
    dir.create(img.dir)



  cmd = paste0('pdfimages -p "',pdf.file,'" "', img.dir,'"')
  cat(cmd)
  system(cmd)
}
