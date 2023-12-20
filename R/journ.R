guess_journ = function(project_dir, opts=NULL) {
  if (!is.null(opts[["journ_format"]])) return(opts$journ_format)

  artid = basename(project_dir)
  journ = str.left.of(basename(project_dir),"_",not.found = "unknown")
  if (!journ %in% repbox_journ_list()) return("unknown")
  if (journ != "aer") return(journ)
  if (is_aer_pandp(artid)) return("aer_pandp")
  return(journ)
}

#' List of journals for which special detection heuristics (sections, footnotes etc.) are implemented.
repbox_journ_list = function() {
  c(
    "American Economic Review" = "aer",
    "American Economic Review P&P" = "aer_pandp",
    "AEJ Applied" = "aejapp",
    "AEJ Policy" = "aejpol",
    "AEJ Micro" = "aejmic",
    "AEJ Macro" = "aejmac"
   # "Management Science" = "ms",
  #  "Quarterly Journal of Economics" = "qje"
  )
}

is_aer_pandp = function(artid) {
  journ = str.left.of(artid, "_")
  if (journ != "aer") return(FALSE)

  str = str.right.of(artid, "_")
  vol = str.left.of(str,"_") %>% as.integer()
  str = str.right.of(str, "_")
  issue = str.left.of(str,"_") %>% as.integer()
  # Only Papers and Proceeding Issues are a problem
  if ((vol <= 103 & issue==3) | (vol>=104 & issue==5)) return(TRUE)
  return(FALSE)
}
