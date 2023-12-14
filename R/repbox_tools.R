guess_journ = function(project.dir) {
  artid = basename(project.dir)
  journ = str.left.of(basename(project.dir),"_")
  if (journ != "aer") return(journ)
  if (is_aer_pandp(artid)) return("aer_pandp")
  return(journ)
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
