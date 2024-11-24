
art_load_text_parts = function(project_dir) {
  file = file.path(get_art_route_dir(project_dir), "art","text_parts.Rds")
  if (!file.exists(file)) return(NULL)
  text_df = readRDS(file)
  text_df
}

art_load_tab_df = function(project_dir) {
  file = file.path(get_art_route_dir(project_dir), "arttab.Rds")
  if (file.exists(file)) return(readRDS(file))

  return(NULL)
}
