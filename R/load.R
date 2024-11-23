
art_load_text_parts = function(project_dir) {
  file = file.path(project_dir, "art","text_parts.Rds")
  if (!file.exists(file)) return(NULL)
  text_df = readRDS(file)
  text_df
}

art_load_tab_df = function(project_dir, route=NULL) {
  if (is.null(route)) {
    file = file.path(project_dir, "art","arttab.Rds")
    if (file.exists(file)) return(readRDS(file))
    route = get_art_route()
  }
  file = file.path(project_dir, "art/routes",route, "arttab.Rds")
  if (file.exists(file)) return(readRDS(file))

  return(NULL)
}
