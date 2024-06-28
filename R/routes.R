# There can be different routes to extract particular information from an article
#
# E.g. there may be both a PDF version and an HTML version from which tables can be extracted
# Also there can be different methods to extract tables from a PDF. Our current heuristic,
# or dfferent approaches using AI.
#
# The different routes to extract information may lead to different results.
# Ex-ante it may not always clear what is the best route. So it seems helpful
# to store results of different routes and have functionality to switch the preferred route,
# and compare routes

get_art_route = function() {
  route = getOption("repbox..art..route")
  if (is.null(route)) stop("No active route was set for repboxArt")
  route
}

set_art_route = function(route) {
  options(repbox..art..route = route)
  invisible(route)
}



# Copy data from route to main folders
activate_art_route = function(project_dir, route)  {
  repdb_files = list.files(file.path(project_dir, "art","routes", route, "repdb"),full.names = TRUE)
  dest_dir = file.path(project_dir, "repdb")
  if (!dir.exists(dest_dir)) dir.create(dest_dir)
  file.copy(repdb_files, dest_dir, overwrite = TRUE)

  files = list.files(file.path(project_dir, "art","routes", route),full.names = TRUE,include.dirs = FALSE,recursive = FALSE)
  dest_dir = file.path(project_dir, "art")
  file.copy(files, dest_dir,overwrite = TRUE)



}

load_art_route_parcels = function(project_dir, parcel_names, route=get_art_route(), parcels=list()) {
  restore.point("load_art_route_parcels")

  dir = file.path(project_dir, "art","routes", route, "repdb")
  files = paste0(dir, "/", parcel_names, ".Rds")
  parcels[parcel_names] = lapply(files, readRDS.or.null)
  parcels
}

route_art_tab_finish_route = function(project_dir, route, parcels=list()) {
  restore.point("route_art_tab_finish_route")
  set_art_route(route)


  cat("\n  2. Extract regression results from tables...")
  art_extract_paren_type_from_tab_notes(project_dir)

  parcels = art_tabs_to_regs(project_dir, opts=opts, parcels=parcels)
  cat("\n  3. Extract key phrases and map to references to figures, tables and columns...")
  art_phrase_analysis(project_dir)

  activate_art_route(project_dir,route)

  cat("\nDone with", project_dir,"\n")
  invisible(parcels)


}

route_art_tab_set = function(project_dir, route, adapt_reg=TRUE, do_save=TRUE) {
  file = paste0(project_dir, "/art/routes/", route,"/arttab.Rds")
  tab_df = readRDS.or.null(file)
  if (is.null(tab_df)) {
    cat(paste0("\nNo article tables extracted for route '", route,"'\n"))
    return(tab_df)
  }
  parcels = art_save_repdb_tab(project_dir, tab_df)
}
