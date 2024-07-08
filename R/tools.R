save_rds_create_dir = function(x, file) {
  dir = dirname(file)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  saveRDS(x, file)
}

my_rank = function(x) {
  restore.point("my_rank")
  if (length(x)==0) return(integer(0))
  vals = unique(x)
  match(x, vals)
}

ensure_empty_types = function(x, cols, type="character") {
  if (NROW(x)>0) return(x)
  if (type=="character") {
    for (col in cols) {
      x[[col]] = character(0)
    }
  }
  x
}

rle_table = function(x) {
  rle = rle(x)
  start = c(1,cumsum(rle$lengths)+1)
  end = start[-1]-1
  start = start[-length(start)]
  tibble(start=start, end=end, length=rle$lengths, value = rle$values)
}

rle_block = function(x, ignore_val=NULL) {
  rle = rle(x)
  block_vals = seq_along(rle$values)
  if (length(ignore_val)>0) {
    zero_rows = rle$values %in% ignore_val
    block_vals[zero_rows] = 0
    uni = unique(block_vals[!zero_rows])
    block_vals[!zero_rows] = match(block_vals[!zero_rows],uni)
  }
  rep(block_vals, rle$lengths)
}

rle_cummax_block = function(x, ignore_val=NULL) {
  rle = rle(x)
  block_vals = seq_along(rle$values)
  if (length(ignore_val)>0) {
    zero_rows = rle$values %in% ignore_val
    block_vals[zero_rows] = 0
    uni = unique(block_vals[!zero_rows])
    block_vals[!zero_rows] = match(block_vals[!zero_rows],uni)
    block_vals = cummax(block_vals)
  }
  rep(block_vals, rle$lengths)
}


first.non.null = function(...) {
  args = list(...)
  for (val in args) {
    if (!is.null(val)) return(val)
  }
  return(NULL)

}

bind_rows_with_parent_fields = function(parent.df,df.field,  fields, parent.fields, parent.row.field = "..PARENT.ROW") {
  restore.point("bind_rows_with_parent_fields")
  li = parent.df[[df.field]]
  names(li) = NULL
  df = bind_rows(li, .id=parent.row.field)
  df[[parent.row.field]] = as.integer(df[[parent.row.field]])
  for (field in fields) {
    df[[field]] = parent.df[[field]][df[[parent.row.field]]]
  }
  use.cols = setdiff(colnames(df), parent.row.field)
  df = df[,use.cols]
}



loc_sep_lines = function(txt, loc) {
  restore.point("loc_sep_lines")
  lines_loc = stri_locate_all_fixed(txt, "\n")[[1]]
  lines_start = c(1, lines_loc[,1]+1)
  loc$row = findInterval(loc$start, lines_start)
  loc$col_start = loc$start-lines_start[loc$row]+1
  loc$col_end = loc$end-lines_start[loc$row]+1
  loc


}

readRDS.or.null = function(file) {
  if (!file.exists(file)) return(NULL)
  readRDS(file)
}

na.remove = function(x) x[!is.na(x)]

na.false = function(x) na.val(x, FALSE)

na.val = function(x, val=0) {
  x[is.na(x)] = val
  x
}

is.true = function(x) {
  val = x == TRUE
  val[is.na(x)] = FALSE
  val
}

most.common = function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

remove.cols = function(x, cols) {
  x = x[,setdiff(colnames(x),cols)]
}

left_join_overlap = function(x, y, by_x, by_y, mult="all", type="any", nomatch=NA) {
  dt_x = as.data.table(x)
  dt_y = as.data.table(y,key = by_y)
  dt_ol = foverlaps(dt_x, dt_y, mult=mult, type=type, which=FALSE, by.x=by_x)
  as_tibble(dt_ol)
}

match_overlap = function(xstart,xend,ystart,yend, mult="all", type="any") {
  restore.point("match_all_rounded")
  dt_x = data.table(ind_x = seq_along(xstart), xstart=xstart, xend=xend,key = c("xlow","xhigh"))
  dt_y = data.table(ind_y = seq_along(ystart), ystart=ystart, yend=yend,key = c("ylow","yhigh"))

  ol = foverlaps(dt_x, dt_y, mult=mult, type=type, which=TRUE, nomatch=NULL)
  res = tibble(ind_x=dt_x$ind_x[ol[[1]]],ind_y = dt_y$ind_y[ol[[2]]])
  res
}
# Remove from x all locations that overlap with a location in y
remove.overlapping.loc = function(x, y, by=c("line", "start","end"), mode=c("remove","keep","mark")[1]) {
  restore.point("remove.overlapping.loc")
  library(data.table)
  loc1 = as.data.table(x)
  setkeyv(loc1, by)

  loc2 = as.data.table(y)
  setkeyv(loc2, by)

  ol = foverlaps(loc1, loc2, by.x=by, by.y=by, which=TRUE, nomatch=NA)
  not.ol = sort(unique(ol$xid[is.na(ol$yid)]))
  if (mode=="mark") {
    x$overlapping = TRUE
    x$overlapping[!not.ol] = FALSE
    return(x)
  }
  if (mode =="remove") {
    x[not.ol,,drop=FALSE]
  } else if (mode == "keep") {
    if (length(not.ol)==0) return(x)
    x[-not.ol,,drop=FALSE]
  }
}

keep.overlapping.loc = function(...) {
  remove.overlapping.loc(..., mode="keep")
}


locate_all_as_df = function(txt, regexpr.li) {
  restore.point("locate_all_as_df")
  locs.li = lapply(regexpr.li, function(regexpr) {
    str_locate_all(txt, regexpr)
  })

  loc.df = bind_rows(lapply(seq_along(locs.li), function(i) {
    type = names(locs.li)[i]
    locs = bind_rows(lapply(seq_along(locs.li[[i]]), function(row) {
      loc = locs.li[[i]][[row]]
      if (NROW(loc)==0) return(NULL)
      loc = as_tibble(loc)
      loc$line = row
      loc$type = type
      loc$pos = 1:NROW(loc)
      loc
    }))
    locs
  }))
  if (NROW(loc.df)==0) {
    loc.df = tibble(start=integer(0),end=integer(0), line=integer(0), type=character(0), pos=integer(0))
  }

  loc.df$str = substring(txt[loc.df$line], loc.df$start, loc.df$end)
  loc.df$nchar = loc.df$end-loc.df$start
  loc.df
}


ends.with.text = function(txt, end.size=4) {
  str = trimws(txt)
  rhs = substring(str, nchar(str)-end.size,nchar(str))
  grepl("[a-zA-z]",rhs,fixed=FALSE)

}

seq_rows = function(x) {
  seq_len(NROW(x))
}

from_to = function(from, to, min=from, max=to) {
  from = max(from, min)
  to = min(to, max)
  if (from > to) return(NULL)
  from:to
}
