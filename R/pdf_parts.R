# Converts text pages generated from PDF file to
# text parts with sections
# as stored from HTML file

example = function() {

  library(repbox)
  library(repboxArt)
  project_dir = "~/repbox/projects_reg/aejapp_2_4_8"
  project_dir = "~/repbox/projects_reg/aejapp_3_1_3"

  art_extract_raw_tabs(project_dir,overwrite = TRUE)
  art_pdf_pages_to_parts(project_dir)
  rstudioapi::filesPaneNavigate(project_dir)

}



art_pdf_pages_to_parts = function(project_dir, journ = guess_journ(project_dir, opts), verbose=TRUE, opts=repbox_art_opts(), route = "pdf") {
  restore.point("art_pdf_pages_to_parts")
  pages_file = file.path( project_dir, "art","txt_pages.Rds")
  if (!file.exists(pages_file)) return(NULL)


  # Try to load meta data for article
  # in order to generate link to original tables
  art = readRDS.or.null(file.path(project_dir, "repdb","art.Rds"))[[1]]
  pdf_url = art$pdf_url


  page_df = readRDS(pages_file)

  txt = sep.lines(page_df$txt)


  line_df = tibble(line=seq_along(txt), txt=txt, trim_txt = trimws(txt), keep=TRUE, type = "", page = findInterval(line,page_df$page_start_line), tabid="",footind=0) %>%
    group_by(page) %>%
    mutate(
      pline = 1:n(),
      rev_pline = (n():1)
    ) %>%
    ungroup()

  line_df = line_df_find_page_header_footer(line_df, journ, opts=opts)
  lines = line_df$type != ""
  line_df$txt[lines] = line_df$trim_txt[lines] = ""

  # Try to repair two column articles
  if (art_has_two_col(project_dir, journ)) {
    line_df = art_repair_two_col(project_dir, line_df, journ)
  }

  # Candidate rows for section titles before
  # removing table and figure rows
  line_df = line_df_find_section_cands(line_df,journ=journ, opts=opts)

  # Table info from ExtractSciTab
  tab_file = file.path(get_art_route_dir(project_dir),"arttab.Rds")
  tab_parts = readRDS.or.null(tab_file)

  if (NROW(tab_parts)==0) {
    if (verbose) cat("\n    No tables found in the article.\n")
    tab_df = tab_parts = NULL
  }


  # Store info about spaces left and right for further characterization
  line_df = line_df %>% mutate(
    nchar = nchar(txt),
    space_left = nchar - nchar(trimws(txt,"left"))
  )
  txt_width = round(median(line_df$nchar[line_df$nchar > 40 & line_df$type==""]))
  line_df$txt_width = txt_width

  # Find Figures
  lines = line_df$type != ""
  line_df$txt[lines] = line_df$trim_txt[lines] = ""

  res = line_df_find_figures(line_df, journ, opts=opts)
  line_df = res$line_df; fig_df = res$fig_df


  # Find footnotes
  lines = line_df$type != ""
  line_df$txt[lines] = line_df$trim_txt[lines] = ""
  res = line_df_find_footnotes(line_df, journ, opts)

  line_df = res$line_df; fn_df = res$fn_df


  # Find section headers
  line_df$sec_num = 0

  line_df = line_df_find_sections(line_df, journ, opts=opts)

  # Find junk lines that will also be cut out
  line_df = line_df_find_junk_lines(line_df, journ, opts=opts)

  # Now transform to parts df
  parts_df = line_df_to_parts_df(line_df, fn_df)
  save_rds_create_dir(parts_df, file.path(project_dir, "art/routes/pdf/", "text_parts.Rds"))
  saveRDS(parts_df, file.path(project_dir, "art", "text_parts.Rds"))
  invisible()
}

line_df_to_parts_df = function(line_df, fn_df, verbose=TRUE) {
  restore.point("line_df_to_parts_df")
  line_df = filter(line_df,type %in% c("","sec1","bib","app"))

  # We remove Footnotes from text that are at the
  # end of a sentence like in
  # RDI does not predict income segregation.29
  # Such footnotes will prevent proper sentence
  # detection.
  line_df$txt = stri_replace_all_regex(line_df$txt,"(?<=[A-Za-z][\\.\\!\\?])[0-9]+(?=([ ]|$))","")

  # Replace Sentences like
  # Table 4.10 Now we
  line_df$txt = stri_replace_all_regex(line_df$txt,"(?<=[Table ][A-Z]?[0-9][\\.\\!\\?])[0-9]+(?=([ ][A-Z]|$))","")

  line_df$trim_txt = trimws(line_df$txt)


  sec_nums = unique(line_df$sec_num)

  sec_num = 0
  parts_df = lapply(sec_nums, function(sec_num) {
    restore.point("hskjdhjkshfjhskfh")
    lines = which(line_df$sec_num==sec_num)
    type = line_df$type[lines[1]]
    if (type %in% c("sec1","bib","app")) {
      head_line = first(lines)
      txt_lines = lines[-1]
    } else {
      head_line = NULL
      txt_lines = lines
    }
    if (type == "bib") {
      # For references we want to keep the original line breaks
      # and put all into a single paragraph
      para = merge.lines(line_df$txt[txt_lines])
    } else {
      txt = combine_text_lines(line_df$txt[txt_lines])
      para = sep.lines(txt) %>% trimws()
      para = combine_short_paragraphs(para)
    }

    par_tibble = tibble(type = "p", text = para, sec1=sec_num)
    if (length(head_line)==0) return(par_tibble)

    bind_rows(
      tibble(type = type, text = line_df$trim_txt[lines[1]], sec1 = sec_num),
      par_tibble
    )
  }) %>% bind_rows()
  parts_df$type_counter = 0
  for (type in unique(parts_df$type)) {
    rows = which(parts_df$type == type)
    parts_df$type_counter[rows] = seq_along(rows)
  }

  if (NROW(fn_df)>0) {
    fn_sec = max(parts_df$sec1)+1
    parts_df = bind_rows(
      parts_df,
      transmute(fn_df, type="note", text=trimws(txt), sec1=fn_sec, type_counter=seq_len(n()))
    )
    if (verbose) cat(paste0("\n    Found ", NROW(fn_df)," footnotes from ", first(fn_df$footind), " to ", last(fn_df$footind),"."))
  }
  parts_df$partind = seq_rows(parts_df)
  parts_df$nchar = nchar(parts_df$text)
  parts_df
}

combine_short_paragraphs = function(para, min_words=20) {
  para = para[para != ""]
  if (length(para)<=1) {
    return(para)
  }

  words = stri_count_words(para)
  comb = words < min_words
  comb[1] = FALSE

  last_ok = 1
  for (i in setdiff(seq_along(para),1)) {
    if (comb[i]) {
      para[last_ok] = paste0(para[last_ok]," ", para[i])
    } else {
      last_ok = i
    }
  }
  para = para[!comb]
  para
}

# Journal dependen additional junk stuff that can be neglected
# E.g. pages in the end that list which articles cite this one
line_df_find_junk_lines = function(line_df, journ, opts) {
  restore.point("line_df_find_junk_line")
  if (startsWith(line_df$txt[1],"American Economic")) {
    line_df$type[1] = "junk"
  }

  # Try to detect title page footnotes
  plines = which(line_df$page==1)
  lines = which(stri_detect_regex(line_df$txt[plines],"^[  ]+[\\*]"))
  if (length(lines)>0) {
    line = max(lines)
    if (line_df$rev_pline[line]<25) {
      lines = plines[plines >= line]
      line_df$type[lines] = "junk"
    }
  }

  lines = which(line_df$pline==1)

  citeby_line = lines[line_df$trim_txt[lines] == "This article has been cited by:"]
  if (length(citeby_line)>=1) {
    citeby_line = first(citeby_line)
    line_df$type[from_to(citeby_line, NROW(line_df))] = "junk"
  }
  line_df
}

line_df_find_section_cands = function(line_df, journ, opts) {
  line_df_find_sections(line_df, journ,opts=opts, just_cand=TRUE, verbose=FALSE)
}

line_df_find_sections = function(line_df, journ,opts, just_cand = FALSE, verbose=TRUE) {
  restore.point("line_df_find_sections")

  if (just_cand) {
    line_df$sec_cand_title = FALSE
  }

  if (startsWith(journ,"aej") | journ %in% c("aer","aeri","jep")) {
    # Sections of these journals start with roman numbers followed by a .
    cand_roman = stri_extract_first_regex(line_df$trim_txt,"^[IVXivx]+(?=([ ]?\\.[ \t]))")
    rows = which(!is.na(cand_roman))
    cand_lines = rows
    cand_num = suppressWarnings(as.integer(as.roman(cand_roman[cand_lines])))

    if (just_cand) {
      line_df$sec_cand_title[cand_lines] = TRUE
    } else {
      if (!all(cand_num == seq_along(cand_num))) {
        line_df$trim_txt[cand_lines]
        repbox_problem("Could not identify unique section starts. Need to augment section detection code.","art_identify_sections", ,opts$detection_problem, project_dir = opts$project_dir)
        can_num = cand_lines = NULL
      }
      line_df$type[cand_lines] = "sec1"
      line_df$sec_num[cand_lines] = cand_num
    }


    if (length(cand_lines) > 0) {
      max_cand_lines = max(cand_lines)
      max_cand_num = max(cand_num)
    } else {
      max_cand_lines = 0
      max_cand_num = 0
    }

    ref_section = which(line_df$trim_txt %in% c("References","REFERENCES") & line_df$line > max_cand_lines)
    if (length(ref_section)>=1) {
      ref_section = first(ref_section)
      if (just_cand) {
        line_df$sec_cand_title[ref_section] = TRUE
      } else {
        line_df$type[ref_section] = "bib"
        line_df$sec_num[ref_section] = max_cand_num+1
      }
    }

    if (verbose) {
      sec_lines = c(cand_lines, first(ref_section))
      cat(paste0("\n    Found ", length(sec_lines), " sections:", paste0("\n      ",line_df$trim_txt[sec_lines], collapse="")))
      if (length(ref_section)==0) {
        cat("  \nWarning: No References section found.")
      }
    }


  } else {
    repbox_problem(paste0("Find sections not yet implemented for journal ", journ),"art_journ_not_impl_find_sections", opts$not_implemented_action, project_dir = opts$project_dir)
  }

  if (just_cand){
    return(line_df)
  }

  # Assign section number to all lines
  line_df$sec_num = cummax(line_df$sec_num)

  line_df
}


line_df_find_figures = function(line_df, journ, opts=opts) {
  restore.point("line_df_find_figures")

  str = stri_extract_first_regex(line_df$trim_txt, "^Figure [A-Z]?[-\\.]?[1-9][0-9]?")
  rows = which(!is.na(str))
  figname = str[rows]
  figid = str.right.of(figname, "Figure ") %>% trimws()
  fig_df = tibble(figid =figid,figname = figname, figtitle = str[rows], title_line = rows, title_line_end = rows, trim_txt = line_df$trim_txt[rows], space_left = line_df$space_left[rows], page = line_df$page[rows])

  # We may have multiple fits for the same figure
  # Use a heuristic. Better are fits that have form "Figure 1." or "Figure 1:"
  # and are more centered.

  fig_df = fig_df %>%
    mutate(
      next_char = stri_sub(trim_txt,nchar(figname)+1,length = 1),
      points = (next_char == ".") * 10000L + (next_char == ":") * 1000L + pmin(space_left, 80-space_left)
    ) %>%
    group_by(figname) %>%
    arrange(desc(points)) %>%
    slice(1) %>%
    ungroup()


  # Key idea to identify lines belonging to a figure:
  # They are above or below the figure title and most lines
  # contain only few words but rather more numbers.

  #stri_extract_all_regex(line_df$trim_txt[[1]], "((^)|[ \\(])[A-Za-z]+[-]?[A-Za-z]*")
  #stri_extract_all_regex(line_df$trim_txt[[1]], "((^)|[ \\(])[^ ]*")

  line_df$word_count = stri_count_regex(line_df$trim_txt, "((^)|[ \\(])[A-Za-z]+[-]?[A-Za-z]*")
  line_df$expr_count = stri_count_regex(line_df$trim_txt, "((^)|[ \\(])[^ ]*")

  line_df$type[fig_df$title_line] = "figtitle"

  pages = unique(fig_df$page)
  fig_df$fignotes = ""

  for (i in seq_along(pages)) {
    page = pages[i]
    res = identify_figure_lines_on_page(page, line_df, fig_df, opts=opts)
    line_df = res$line_df; fig_df = res$fig_df
  }

  if (NROW(fig_df)>0) {
    # Check whether lines below figure title should be added to title
    lines_below = fig_df$title_line+1
    add_below = line_df$type[lines_below] %in% c("","fig") & line_df$word_count[lines_below] > 0 & line_df$page[lines_below] == line_df$page[fig_df$title_line]

    fig_df$title_line_end[add_below] = fig_df$title_line[add_below]+1
    fig_df$figtitle[add_below] = sapply(which(add_below), function(i) {
      combine_text_lines(line_df$trim_txt[fig_df$title_line[i]:fig_df$title_line_end[i]])
    })
    line_df$type[lines_below[add_below]] = "figtitle_below"
  }

  cols = c("figid",  "figname", "figtitle","title_line", "title_line_end",    "page","fignotes")
  fig_df = fig_df[,cols]

  return(list(line_df=line_df, fig_df=fig_df))
}


identify_figure_lines_on_page = function(page, line_df, fig_df, opts) {
  restore.point("identify_figure_lines_on_page")
  #if (page==9) stop()
  flines = which(fig_df$page==page)
  lines = which(line_df$page == page)

  title_inds = which(line_df$type[lines]=="figtitle")

  is_text = line_df$word_count[lines] >= 8
  text_neighbour = is.true(lead(is_text) | lag(is_text))

  is_cand = (!is_text | !text_neighbour) & line_df$type[lines] == ""

  areas = rle_table(is_cand) %>%
    # Assume a figure stretches over at least 7 lines
    filter(value==TRUE, length > 7) %>%
    select(-value) %>%
    mutate(type="area") %>%
    bind_rows(tibble(start=title_inds,end=start,length=1, type="title")) %>%
    arrange(start) %>%
    mutate(
      above_title = is.true(lead(type=="title") & type =="area"),
      below_title = is.true(lag(type=="title") & type =="area")
    ) %>%
    filter(above_title | below_title)



  i = 2
  for (i in seq_rows(areas)) {
    arows = from_to(areas$start[i],areas$end[i])
    alines = lines[arows]

    # Check if some lines below Figure are actually
    # likely section headers. Then they will not
    # be considered part of the figure
    if (areas$below_title[i]) {
      title_cand_row = which(line_df$sec_cand_title[alines])
      if (length(title_cand_row)>=1) {
        title_cand_row = title_cand_row[1]
        areas$end[i] = areas$start[i] + title_cand_row - 2
        if (areas$end[i]<areas$start[i]) next
        arows = from_to(areas$start[i],areas$end[i])
        alines = lines[arows]
      }
    }

    line_df$type[alines] = "fig"

    fig_ind = flines[min(i, length(flines))]
    # notes_se = ExtractSciTab::extract_tab_note_lines(line_df$txt, line_df$trim_txt, max(alines),rev_pline = line_df$rev_pline[max(alines)])
    #
    # if (!is.null(notes_se)) {
    #   notes_lines = from_to(notes_se[1], notes_se[2])
    #   line_df$type[notes_lines] = "fignote"
    #   fig_df$fignotes[fig_ind] = combine_text_lines(line_df$trim_txt[notes_lines])
    # }
  }

  return(list(line_df=line_df, fig_df=fig_df))

}


line_df_find_footnotes = function(line_df, journ, opts) {
  restore.point("line_df_find_footnotes")
  #fail_fun = make_fail_fun(opts$not_implemented_action)
  if (startsWith(journ,"aej") | journ %in% c("aer","aeri","jep")) {
    su5 = substring(line_df$trim_txt,1,5)
    int = suppressWarnings(as.numeric((su5)))
    rows = is.true(trimws(su5) != as.character(round(int))) | int == 0
    #rows = is.true(line_df$trim_txt != as.character(round(int))) | int == 0
    int[rows] = NA_real_
    rows = line_df$space_left > 16 | int > sum(!is.na(int))+5
    int[rows] = NA_real_
    rows = which(!is.na(int))
    int = int[rows]

    # A proper footnote start only consists of the integer specifying the footnote
    proper =  is.true(line_df$trim_txt[rows] == as.character(round(int)))


    fn_df = tibble(footind = int, start_line=rows, proper=proper, space_left = line_df$space_left[rows])

    up_df = filter(fn_df, !proper)
    fn_df = filter(fn_df, proper)

    # Check if some footnote is missing among proper
    max_int = last(fn_df$footind[fn_df$proper])
    missing_int =setdiff(seq_len(max_int), fn_df$footind)
    if (length(missing_int)>0) {
      # Check whether some inproper footnotes correctly
      # fill the gaps
      cand_df = filter(up_df, footind %in% missing_int)
      df = bind_rows(fn_df, cand_df) %>%
        arrange(start_line)
      n = NROW(df)
      while(TRUE) {
        df = df %>% filter(! (!proper & is.true(footind >= lead(footind))))
        if (NROW(df)==n) break
        n = NROW(df)
      }
      fn_df = df
    }
  } else {
    repbox_problem(paste0("Footnote detection not yet implemented for journal ", journ),"art_journ_not_impl_find_footnote", opts$not_implemented_action)
    return(list(line_df=line_df, fn_df=NULL))
  }
  if (NROW(fn_df)==0) return(list(line_df=line_df, fn_df=NULL))

  end_lines_cand = sort(c(fn_df$start_line-1, which(line_df$rev_pline==1), NROW(line_df)))
  end_ind = findInterval(fn_df$start_line, end_lines_cand)+1
  fn_df$end_line = end_lines_cand[end_ind]

  fn_df = check_and_repair_footnote_candidates(fn_df)

  if (NROW(fn_df)==0) return(list(line_df=line_df, fn_df = fn_df))

  fn_df$txt = ""
  # Change line_df according to fn_df
  for (i in seq_rows(fn_df)) {
    lines = from_to(fn_df$start_line[i], fn_df$end_line[i])
    line_df$type[lines] = "note"
    line_df$footind[lines] = fn_df$footind[i]
    fn_df$txt[i] = combine_text_lines(line_df$txt[lines])
  }

  return(list(line_df=line_df, fn_df=fn_df))
}

check_and_repair_footnote_candidates = function(fn_df) {
  restore.point("check_and_repair_footnote_candidates")
  # Something is fishy
  if (all(is.true(fn_df$footind==seq_along(last(fn_df$footind))))) {
    return(fn_df)
  }

  # There is a foot note in wrong order
  # not only gaps
  fn_df$larger_than_next = is.true(fn_df$footind >= lead(fn_df$footind))
  fn_df$smaller_than_prev = is.true(fn_df$footind <= lag(fn_df$footind))

  if (any(fn_df$larger_than_next)) {
    restore.point("fishy_footnote")

    # First guess fishy footnote has problems on both sides
    fishy = which(fn_df$larger_than_next & fn_df$smaller_than_prev)

    # 2n guess
    if (length(fishy)==0) {
      fishy = which(
        # jumps more than 1 and next is smaller again
        is.true(
          (fn_df$footind > lag(fn_df$footind)+2) &
          (lead(fn_df$footind) < fn_df$footind)
        ) |
        # is smaller than previous
        is.true(fn_df$footind < lag(fn_df$footind))
      )
    }

    # 3rd guess
    if (length(fishy)==0) {
      fishy = which(
        # jumps more than 1
        is.true(fn_df$footind > lag(fn_df$footind)+2)
      )
    }
    # 4th guess
    if (length(fishy)==0) {
      fishy = which(
        # jumps 1
        is.true(fn_df$footind > lag(fn_df$footind)+1)
      )
    }

    if (length(fishy)==0 & any(duplicated(fn_df$footind))) {
      fn_df = fn_df %>%
        mutate(num_lines = end_line-start_line+1) %>%
        group_by(footind) %>%
        arrange(num_lines) %>%
        mutate(remove = 1:n()>1) %>%
        ungroup()
      fishy = which(fn_df$remove)
    }

    if (length(fishy)==0) {
      stop("Fishy footnotes ordering that could not yet be repaired.")
    }
    fn_df = fn_df[-first(fishy),,drop=FALSE]
    return(check_and_repair_footnote_candidates(fn_df))
  }
  return(fn_df)
}

combine_text_lines = function(txt, remove.line.end.hyphen = TRUE, replace.line.breaks=TRUE, remove_repeated_ws = TRUE) {
  restore.point("combine_text_lines")
  txt = merge.lines(txt)
  if (remove.line.end.hyphen) {
    if (replace.line.breaks) {
      txt = stri_replace_all_fixed(txt, "-\n","")
    } else {
      txt = stri_replace_all_fixed(txt, "-\n","\n")
    }
  }
  if (replace.line.breaks) {
    # Replace line breaks that are not followed by a space or another line break
    # This means we only want to keep line breaks for likely paragraph breaks.
    txt = stri_replace_all_regex(txt, "\n(?![ \n])"," ")

    # Also replace line breaks if last letter in previous line was
    # a simple lower case character. Then we don't have a finished sentence
    # and no new paragraph should start
    txt = stri_replace_all_regex(txt, "(?<=[a-z])\n"," ")
  }
  if (remove_repeated_ws) {
    txt = stri_replace_all_regex(txt,"[ ]+"," ")
  }

  txt
}



is_really_a_note_line = function(txt, trim_txt, opts) {
  restore.point("is_really_a_note_line")

  if (stri_detect_regex(trim_txt,"^Note[s]?[\\:\\.]")) return(TRUE)

  words = stringi::stri_extract_all_words(trim_txt, omit_no_match=TRUE)[[1]]
  if(length(words)<=1) return(TRUE)
  # That should be sufficients since no normal
  # sentence should start with the word "Notes"
  if (words[1]=="Notes") return(TRUE)
  # If the second word starts with a capital letter
  # we likely have a TRUE note
  if (words[2]!=tolower(words[2])) return(TRUE)

  # The expression "Note that" suggests a normal sentence
  if (words[2]=="that") return(FALSE)

  if (startsWith(txt,"      ")) return(TRUE)

  repbox_problem("\n\nThe text below the table note does not satisfy any of our (crude) checking criteria for a table note.","art_table_note", opts$detection_problem)
  #stop()
  return(FALSE)
}

line_df_find_page_header_footer = function(line_df, journ, opts) {
  restore.point("line_df_find_page_header_footer")
  #fail_fun = make_fail_fun(opts$not_implemented_action)
  wtxt = line_df$trim_txt

  start_lines = which(line_df$pline == 1)
  type = line_df$type

  if (startsWith(journ,"aej") | journ %in% c("aer","aer_pandp") ) {
    lines = start_lines
    str = wtxt[lines]
    if (journ == "aer") {
      title_str = " The American Economic Review  "
    } else if (journ == "aer_pandp") {
      title_str = "AEA Papers and Proceedings"
    } else {
      title_str = "  American Economic "
    }
    clear = has.substr(str, title_str) |
      has.substr(str, toupper(title_str)) |
      (startsWith(str,"Vol.") & has.substr(str, " No")) |
      (startsWith(str,"VOL.") & has.substr(str, " NO"))
    wtxt[lines[clear]] = ""
  } else {
    repbox_problem(paste0("Find page header / footer not yet implemented for journal ", journ),"art_journ_not_impl_find_page_header", opts$not_implemented_action)
    return(line_df)
  }

  wtxt[start_lines]
  line = first(start_lines)
  for (sline in start_lines) {
    line = sline
    while(wtxt[line]=="") {
      type[line] = "header"
      line = line+1
    }
  }

  end_lines = c(start_lines[-1]+1,NROW(line_df))

  str = wtxt[end_lines]
  clear = str == "\f"
  wtxt[end_lines[clear]] == ""

  for (eline in end_lines) {
    line = eline
    while(wtxt[line]=="") {
      type[line] = "footer"
      line = line-1
    }
  }
  line_df$type = type
  line_df$keep = line_df$keep & (!line_df$type %in% c("header","footer"))
  line_df
}



