## Code for interfacing with tablelist5.6 which is loaded in
## zzz.R


## Events are:  <<TablelistCellUpdated>> <<TablelistSelect>> 


## Configure tbl
tl_configure_columns <- function(tbl, nms) {
  .Tcl(sprintf("%s configure -columns {%s}",
               tbl$ID,
               paste(sprintf("0 {%s} left", nms), collapse="\n")
      ))
  sapply(seq_along(nms), function(j) tl_set_column_editable(tbl, j))
}

## Load Data
## helper to load a row
tl_insert_row <- function(tbl, row) {
  if(length(row) == 1 && grepl(" ", row))
    row <- paste("{", row, "}", sep="")
  tcl(tbl, "insert", "end", unlist(lapply(row, as.character)))
}

tl_clear_data <- function(tbl) {
  tcl(tbl, "delete", "0", "end")
}

tl_load_data <- function(tbl, items) {
  ## need to clear old first!
  tl_clear_data(tbl)
  sapply(seq_len(nrow(items)), function(i)
         tl_insert_row(tbl, items[i,,drop=TRUE]))
}

## return tcl cell index
tl_get_cellindex <- function(tbl, i, j) {
  tcl(tbl, "cellindex", sprintf("%s, %s", i-1, j-1))
}


## Get Data
## get cell infor -- raw = text
tl_get_cell_raw <- function(tbl, i, j) {
  raw <- tcl(tbl, "cellcget", tl_get_cellindex(tbl, i, j), "-text")
  tclvalue(raw)
}

## returns text value for column -- must coerce to ...
tl_get_column_raw1 <- function(tbl, j) {
  m <- tl_no_rows(tbl)
  sapply(seq_len(m), function(i) tl_get_cell_raw(tbl, i, j))
}

##helper
parse_tcl <- function(x) {

  ctr <- 0
  y <- strsplit(x, "")[[1]]
  tmp <- character(0)
  cur <- ""
  
  push_chr <- function(cur, i) {
    if(cur == "") i else paste(cur, i, sep="")
  }
  commit_cur <- function() {
    if(nchar(cur) > 0)
      tmp <<- c(tmp, cur)
    cur <<- ""
  }
  for(i in y) {
    if(i == "{") {
      if(ctr == 1) {
        commit_cur()
      }
      ctr <- ctr + 1
    } else if(i == "}") {
      if(ctr == 2) {
        commit_cur()
      }
      ctr <- ctr - 1
    } else if(i == " ") {
      if(ctr == 1) {
        commit_cur()
      } else {
        cur <- push_chr(cur, i)
      }
    } else {
      cur <- push_chr(cur, i)
    }
  }
  commit_cur()
  tmp
}

tl_get_column_raw <- function(tbl, j) {
  raw <- tcl(tbl, "getcolumns", j-1, j-1)
  parse_tcl(tclvalue(raw))
}


## return character matrix
tl_get_raw <- function(tbl) {
  do.call(cbind, lapply(seq_len(tl_no_cols(tbl)), function(j) tl_get_column_raw(tbl, j)))
}

## coerce
coerce_raw <- function(x, values) UseMethod("coerce_raw")
coerce_raw.default <- function(x, values) as.character(values)
coerce_raw.integer <- function(x, values) as.integer(values)
coerce_raw.numeric <- function(x, values) as.numeric(values)
coerce_raw.logical <- function(x, values) as.logical(values)
coerce_raw.factor <- function(x, values) factor(values)


## names
tl_set_column_name <- function(tbl, j, nm) {
  tcl(tbl, "columnconfigure", j-1, title=nm)
}

tl_set_column_names <- function(tbl, nms) {
  for(j in seq_along(nms)) tl_set_column_name(tbl, j, nms[j])
}


tl_get_column_name <- function(tbl, j) {
  tail(as.character(tcl(tbl, "columnconfigure", j-1, title=NULL)), n=1)
}

tl_get_column_names <- function(tbl) {
  sapply(seq_len(tl_no_cols(tbl)), function(j) tl_get_column_name(tbl, j))
}

## remove column
tl_remove_column <- function(tbl, j) {
  tcl(tbl, "deletecolumns", j-1, j-1)
}


## sort by column
tl_sort_bycolumn <- function(tbl, j, decreasing=FALSE) {
  dir <- if(decreasing) "decreasing" else "increasing"
  tcl(tbl, "sortbycolumn", j-1, sprintf("-%s", dir))
}


## size
tl_no_rows <- function(tbl) as.numeric(tcl(tbl, "childcount", "root"))
tl_no_cols <- function(tbl) as.numeric(tcl(tbl, "columncount"))


##
tl_set_focus_on_cell <- function(tbl, i, j) {
  tcl(tbl, "see", sprintf("%s, %s", i-1, j-1))
}


## show/hide column
tl_hide_row <- function(tbl, i, hide=TRUE) {
  hide <- if(hide) 1 else 0
  tcl(tbl, "rowconfigure", i-1, hide=hide)
}

tl_hide_column <- function(tbl, j, hide=TRUE) {
  hide <- if(hide) 1 else 0
  tcl(tbl, "columnconfigure", j-1, hide=hide)
}

## toggle editabbility of column
tl_set_column_editable <- function(tbl, j, editable=TRUE) {
  editable <- if(editable) "yes" else "no"
  tcl(tbl, "columnconfigure", j-1, editable=editable)
}


