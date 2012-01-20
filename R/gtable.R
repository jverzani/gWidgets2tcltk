##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gtable
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gtable guiWidgetsToolkittcltk
##' @S3method .gtable guiWidgetsToolkittcltk
.gtable.guiWidgetsToolkittcltk <-  function(toolkit,
                                         items,
                                         multiple = FALSE,
                                         chosen.col = 1,
                                         icon.col = NULL,
                                         tooltip.col=NULL,
                                         handler = NULL, action = NULL,
                                         container = NULL, ... ) {
  GTable$new(toolkit,
           items=items,
           multiple=multiple,
           chosen.col=chosen.col,
           icon.col = icon.col,
           tooltip.col = tooltip.col,
           handler=handler,
             action=action,
           container=container ,...)
}



format_to_char <- function(x) UseMethod("format_to_char")
format_to_char.default <- function(x) as.character(x)
format_to_char.integer <- function(x) format(x, justify="right")
format_to_char.numeric <- function(x) format(x, trim=TRUE)
format_to_char.Date <- function(x) format(x, format="%d-%m-%Y")
format_to_char.data.frame <- function(x) sapply(x, format_to_char)

column_alignment <- function(x) UseMethod("column_alignment")
column_alignment.default <- function(x) "w"
column_alignment.numeric <- function(x) "e"
column_alignment.logical <- function(x) "c"



## Base class for the table widgets: gtable, gdf, gcheckboxgroup (with table)
BaseTableClass <- setRefClass("BaseTableClass",
                              contains="GWidget",
                              fields=list(
                                DF="ANY",   # data frame
                                n="numeric", # n cols
                                ..visible="logical",
                                child_ids="ANY"
                                ),
                              methods=list(
                                init_widget=function(parent, ...) {
                              
                                  block <<- ttkframe(parent)
                                  xscr <- ttkscrollbar(block, orient="horizontal",
                                                       command=function(...) tkxview(widget,...))
                                  yscr <- ttkscrollbar(block, orient="vertical",
                                                       command=function(...) tkyview(widget,...))
                                  
                                  ## child needs to configure columns, displaycolumns, show
                                  widget <<- ttktreeview(block,
                                                         show="headings",
                                                         selectmode = "browse",
                                                         xscrollcommand=function(...) tkset(xscr,...),
                                                         yscrollcommand=function(...) tkset(yscr,...)
                                                         )
                                  
                                  tkgrid(widget, row=0, column=0, sticky="news")
                                  tkgrid(yscr, row=0, column=1, sticky="ns")
                                  tkgrid(xscr, row=1, column=0, sticky="ew")
                                  tkgrid.columnconfigure(block, 0, weight=1)
                                  tkgrid.rowconfigure(block, 0, weight=1)

                                  tcl("autoscroll::autoscroll", xscr)
                                  tcl("autoscroll::autoscroll", yscr)

                                },
                                ## DF is  adata frame
                                set_DF=function(items) {
                                  "Set data and populate the view"
                                  ## DF is just items by default
                                  DF <<- as.data.frame(items)
                                  ..visible <<- rep(TRUE, nrow(DF))
                                  populate_view()
                                },
                                ## headings
                                set_column_headings=function(nms) {
                                  "Set column headings"
                                  if(length(nms) != n) {
                                    message(gettext("Wrong length for column headings"))
                                    return() # wrong size
                                  }
                                  f <- function(col, value) tcl(widget, "heading", col, text=value)
                                  mapply(f, seq_along(nms), nms)
                                },
                                set_column_widths=function(widths, data) {
                                  "Set widths from widths, or from data frame passed in via data"
                                  if(!missing(data)) {
                                    m <- format_to_char(data)
                                    chars <- apply(m, 2, function(x) max(nchar(x)))
                                    widths <- ceiling(1.4 * widthOfChar * pmax(4, chars))
                                  }
                                  if(length(widths) != n) {
                                    message(sprintf("Widths are not the correct length. Expecting %s, got %s", n, length(widths)))
                                    return()
                                  }
                                  f <- function(col, width) tcl(widget, "column", col, width=width, stretch=FALSE)
                                  mapply(f, seq_along(widths), widths)

                                  tcl(widget, "column", ncol(m), stretch=TRUE)
                                },
                                set_column_alignment=function(aligns) {
                                  if(missing(aligns)) 
                                    aligns <- sapply(get_data(), column_alignment)

                                  if(length(aligns) != n) {
                                    message(sprintf("Wrong length. Expecting %s, got %s.", n, length(aligns)))
                                    return()
                                  }
                                  f <- function(col, value) tcl(widget, "column", col, anchor=value)
                                  mapply(f, seq_along(aligns), aligns)
                                },
                                ## icon column
                                configure_icon_column=function(width=20L) {
                                  "Put in configuration for icons"
                                  tkconfigure(widget,
                                              displaycolumns="#all",
                                              show="tree headings"
                                              )
                                  set_icon_width(width)
                                },
                                set_icon_width=function(width=20L) {
                                  "Set width for icon column"
                                  tcl(widget, "column", "#0", width=width, anchor="w", stretch=FALSE)
                                },
                                set_icons=function(icons) {
                                  "Set column of icons"
                                  if(is.null(icons))
                                    return()
                                  if(length(icons) != nrow(DF)) {
                                    message(sprintf("Too few icons specified. Expected %s, got %s", nrow(DF), length(icons)))
                                    return()
                                  }
                                  ## check class
                                  if(!is(icons, "StockIcon"))
                                    icons <- sapply(icons, getStockIconByName)
                                  ## configure
                                  f <- function(id, value) tcl(widget, "item", id, image=value)
                                  mapply(f, child_ids, icons)
                                },
                                set_tooltips=function(tips) {
                                  if(is.null(tips))
                                    return()
                                  return()
                                  ## This *should* work, but really fails
                                  tkbind(widget, "<Motion>", function(W, x, y) {
                                    row <- as.character(tcl(W, "identify", "row", x, y))
                                    ind <- match(row, child_ids)
                                    if(length(ind)) 
                                      tcltk2:::tk2tip(W, tips[ind])
                                    else
                                      tcltk2:::tk2tip(W, "")
                                  })
                                },
                                ## rows
                                append_row=function(values) {
                                  ## values a list or vector
                                  values <- sapply(values, format_to_char)
                                  id <- tcl(widget, "insert", "", "end", values=values)
                                  as.character(id)
                                },
                                replace_row_data=function(i, values) {
                                  "Replace row data. @param i row index, @param values a vector or list of values"
                                  DF[i, ] <<- values
                                  values <- sapply(values, format_to_char)
                                  tcl(widget, "item", child_ids[i], values=values)
                                },
                                insert_row=function(i, values) {},
                                remove_row=function(i) {
                                  "Remove row, @param i is row index"
                                  DF <<- DF[-i, ]
                                  id <- child_ids[i]; child_ids <<- child_ids[-i]
                                  ..visible <<- ..visible[-i]
                                  tcl(widget, "delete", id)
                                },
                                clear_view=function() {
                                  "clear out widget and reset ..visible and child_ids"
                                  tcl(widget, "delete", tcl(widget, "children", "")) # clear widget
                                  ..visible <<- rep(TRUE, nrow(DF))
                                  child_ids <<- list()
                                },
                                get_col = function(i) {
                                  "Helper: Get column if not NULL"
                                  if(!is.null(i)) DF[,i] else NULL
                                },
                                get_data = function() {
                                  "Helper: Return DF less and special columns. Meant to help in subclass"
                                  if(length(get_hidden_columns()))
                                    DF[,-get_hidden_columns()]
                                  else
                                    DF
                                },
                                get_hidden_columns = function() {
                                  "Return columns not to show"
                                  integer(0)
                                },
                                populate_view=function() {
                                  "Populate widget, set column widths and alignment"
                                  tclServiceMode(FALSE)
                                  on.exit(tclServiceMode(TRUE))

                                  clear_view()
                                  
                                  m <- get_data()
                                  child_ids <<- sapply(seq_len(nrow(m)), function(i) append_row(m[i,]))
                                  set_column_widths(data=m)
                                  set_column_alignment()

                                },
                                ## hide/show rows
                                set_visible=function(values) {
                                  "Set visible rows. @param values is recycled"
                                  values <- rep(values, length.out=nrow(DF))
                                  sapply(seq_along(..visible), function(i) {
                                    ## detach or replace, depending if a change
                                    if(..visible[i] && !values[i]) {
                                      tcl(widget, "detach", child_ids[i])
                                    } else if(!..visible[i] && values[i]) {
                                      tcl(widget, "move",  child_ids[i], "", i-1)
                                    }
                                  })
                                  ..visible <<- values
                                },
                                get_visible=function(...) {
                                  "Logical vectors indicating which rows are visible"
                                  ..visible
                                },
                                ## selection
                                set_selectmode=function(type=c("extended", "browse", "none")) {
                                  "Change selection mode. Multiple select is 'extended'"
                                  tkconfigure(widget, selectmode=type)
                                },
                                get_selection = function(...) {
                                  "return selected index (indices)"
                                  ids <- as.character(tcl(widget, "selection"))
                                  match(ids, child_ids)
                                },
                                set_selection = function(ind=integer(0), ...) {
                                  "set selected indices."
                                  selectmode <- as.character(tkcget(widget, "-selectmode"))
                                  if(length(ind) == 0 || selectmode == "none") {
                                    tcl(widget, "selection", "set", "") # clear
                                  } else {
                                    if(selectmode == "browse") ind <- ind[1] # only 1
                                    tcl(widget, "selection", "set", paste(child_ids[ind], collapse=" "))
                                  }
                                },
                                scroll_to=function(i) {
                                  id <- child_ids[i]
                                  tcl(widget, "see", id)
                                },
                                ## GWidgets methods
                                get_index = function(...) {
                                  "Get index of selected rows or integer(0)"
                                  get_selection()
                                },
                                set_index = function(value,...) {
                                  "set selected values in value. integer(0) or 0L clears selection"
                                  set_selection(as.integer(value))
                                },
                                get_items = function(i, j, ..., drop=TRUE) {
                                  m <- get_data()
                                  ## we possibly drop out some stuff
                                  m[i,j, drop=getWithDefault(drop, TRUE)]
                                },
                                set_items = function(value, i, j, ...) {
                                  if(missing(i) && missing(j)) {
                                    ## replace data frame
                                    value <- as.data.frame(value)
                                    if(ncol(DF) != ncol(value)) {
                                      message(sprintf("Trying to replace data with different number of columns. Expected %s, got %s.", ncol(DF), ncol(value)))
                                      return()
                                    }
                                    set_DF(value)
                                  } else if(missing(j)) {
                                    if(length(i) == 1) {
                                      replace_row_data(i, value)
                                    } else {
                                      sapply(seq_along(i), function(i) replace_row_data(i, value[i,]))
                                    }
                                  } else if(missing(i)) {
                                    sapply(seq_len(nrow(DF)), function(i) {
                                      vals <- DF[i,]
                                      vals[j] <- value[i,] # replace
                                      replace_row_data(i, vals)
                                    })
                                  } else {
                                    sapply(seq_along(i), function(ii) {
                                      vals <- DF[i[ii], ]
                                      vals[j] <- value[i[ii], ]
                                      replace_row_data(i[ii], vals)
                                    })
                                  }
                                },
                                ## data store methods
                                get_length=function() {
                                  get_dim()[2]
                                },
                                get_dim=function() {
                                  "Return dim of view (not data frame which may have extra information)"
                                  dim(get_data())
                                },
                                get_names=function() {
                                  names(get_data())
                                },
                                set_names =function(value) {
                                  ## check length
                                  m <- get_dim()[2]
                                  if(length(value) != m)
                                    return()
                                  ## set names of DF
                                  ind <- get_hidden_columns()
                                  if(length(ind))
                                    names(DF)[-ind] <<- value
                                  else
                                    names(DF) <<- value
                                  ## widget names
                                  set_column_headings(value)
                                },
                                set_size=function(value, ...) {
                                  "set size also has possibility of column widths"
                                  if(is.list(value)) {
                                    col_widths <- value$column.widths
                                    value$column.widths <- NULL
                                    set_column_widths(col_widths)
                                    value <- c(width=value$width, height=value$height) # make vector, not list
                                  }
                                  callSuper(value, ...)
                                },
                                add_handler_column_clicked=function(handler, action=NULL) {
                                  "Column clicked passed back column index in column component"
                                  ## have to do this the hard way
                                  signal <- "ColumnClicked"
                                  
                                  if(is.null(connected_signals[[signal, exact=TRUE]])) {
                                    ## apply to each column the command
                                    f <- function(col) {
                                      tcl(widget, "heading", col, command=function() {
                                        .self$notify_observers(signal=signal, extra_args=list(column=col))
                                      })
                                    }
                                    sapply(seq_len(get_length()), f)
                                    connected_signals[[signal]] <<- TRUE
                                  }
                                  add_handler(signal, handler, action)
                                }

                                ))


GTable <- setRefClass("GTable",
                      contains="BaseTableClass",
                      fields=list(
                        ..visible="logical",
                        multiple="logical",
                        chosen_col="ANY",
                        icon_col="ANY",
                        tooltip_col="ANY",
                        tips="ANY"
                        ),
                      methods=list(
                        initialize=function(toolkit=NULL,
                                items="data.frame",
                                multiple = FALSE,
                                chosen.col = 1,
                                icon.col = NULL,
                                tooltip.col=NULL,
                                handler = NULL, action = NULL,
                                container = NULL, ...) {

                              initFields(chosen_col=chosen.col,
                                         icon_col=icon.col,
                                         tooltip_col=tooltip.col,
                                         multiple=multiple,
                                         change_signal="<<TreeviewSelect>>"
                                         )

                              init_widget(container$get_widget(), ...)

                              items <- as.data.frame(items)
                              n <<- ncol(items) - !is.null(icon_col) - !is.null(tooltip_col)
                              tkconfigure(widget, columns=1:n)
                              ## icons?
                              if(!is.null(icon.col))
                                configure_icon_column()
                              
                              selectmode <- ifelse(multiple, "extended", "browse")
                              set_selectmode(selectmode)

                              ## populate
                              set_DF(items)
                              set_column_headings(names(get_data()))

                              ## icons/tooltips
                              tooltips <- get_col(tooltip_col)
                              set_tooltips(tooltips)

                              icons <- get_col(icon_col)
                              set_icons(icons)
                              
                              add_to_parent(container, .self, ...)

                              handler_id <<- add_handler_changed(handler, action)
                              
                              callSuper(toolkit)

                              
                            },
                        get_hidden_columns=function() {
                          ## for get_data
                          ind <- c(icon_col, tooltip_col)
                          if(is.null(ind)) ind <- integer(0)
                          ind
                        },
                        get_value=function(drop=TRUE, ...) {
                          "Get selected values by value (or character(0))"
                          vals <- get_items(drop=FALSE)[get_selection(), , drop=FALSE]
                          if(getWithDefault(drop, TRUE))
                            vals[, chosen_col, drop=TRUE]
                          else
                            vals
                        },
                        set_value=function(value, ...) {
                          "Set selected values by vector matching chosen.col, unless an integer"
                          block_handlers()
                          vals <- get_value(drop=TRUE)
                          if(is.numeric(value) && !is.numeric(vals))
                            ind <- value
                          else
                            ind <- match(value, get_value(drop=TRUE))
                          if(length(ind) == 1 && is.na(ind))
                            return() ## no match
                                  set_index(ind)
                          unblock_handlers()                          
                        }
                        
                        ))
