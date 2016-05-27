##' @include GWidget.R
##' @include dialogs.R
##' @include gtable.R
##' @include tablelist.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gdf
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gdf guiWidgetsToolkittcltk
.gdf.guiWidgetsToolkittcltk <-  function(toolkit,
                                         items = NULL,
                    handler = NULL,action = NULL, container = NULL, ... ) {
  GDf$new(toolkit,
           items=items, 
           handler = handler, action = action, container = container, ...)
}


##' Class to provide means to edit data frames
##'
##' The \code{GDf} class provides a means to edit a data frame. We
##' use the add on TK code provided by tablelist as the underlying
##' widget
##'
##' Simply click on a row and the editor pops up as a modal
##' dialog. The shortcut Shift+Enter will go onto the next case,
##' saving changes provided the auto save featuer is employed.
##'
##' There is no undo/redo support here. There is no support for
##' editing rownames (just lazy at the moment, holler if you would
##' like that). No support to change the dimensions of the data frame
##' or edit factors, ...
##' @rdname gWidgets2tcltk-package
GDf <- setRefClass("GDf",
                   contains="GWidget",
                    fields=list(
                      head="ANY"
                      ),
                    methods=list(
                      initialize=function(toolkit,
                        items,
                        name=deparse(substitute(df)),
                        handler=NULL, action=NULL,
                        container=NULL,
                        ...) {

                        ## what is 
                        initFields(change_signal="<<TablelistCellUpdated>>", coerce_with=NULL)
                        init_widget(container$widget)
                        items <- as.data.frame(items)
                        tl_configure_columns(widget, names(items))

                        ## populate
                        set_items(value=items)
                        head <<- head(items, n=1) # store types

                        add_to_parent(container, .self, ...)

                        ## change handler is row updated
#                        handler_id <<- add_handler_changed(handler, action)
                        
                        callSuper(toolkit)
                      },
                      init_widget=function(parent) {
                        block <<- ttkframe(parent)
                        xscr <- ttkscrollbar(block, orient="horizontal",
                                             command=function(...) tkxview(widget,...))
                        yscr <- ttkscrollbar(block, orient="vertical",
                                             command=function(...) tkyview(widget,...))
                        

                        widget <<- tkwidget(block, "tablelist::tablelist",
                                            resizablecolumns=1,
                                            xscrollcommand=function(...) tkset(xscr,...),
                                            yscrollcommand=function(...) tkset(yscr,...))

                        tcl(widget, "configure", selecttype="cell")

                        
                        tkgrid(widget, row=0, column=0, sticky="news")
                        tkgrid(yscr, row=0, column=1, sticky="ns")
                        tkgrid(xscr, row=1, column=0, sticky="ew")
                        tkgrid.columnconfigure(block, 0, weight=1)
                        tkgrid.rowconfigure(block, 0, weight=1)
                        
                        tcl("autoscroll::autoscroll", xscr)
                        tcl("autoscroll::autoscroll", yscr)

                        tkgrid.propagate(block, FALSE)                        
                      },
                      set_items=function(value, i,j,...) {
                        if(!missing(i) || !missing(j)) {
                          tmp <- get_items()
                          tmp[i,j] <- value
                          value <- tmp
                        }
                        tl_load_data(widget, value)
                      },
                      save_data=function(nm, where) {
                        "Save data set"
                        assign(make.names(nm), get_items(), where)
                      },
                      get_items=function(i, j, ...) {
                        opar <- options("warn"); on.exit(options(opar))
                        options(list(warn=-1)) # quiet for coerce_raw
                        d <- get_dim()
                        l <- lapply(seq_len(d[2]), function(j) {
                          coerce_raw(head[[j]], tl_get_column_raw(widget, j))
                        })

                        m <- structure(l,
                                       .Names=tl_get_column_names(widget),
                                       row.names=seq_len(d[1]),
                                       class="data.frame")

                        m[i,j, ...]
                      },
                      get_names=function() tl_get_column_names(widget),
                      set_names=function(values, ...) tl_set_column_names(widget,values),
                      get_dim=function() c(tl_no_rows(widget), tl_no_cols(widget)),
                      get_length=function() get_dim()[2],
                      ## some extras
                      hide_row=function(i, hide=TRUE) tl_hide_row(widget, i, hide),
                      hide_column=function(j, hide=TRUE) tl_hide_column(widget, j, hide),
                      set_editable=function(j, value=TRUE) tl_set_column_editable(widget, j, value),
                      focus_cell=function(i,j) tl_set_focus_on_cel(widget, i, j),
                      sort_bycolumn=function(j, decreasing=FALSE) {
                        tl_sort_bycolumn(widget, j, decreasing)
                      }
                      ))

