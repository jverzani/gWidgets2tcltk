##' @include GContainer.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gnotebook
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gnotebook guiWidgetsToolkittcltk
.gnotebook.guiWidgetsToolkittcltk <-  function(toolkit,
                                               tab.pos = 3, 
                                               container = NULL, ... ) {
  GNotebook$new(toolkit, tab.pos, 
                    container = container, ...)
}



GNotebook <- setRefClass("GNotebook",
                            contains="GContainer",
                            methods=list(
                              initialize=function(toolkit=NULL, tab.pos=3, 
                                 container=NULL, ...) {


                                widget <<- ttknotebook(container$get_widget())

                                ## tab placement: 1,2,3,4 -> 3,0,2,1
                                if(tab.pos !=3)
                                 message(gettext("tab.pos is not implemented\n"))
                                
                                
                                initFields(block=widget)

                                
                                add_to_parent(container, .self, ...)

                                callSuper(toolkit)
                              },
                              add_child=function(child, label="", index,  ...) {
                                "Add child. Can pass index value in case we want to replace"
                                do_insert <- !(is.null(index) || missing(index) || index > get_length() || index < 1)

                                child$set_parent(.self)
                                if(do_insert) {
                                  children[[index]] <<- child
                                  tcl(widget, "forget", index - 1)
                                  tcl(widget, "insert", index -1, child$get_block())
                                } else {
                                  ## aooebd
                                  index <- get_length() + 1
                                  children <<- c(children, child)
                                  tcl(widget, "add", child$get_block())
                                }
                                set_names(label, index)
                                set_index(index)
                              },
                              get_value=function( ...) {
                                get_index()
                              },
                              get_index = function(...) {
                                as.numeric(tcl(widget, "index", "current")) + 1
                              },
                              set_value=function(value, ...) {
                                old_value <- get_index()
                                value <- max(1,min(value,get_length()))
                                
                                tcl(widget,"select",value - 1) # 0 -based
                                if(value != old_value)
                                  invoke_handler("<<NotebookTabChanged>>")
                              },
                              get_names = function(...) {
                                sapply(seq_len(get_length()), function(i) {
                                  tclvalue(tcl(widget, "tab", i-1, "-text"))
                                })
                              },
                              set_names = function(value, i, ...) {
                                f <- function(i,x) tcl(widget, "tab", i-1, text=x)
                                if(missing(i))
                                  i <- seq_len(get_length())
                                mapply(f, i=i, x=value)
                                invisible()
                              },
                              get_items = function(i, j, ..., drop=TRUE) {
                                "Return child at ith spot"
                                items <- children[i]
                                if(drop && length(items) == 1)
                                  items[[1]]
                                else
                                  items
                              },
                              set_items = function(value, i) {
                                "XXX not implemented"

                              },
                              get_length = function(...) {
                                "Nmber of pages"
                                as.numeric(tclvalue(tcl(widget,"index","end")))
                              },
                              remove_child = function(child) {
                                ## remove from children
                                ind <- which(sapply(children, identical, y=child))
                                if(length(ind)) {
                                  remove_page_by_index(ind)
                                }
                              },
                              remove_page_by_index=function(ind) {
                                  children[[ind]] <<- NULL
                                  tcl(widget, "forget", ind-1)
                              },
                              ## this one called by dispose.GNotebook
                              remove_current_page = function() {
                                remove_page_by_index(get_index())
                              },
                              change_page_decorator=function(f) {
                                FUN <- function(W) {
                                  ind <- as.integer(tcl(W, "index", "current")) + 1
                                  f(extra_args=list(page.no=ind))
                                }
                              },
                              add_handler_changed=function(handler, action=NULL, ...) {
                                "A tab changed"
                                decorator <- function(FUN) {
                                  force(FUN)
                                  f <- function(W) {
                                    ind <- as.integer(tcl(W, "index", "current")) + 1L
                                    FUN(.self, page.no=ind)
                                  }
                                  f
                                }
                                add_handler("<<NotebookTabChanged>>", handler, action, decorator=decorator)
                              }
                              ))

