##' @include GContainer.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gpanedgroup
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gpanedgroup guiWidgetsToolkittcltk
.gpanedgroup.guiWidgetsToolkittcltk <-  function(toolkit,
                                                horizontal = TRUE, 
                                                container = NULL, ... ) {
  GPanedGroup$new(toolkit,
           horizontal=horizontal, 
           container = container, ...)
}


## main class
GPanedGroup <- setRefClass("GPanedGroup",
                            contains="GContainer",
                           fields=list(
                             horizontal="logical"
                             ),
                            methods=list(
                              initialize=function(toolkit=NULL,
                                horizontal=TRUE,
                                container=NULL, ...) {

                                widget <<- ttkpanedwindow(container$get_widget(),
                                                          orient=ifelse(horizontal, "horizontal", "vertical"))
                                initFields(block=widget,
                                           horizontal=horizontal
                                           )
                                add_to_parent(container, .self, ...)
                                callSuper(toolkit)
                              },
                              get_value = function(...) {
                                "get sash position"
                                sashpos <- as.numeric(tclvalue(tcl(widget,"sashpos",0)))
                                theSize <- get_size()
                                if(horizontal)
                                  return(sashpos/theSize[1])
                                else
                                  return(sashpos/theSize[2])
                              }, 
                              set_value = function(value, ...) {
                                "Set sash position"
                                if(is.integer(value)) {
                                  pos = value
                                } else {
                                  if(0 <= value && value <= 1) {
                                    theSize <- get_size()
                                    if(horizontal)
                                      pos <- floor(value *  theSize[1])
                                    else
                                      pos <- floor(value *  theSize[2])
                                  } else {
                                    stop("Value must be in [0,1] or of class integer")
                                  }
                                }
                                
                                tcl(widget,"sashpos", 0, as.integer(pos))
                              },
                              get_items = function(i, j, ..., drop=TRUE) {
                                children[[i, drop=drop]]
                              },
                              get_length = function() {
                                length(children)
                              },
                              add_child=function(child, expand=NULL, fill=NULL, anchor=NULL) {
                                "Add one of two possible children"
                                n <- get_length()
                                if(n >= 2) {
                                  message("Already have two children. Remove one?")
                                  return()
                                }

                                tcl(widget, "insert", "end", child$get_block())
                                
                                child_bookkeeping(child)
                              },
                              remove_child=function(child) {
                                "remove child from paned container"
                                children <<- Filter(function(x) !identical(x, child), children)
                                child$set_parent(NULL)
                                tkpack.forget(child$get_block())
                              }
                              ))

