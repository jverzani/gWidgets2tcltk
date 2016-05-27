##' @include GContainer.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gnotebook
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gstackwidget guiWidgetsToolkittcltk
.gstackwidget.guiWidgetsToolkittcltk <-  function(toolkit,
                                                  container = NULL, ... ) {
  GStackWidget$new(toolkit,
                   container = container, ...)
}



GStackWidget <- setRefClass("GStackWidget",
                            contains="GContainer",
                            methods=list(
                              initialize=function(toolkit=NULL,
                                 container=NULL, ...) {

                                widget <<- ttkframe(container$get_widget())
                                block <<- widget

                                add_to_parent(container, .self, ...)

                                callSuper(toolkit)
                              },
                              add_child=function(child,  index=NULL,  ...) {
                                "Similar to GNotebook's, but without label and close button code"

                                n <- get_length()
                                if(is.null(index))
                                  index <- n
                                index <- as.integer(index)

                                ## insert into children
                                if(index <= 0) {
                                  children <<- c(child, children)
                                } else if(index >= n) {
                                  children <<- c(children, child)
                                } else {
                                  children <<- c(children[1:index], child, children[(index+1):n])
                                }
                                set_index(index + 1)
                              },
                              get_value=function(...) get_index(),
                              set_value=function(value, ...) set_index(value),
                              set_index=function(ind) {
                                tclServiceMode(TRUE)
                                ## remove child
                                sapply(as.character(tkwinfo("children", widget)), tkpack.forget)
                                ## add child
                                tkpack(children[[ind]]$get_block(), expand=TRUE, fill="both")
                                tclServiceMode(TRUE)
                              },
                              get_index=function(...) {
                                "which card is shown?"
                                ## hack with tk IDs
                                tk_children <- as.character(tkwinfo("children", widget))
                                tk_id <- Filter(function(i) as.logical(tkwinfo("ismapped", i)), tk_children)
                                if(length(tk_id) == 0)
                                  return(NA)
                                child_ids <- sapply(children, function(i) i$get_widget()$ID)
                                match(tk_id, child_ids)
                              },
                              get_length=function() {
                                length(children)
                              },
                              remove_current_page=function() {
                                remove_page(get_index())
                              },
                              remove_page=function(idx) {
                                if(! (1 <= idx && idx <= get_length())) return()
                                
                                
                                child <- children[[idx]]
                                ## remove from GUI, then from children
                                if(idx == get_index()) {
                                  if(idx > 1) set_index(idx - 1)
                                  if(idx == 1 && get_length() > 1) set_index(idx + 1)
                                  if(idx == 1 && get_length() == 1) {
                                    warning(gettext("Removing last page"))
                                    tkpack.forget(getBlock(child))
                                  }
                                }
                                children[[idx]] <<- NULL
                              }
                              ))

