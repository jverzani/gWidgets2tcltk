##' @include GContainer.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::glabel
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gstatusbar guiWidgetsToolkittcltk
.gstatusbar.guiWidgetsToolkittcltk <-  function(toolkit,
                                                text="",
                                                container = NULL, ... ) {
  GStatusBar$new(toolkit,
                 text=text,
                 container = container, ...)
}


## \code{GStatusBar} is the base class for a status bar
##
## The \code{GStatusBar} class inherits for \code{GBoxContainer}
## meaning it can be used as a parent container. As such, one can add
## additional widgets beyond the plain label that is the main
## property of this widget.
## @param ... passed to constructor
GStatusBar <- setRefClass("GStatusBar",
                          contains="GBoxContainer",
                          fields=list(
                            label="ANY"
                            ),
                          methods=list(
                            initialize=function(toolkit=NULL,
                              text="", container=NULL, ...) {
                              
                              ## only works with windows
                              if(!is(container, "GWindow")) {
                                warning(gettext("Status bar only works with toplevel windows"))
                                return()
                              }
                              
                              widget <<- ttkframe(container$statusbar_area, padding=c(0,0,13,0))
                              tkpack(widget, expand=TRUE, fill="x", side="left")

                              initFields(
                                         block=widget,
                                         horizontal=TRUE
                                         )
                              set_spacing(1)

                              
                              label <<- ttklabel(widget)
                              add_child(label, expand=FALSE, anchor=c(-1,0))
                              set_value(text)
                              
                              callSuper(toolkit)
                            },
                            get_value=function(...) {
                              as.character(tkcget(label, "-text"))
                            },
                            set_value=function(value, ...) {
                              tkconfigure(label, text=as.character(value))
                            }
                            ))

