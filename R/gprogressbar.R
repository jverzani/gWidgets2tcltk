##' @include GWidget.R
NULL

##' Toolkit  constructor
##'
##' @inheritParams gWidgets2::gprogressbar
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @seealso The documentation for this is found at \code{\link{gprogressbar}}.
##' @method .gprogressbar guiWidgetsToolkittcltk
##' @S3method .gprogressbar guiWidgetsToolkittcltk
.gprogressbar.guiWidgetsToolkittcltk <- function(toolkit, value, container, ...) {
  GProgressBar$new(toolkit, value, container, ...)
}

##' For tcltk, the Gprogressbar class has the extra reference method
##' \code{set_border}. The \code{border} argument has been deprecated.
##' @rdname gWidgets2tcltk-package
GProgressBar <- setRefClass("GProgressBar",
                            contains="GWidget",
                            methods=list(
                              initialize=function(toolkit=NULL, value, container, ...) {
                                
                                widget <<- ttkprogressbar(container$get_widget())

                                if(!missing(value))
                                  set_value(value)
                                
                                initFields(block=widget)
                                
                                add_to_parent(container, .self, ...)

                                callSuper(toolkit)
                              },
                              set_value=function(value, index=TRUE, drop=TRUE, ...) {
                                if(is.null(value))
                                  perc <- get_value() + 10
                                else
                                  perc <- value
                                tkconfigure(widget, value = as.numeric(perc) %% 100)
                                tkconfigure(widget, mode=if(is.null(value)) "indeterminate" else "determinate")
                              },
                              get_value=function(index=TRUE, drop=TRUE, ...) {
                                as.numeric(tkcget(widget, "-value"))
                              }
                              ))


