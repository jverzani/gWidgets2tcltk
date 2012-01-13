##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::ggroup
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gseparator guiWidgetsToolkittcltk
##' @S3method .gseparator guiWidgetsToolkittcltk
.gseparator.guiWidgetsToolkittcltk <-  function(toolkit,
                                         horizontal = TRUE,
                   container = NULL, ... ) {
  GSeparator$new(toolkit, horizontal=horizontal, container = container, ...)
}


GSeparator <- setRefClass("GSeparator",
                          contains="GWidget",
                          methods=list(
                            initialize=function(toolkit,
                              horizontal=TRUE, container=NULL,
                              ...) {

                              if(is.null(container)) {
                                ## possible stub
                                widget <<- NULL
                              } else {
                                widget <<- ttkseparator(container$get_widget(),
                                                       orient=ifelse(horizontal, "horizontal", "vertical"))
                              }

                              initFields(block=widget)
                              if(!is.null(container))
                                add_to_parent(container, .self, ...)
                              
                              callSuper(toolkit)
                            }
                            ))

