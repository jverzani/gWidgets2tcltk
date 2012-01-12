##' @include GWidget.R
NULL

##' Toolkit  constructor
##'
##' @inheritParams gWidgets2::gbutton
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @seealso The documentation for this is found at \code{\link{gbutton}}.
##' @method .gbutton guiWidgetsToolkittcltk
##' @S3method .gbutton guiWidgetsToolkittcltk
.gbutton.guiWidgetsToolkittcltk <- function(toolkit, text, handler, action, container, ...) {
  if(is(action, "GAction"))
    GButtonAction$new(toolkit, text, handler, action, container, ...)
  else
    GButton$new(toolkit, text, handler, action, container, ...)
}

##' For RGtk2, the GButton class has the extra reference method
##' \code{set_border}. The \code{border} argument has been deprecated.
##' @rdname gWidgets2tcltk-package
GButton <- setRefClass("GButton",
                            contains="GWidget",
                            methods=list(
                              initialize=function(toolkit=NULL, text=NULL,  handler, action, container, ...) {
                                
                                widget <<- ttkbutton(container$get_widget())
                                toolkit <<- toolkit # otherwise next line fails to find toolkit for dispatch

                                if(!is_empty(text))
                                  set_value(text)


                                
                                initFields(block=widget,
                                           change_signal="command"
                                           )
                                

                                add_to_parent(container, .self, ...)
                                add_handler_changed(handler, action)
                                callSuper(toolkit)
                              },
                              set_value=function(value, index=TRUE, drop=TRUE, ...) {
                                "Set value, does not invoke widget"

                                tkconfigure(widget, text=as.character(value))
                                icon <- getStockIconByName(value, toolkit=toolkit)
                                tkconfigure(widget, image=ifelse(is.null(icon), "", icon))

                              },
                              set_index=function(...) set_value(...),
                              get_value=function(...) {
                                val <- paste(as.character(tkcget(widget,"-text")),
                                             sep=" ",collapse=" ")
                                return(val)
                              },
                              get_index=function(...) get_value(),
                              set_font = function(value) {
                                XXX("Is this right")
                               callSuper(value)
                              },
                              ## Handler: changed -> clicked
                              add_handler_clicked=function(handler, action=NULL, ...) {
                                add_handler_changed(handler, action, ...)
                              },
                              ## Extra methods
                              remove_border=function() {
                                "Remove border by setting relief to none"
                                XXX("remove border")
                              }
                              ))


## XXX
GButtonAction <- setRefClass("GButtonAction")
                             
