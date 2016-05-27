##' @include GWidget.R
NULL

##' Toolkit  constructor
##'
##' @inheritParams gWidgets2::gbutton
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @seealso The documentation for this is found at \code{\link{gbutton}}.
##' @method .gbutton guiWidgetsToolkittcltk
.gbutton.guiWidgetsToolkittcltk <- function(toolkit, text, handler, action, container, ...) {
  if(is(action, "GAction"))
    GButtonAction$new(toolkit, action, container, ...)
  else
    GButtonNoAction$new(toolkit, text, handler, action, container, ...)
}

##' For RGtk2, the GButton class has the extra reference method
##' \code{set_border}. The \code{border} argument has been deprecated.
##' @rdname gWidgets2tcltk-package
GButton <- setRefClass("GButton",
                            contains="GWidget",
                            methods=list(
                             
                              set_value=function(value, index=TRUE, drop=TRUE, ...) {
                                "Set value, does not invoke button's command"
                                value <- as.character(value)
                                tkconfigure(widget, text=value)
                                set_icon(value)

                              },
                              set_index=function(...) set_value(...),
                              get_value=function(...) {
                                val <- paste(as.character(tkcget(widget,"-text")),
                                             sep=" ",collapse=" ")
                                return(val)
                              },
                              get_index=function(...) get_value(),
                              set_icon=function(value, ...) {
                                if(!is(value, "gWidgetstcltkIcon"))
                                  value <- getStockIconByName(value)
                                if(!missing(value) && !is.null(value)) {
                                  tkconfigure(widget, image=value, compound="left")
                                }
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


GButtonNoAction <- setRefClass("GButtonNoAction",
                       contains="GButton",
                       methods=list(
                         initialize=function(toolkit=NULL, text=NULL,  handler, action=NULL, container, ...) {
                                
                           widget <<- ttkbutton(container$get_widget())
                           
                           if(!is_empty(text))
                             set_value(text)

                           initFields(block=widget,
                                      change_signal="command"
                                      )
                           

                           
                           
                           add_to_parent(container, .self, ...)
                           add_handler_changed(handler, action)
                           ## invoke button on Return or Enter key
                           tkbind(widget, "<Return>", function(W) tkinvoke(W))
                           
                           callSuper(toolkit)
                         }
                         ))
## XXX
GButtonAction <- setRefClass("GButtonAction",
                             contains="GButton",
                             methods=list(
                               initialize=function(toolkit, action, container, ...) {


                                 widget <<- ttkbutton(container$get_widget())
                                 block <<- widget
                                
                                 set_value(action$get_value())
                                 set_icon(action$get_icon())
                                 set_tooltip(action$get_tooltip())

                                tkconfigure(widget, command=function() {
                                  action$invoke_change_handler()
                                })

                                action$add_listener(.self)
                                
                                add_to_parent(container, .self, ...)
                                callSuper(toolkit)                             
                              }
                               ))
                             
