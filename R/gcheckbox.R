##' @include GWidget.R
##' @include gmenu.R
NULL

##' Toolkit XXX constructor
##'
##' @inheritParams gWidgets2::gcheckbox
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gcheckbox guiWidgetsToolkittcltk
.gcheckbox.guiWidgetsToolkittcltk <- function(toolkit,
                                              text, checked = FALSE, use.togglebutton=FALSE, handler = NULL, action = NULL,
                                              container = NULL, ... ) {

  GCheckbox$new(toolkit,
                text, checked, use.togglebutton, handler, action, container, ...)
}

## Checkbox reference class
GCheckbox <- setRefClass("GCheckbox",
                         contains="GWidgetWithTclVariable",
                         fields=list(
                           menu_proxies="ANY"
                           ),
                         methods=list(
                           initialize=function(toolkit=NULL,
                             text="", checked = FALSE, use.togglebutton=FALSE,
                             handler = NULL, action = NULL,
                             container = NULL, parent, ... ) {
                             
                             t_var <<- tclVar(as.numeric(checked))

                             if(!missing(parent)) # a menu item
                               container <- parent
                             
                             l <- list(parent=container$get_widget(),
                                       text=as.character(text),
                                       variable=t_var)
                             if(use.togglebutton)
                               l$style <- "Toolbutton"
                             widget <<- do.call(ttkcheckbutton, l)
                             
                             initFields(block=widget,
                                        change_signal="command",
                                        menu_proxies=GMenuProxy$new()                                     
                                        )

                             if(missing(parent) || is(parent, "GToolBar")) # not menu item
                               add_to_parent(container, .self, ...)
                             
                             handler_id <<- add_handler_changed(handler, action)
                             
                             callSuper(toolkit)
                           },
                           get_value=function(...) {
                             val <- callSuper()
                             as.logical(as.numeric(val))
                           },
                           set_value=function(value, ...) {
                             menu_proxies$set_value(value)
                             callSuper(value, ...)
                           },
                           get_items = function(i, j, ..., drop=TRUE) {
                             as.character(tkcget(widget, "-text"))
                           },
                           set_items = function(value, i, j, ...) {
                             tkconfigure(widget, text=as.character(value))
                           },
                           set_enabled=function(value) {
                             menu_proxies$set_enabled(value)
                             callSuper(value)
                           },
                           add_menu_item_proxy=function(mb, index) {
                             menu_proxies$add_proxy(mb, index)
                           }
                           ))

