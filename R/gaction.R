##' @include GWidget.R
##' @include gmenu.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gaction
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gaction guiWidgetsToolkittcltk
.gaction.guiWidgetsToolkittcltk <-  function(toolkit,
                                             label, tooltip=NULL, icon = NULL, key.accel = NULL,
                                             handler = NULL,action = NULL, parent = NULL, ... ) {
  GAction$new(toolkit,
              label, tooltip=tooltip, icon = icon, key.accel = key.accel,
              handler = handler,action = action, parent = parent, ...)
}


## GAction class
GAction <- setRefClass("GAction",
                       contains="GWidget",
                       fields=list(
                         label="character",
                         tooltip="ANY",
                         icon="ANY",
                         accel_key="ANY",
                         parent="ANY",
                         observers="ANY",
                         menu_proxies="ANY",
                         ..enabled="logical"
                         ),
                       methods=list(
                         initialize=function(toolkit=NULL,
                           label="", tooltip=NULL, icon = NULL, key.accel = NULL,
                           handler, action=NULL, parent, ...) {


                           initFields(
                                      widget=NULL,
                                      block=NULL,
                                      observers=List$new(),
                                      menu_proxies=GMenuProxy$new(),
                                      label=label,
                                      tooltip=tooltip,
                                      icon=icon,
                                      accel_key=key.accel,
                                      parent=parent,
                                      change_signal="<<InvokeAction>>",
                                      ..enabled=TRUE
                                      )
                           
                           
                           if(!is.null(parent) && !is.null(handler))
                             add_key_accel(parent, handler)

                           handler_id <<- add_handler_changed(handler, action)
                           
                           callSuper(toolkit)
                         },
                         add_listener=function(observer) {
                           observers$push(observer)
                         },
                         add_menu_item_proxy=function(mb, index) {
                           menu_proxies$add_proxy(mb, index)
                         },
                         add_key_accel=function(parent, handler) {
                           "Hack to add in accelerator button binding"
                           tkbind(parent$get_block(), accel_key, function() {
                             invoke_change_handler()
                           })
                         },
                         get_value=function( ...) {
                           label
                         },
                         set_value=function(value, ...) {
                           observers$each(function(i, key, widget) widget$set_value(value))
                           menu_proxies$set_value(value)
                         },
                         get_icon=function(...) {
                           icon
                         },
                         set_icon=function(value, ...) {
                           icon <<- value
                           observers$each(function(i, key, widget) widget$set_icon(value))                           
                           menu_proxies$set_value(value)
                         },
                           
                         get_tooltip=function(...) {
                           tooltip
                         },
                         set_tooltip=function(value, ...) {
                           observers$each(function(i, key, widget) widget$set_tooltip(value))
                         },
                         get_enabled=function(...) ..enabled,
                         set_enabled=function(value, ...) {
                           ..enabled <<- as.logical(value)
                           observers$each(function(i, key, widget) widget$set_enabled(value))
                           menu_proxies$set_enabled(value)
                         }
                         ))

