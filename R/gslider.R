##' @include GWidget.R
NULL

##' Toolkit  constructor
##'
##' @inheritParams gWidgets2::gslider
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gslider guiWidgetsToolkittcltk
.gslider.guiWidgetsToolkittcltk <-  function(toolkit,
                                             from = 0, to = 100, by = 1, value = from, horizontal = TRUE,
                                             handler = NULL, action = NULL, container = NULL, ... ) {
  GSlider$new(toolkit,
              from, to, by, value, horizontal,
              handler,action, container, ...)
}


## glider class
GSlider <- setRefClass("GSlider",
                       contains="GWidgetWithTclVariable",
                       fields=list(
                         items = "ANY"
                         ),
                       methods=list(
                         initialize=function(toolkit,
                           from, to, by, value, horizontal,
                           handler, action, container, ...) {
                           if(length(from) == 1)
                             x <- seq(from, to, by)
                           else
                             x <- from
                           x <- sort(unique(x))
                           items <<- x
                           ind <- max(which(items <= value)) #match(value, items)

                           t_var <<- tclVar(ind)
                           orientation <- ifelse(horizontal, "horizontal", "vertical")
                           widget <<- tkwidget(container$get_widget(), "ttk::scale", from=1L, to=length(x),
                                               variable=t_var,
                                               orient=orientation)
                           
                           
                           initFields(block=widget,
                                      default_expand=TRUE,
                                      default_fill=ifelse(horizontal, "x", "y"),
                                      change_signal="command")
                           
                           add_to_parent(container, .self, ...)

                           add_handler_changed(function(...) update_tooltip())
                           update_tooltip()
                           
                           handler_id <<- add_handler_changed(handler, action)
                           
                           callSuper(toolkit, ...)
                         },
                         get_value=function(drop=TRUE, ...) {
                           items[get_index()]
                         },
                         get_index = function(...) {
                           as.integer(tclvalue(t_var))
                         },
                         set_value=function(value, drop=TRUE, ...) {
                             i <- max(which(items <= value))
#                             i <- match(value, items)
                             if(!is.na(i)) 
                               set_index(as.integer(i))
                         },
                         set_index = function(value,...) {
                           a <- t_var
                           tclvalue(a) <- as.integer(value)
                           invoke_change_handler()
                           update_tooltip()
                         },
                         get_items = function(i, ...) {
                           items
                         },
                         set_items = function(value, i, ...) {
                           items <<- sort(unique(value))
                           tkconfigure(widget, from=1L, to=length(items))
                           
                           set_index(1L)
                         },
                         update_tooltip=function() {
                             val = as.character(get_value())
                             tk2tip(widget, val)
                         }

                         ))

