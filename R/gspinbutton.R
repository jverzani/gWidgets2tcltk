##' @include GWidget.R
NULL

##' Toolkit XXX constructor
##'
##' @param digits digits
##' @inheritParams gWidgets2::gslider
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gspinbutton guiWidgetsToolkittcltk
.gspinbutton.guiWidgetsToolkittcltk <-  function(toolkit,
                                                 from = 0, to = 10, by = 1, value = from, digits = 0,
                                                 handler = NULL,action = NULL, container = NULL, ... ) {
  GSpinButton$new( toolkit, from , to , by, value, digits,
                  handler = handler, action = action, container = container, ...)
}


## Spinbutton class
##
## @param ... passed to constructor
## \code{GSpinButton} is the base class for spin buttons. The widget
## is buggy in tcltk, atleast with the old non-themed style and a Mac
## running tcltk under X11. The bug is the thing keeps spinning when
## the buttons are pressed.
GSpinButton <- setRefClass("GSpinButton",
                            contains="GWidget",
                           fields=list(
                             oldstyle_widget="logical"
                             ),
                            methods=list(
                              initialize=function(toolkit,
                                from = 0, to = 10, by = 1, value = from, digits = 0,
                                handler, action, container, ...) {

                                if(digits == 0 &&  as.logical((by %% 1))) # FALSE if integer o/w T
                                  digits <- abs(floor(log(by,10)))

                                init_widget(container$get_widget(), from, to, by)
                                ## configure
                                
                                initFields(block=widget,
                                           change_signal="command"
                                           )
                                
                                set_value(value)

                                add_to_parent(container, .self, ...)

                                handler_id <<- add_handler_changed(handler, action)

                                callSuper(toolkit)
                              },
                              init_widget=function(parent, from, to, by) {
                                ## ttk spinbox new as of 8.5.9 
                                out <- try(tkwidget(parent, "ttk::spinbox", from=from, to=to, increment=by), silent=TRUE)
                                if(inherits(out, "try-error")) {
                                  out <- tkwidget(parent, "spinbox", from=from, to=to, increment=by)
                                  oldstyle_widget <<- TRUE
                                } else {
                                  oldstyle_widget <<- FALSE
                                }
                                widget <<- out
                              },
                              is_ttkwidget=function() {
                                !oldstyle_widget
                              },
                              get_value=function(drop=TRUE, ...) {
                                 as.numeric(tcl(widget,"get"))
                              },
                              set_value=function(value, drop=TRUE, ...) {
                                tcl(widget, "set", as.numeric(value))
                              },
                              set_items = function(value, i, ...) {
                                ## check that value is a regular sequence
                                if(length(items) <=1) {
                                  warning("Can only assign a vector with equal steps, as produced by seq")
                                  return(obj)
                                }
                                if(length(items) > 2 &&
                                   !all.equal(diff(diff(items)), rep(0, length(items) - 2))) {
                                  warning("Can only assign a vector with equal steps, as produced by seq")
                                  return(obj)
                                }
                                
                                ## get current value, increment
                                curValue <- get_value()
                                inc <- head(diff(items), n=1)
                                
                                tkconfigure(widget, from=min(items), to=max(items), increment=inc)
                                tcl(widget, "set", curValue)
                              }
                              ))

