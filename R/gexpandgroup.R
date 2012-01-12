##' @include gcontainer.R
NULL

##' toolkit constructor
##'
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gexpandgroup guiWidgetsToolkittcltk
##' @S3method .gexpandgroup guiWidgetsToolkittcltk
.gexpandgroup.guiWidgetsToolkittcltk <- function(toolkit,
                                                 text, markup,  horizontal=TRUE,
                                                 handler=NULL, action=NULL,
                                                 container=NULL, ...) {
  GExpandGroup$new(toolkit, text=text, markup=markup, horizontal=horizontal, handler=handler, action=action, container=container, ...)
}

## base class from gframe
GExpandGroup <- setRefClass("GExpandGroup",
                            contains="GBoxContainer",
                            fields=list(
                              disclose_icon="ANY",
                              label="ANY",
                              inner_frame="ANY",
                              ..visible = "logical"
                              ),
                            methods=list(
                              initialize=function(toolkit=NULL, text="", markup=FALSE, horizontal=TRUE, handler, action=NULL, container=NULL, ...) {

                                block <<- ttkframe(container$get_widget())
                                inner_frame <<- ttkframe(block)
                                
                                disclose_icon <<- ttkcheckbutton(inner_frame,  variable=tclVar(1))
                                label <<-ttklabel(inner_frame)
                                widget <<- ttkframe(block)

                                do_layout()
                                set_names(text)
gc

                                initFields(horizontal=horizontal,
                                          ..visible=TRUE,
                                          change_signal="<<StateChanged>>"
                                          )
                                set_spacing(5L)
                                
                                handler_id <<- add_handler_changed(handler, action)
                                add_to_parent(container, .self, ...)
                                
                                callSuper(toolkit, horizontal=horizontal, ...)
                              },
                              do_layout=function() {
                                tkpack(inner_frame, expand=FALSE, fill="x", side="top")
                                tkpack(disclose_icon, expand=FALSE, fill="none", anchor="w", side="left")
                                tkpack(label, expand=TRUE, fill="x", anchor="w", side="left", padx=2)

                                tkpack(widget, expand=TRUE, fill="both")

                                tkconfigure(disclose_icon, command=function() {
                                  set_visible(!get_visible())
                                })
                              },
                              get_names=function(...) {
                                as.character(tkcget(label, "-text"))
                              },
                              set_names=function(value, ...) {
                                tkconfigure(label, text=paste(value, collapse=" "))
                              },
                              get_visible = function() {
                                ..visible
                              },
                              set_visible = function(value) {
                                if(value) {
                                  tkpack("propagate", widget, TRUE)
                                  tkpack(widget, expand=TRUE, fill="both")
                                  ..visible <<- TRUE
                                } else {
                                  width <- as.numeric(tkwinfo("width", widget))
                                  tkpack("propagate", widget, FALSE)
                                  ## configure height but not width!!!
                                  tkconfigure(widget, "height"=1, width=width)
                                  ..visible <<- FALSE
                                }

                                tcl("event", "generate", widget, change_signal)
                              }
                              ))
                            
