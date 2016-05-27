##' @include GContainer.R
NULL

##' toolkit constructor
##'
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gexpandgroup guiWidgetsToolkittcltk
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
                              t_var = "ANY",
                              label="ANY",
                              inner_frame="ANY"
                              ),
                            methods=list(
                              initialize=function(toolkit=NULL, text="", markup=FALSE, horizontal=TRUE, handler, action=NULL, container=NULL, ...) {

                                block <<- ttkframe(container$get_widget())
                                inner_frame <<- ttkframe(block)
                                t_var <<- tclVar(1)
                                disclose_icon <<- ttkcheckbutton(inner_frame,  variable=t_var)
                                label <<- ttklabel(inner_frame)
                                widget <<- ttkframe(block)

                                do_layout()
                                set_names(text)


                                initFields(horizontal=horizontal,
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

                                
                                tkpack(widget, expand=TRUE, fill="both", anchor="nw")
                                
                                tkconfigure(disclose_icon, command=function() {
                                  if(get_visible()) show_container() else hide_container()
                                        tcl("event", "generate", widget, change_signal)
                                })
                              },
                              get_names=function(...) {
                                as.character(tkcget(label, "-text"))
                              },
                              set_names=function(value, ...) {
                                tkconfigure(label, text=paste(value, collapse=" "))
                              },
                              get_visible = function() {
                                tclvalue(t_var) == "1"
                              },
                              show_container=function() {
                                tkpack("propagate", block, TRUE)
#                                tkpack("propagate", widget, FALSE)
                                tkpack(widget, expand=TRUE, fill="both")
                              },
                              hide_container=function() {
                                width <- as.numeric(tkwinfo("width", widget))
#                                tkpack("propagate", block, FALSE)
                                ## configure height but not width!!!
                                tkpack.forget(widget)
                                        #                                  tkconfigure(widget, "height"=1, width=width)
                              },
                              set_visible = function(value) {
                                tmp <- t_var
                                tclvalue(tmp) <- as.numeric(value)
                                if(value) show_container() else hide_container()
                                tcl("event", "generate", widget, change_signal)
                              },
                              set_enabled=function(value) {
                                sapply(list(disclose_icon, label), function(i) {
                                  tcl(i, "state", ifelse(value, "!disabled", "disabled"))
                                })
                                callSuper(value)
                              },
                              set_font=function(value) {
                                set_font_ttk(value, label)
                              }
                              ))
                            
