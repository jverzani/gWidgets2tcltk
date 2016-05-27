##' @include GContainer.R
NULL

##' toolkit constructor for gwindow
##'
##' @inheritParams gWidgets2::gwindow
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gwindow guiWidgetsToolkittcltk
.gwindow.guiWidgetsToolkittcltk <- function(toolkit, title, visible=visible, name, width, height, parent, handler, action,  ...) {
  GWindow$new(toolkit, title, visible=visible, name, width, height, parent, handler, action,  ...)
}

## Main class for gwindow instances
GWindow <- setRefClass("GWindow",
                            contains="GContainer",
                            fields=list(
                              frame="ANY",
                              menubar_area="ANY",
                              toolbar_area="ANY",
                              infobar_area="ANY",
                              content_area="ANY",
                              statusbar_area="ANY",
                              statusbar_widget="ANY",
                              modal_flag="ANY"
                              ),
                            methods=list(
                              initialize=function(toolkit=NULL, title="",  visible=TRUE, name=NULL, width=NULL, height=NULL,
                                parent=NULL, handler, action, ...) {

                                ## don't draw until asked
                                tclServiceMode(FALSE)
                                
                                block <<- tktoplevel()
                                set_value(title)
                                tkwm.state(block,"withdrawn")


                                frame <<- ttkframe(block)
                                
                                
                                if(is.null(width))
                                  width <- 400L
                                if(is.null(height))
                                  height <- as.integer(0.7 * width)

                                

                                if(!is.null(parent)) {
                                  set_location(parent)
                                  if(is(parent, "GWindow")) {
                                    add_handler_destroy(parent, function(h,...) {
                                      dispose_window()
                                    })
                                  }
                                }

                                
                                initFields(toolkit=toolkit, 
                                           menubar_area=ttkframe(block),
                                           toolbar_area=ttkframe(block),
                                           infobar_area=ttkframe(block),
                                           content_area=ttkframe(block,
                                             padding=ifelse(is_aqua(), c(0, 6, 14, 4), c(0,6, 0, 4))
                                             ),
                                           statusbar_area=ttkframe(block),
                                           modal_flag=tclVar(FALSE)
                                           )
                                widget <<- content_area # generic name
                                
                                tkconfigure(statusbar_area, borderwidth = 1, relief="sunken")
                                ## add areas to widget. For now we have simple
                                layout_widget()
                                tkbind(block, "<Unmap>", function() tkgrab.release(block))
                                
                                add_handler_changed(handler, action)

                                set_size(c(width, height))
                                tclServiceMode(TRUE)
                                set_visible(visible)
                                
                                callSuper(...)
                              },
                              layout_widget = function() {
                                ## we make a stack of widgets, content_area is the key one
                                tkgrid(toolbar_area, row=0, column = 0, sticky = "nswe")
                                tkgrid(menubar_area, row=1, column = 0, sticky = "nswe")
                                tkgrid(infobar_area, row=2, column = 0, sticky = "nswe")
                                tkgrid(content_area, row=3, column = 0, sticky = "nswe")
                                tkgrid(statusbar_area, row=4, column = 0, sticky = "we")

                                tkgrid.columnconfigure(block, 0, weight = 1)
                                tkgrid.rowconfigure(block, 3, weight = 1) # weight to content area
                                
                                tkgrid.propagate(content_area, FALSE) # make size carry forward
                              },
                              ## Widget methods
                              is_ttkwidget=function() {
                                FALSE
                              },
                              set_location=function(location) {
                                "Locate window based on other. If location is a widget, make transient. Else location can be (x,y) pair of pixel counts"

                                if(is(location, "GComponent"))
                                  location <- location$get_widget()
                                if(is(location, "tkwin")) {
                                  ## make transient to widget
                                  pos <- sapply(c("rootx", "rooty"), function(i) {
                                    as.numeric(tkwinfo(i, location))
                                  })
                                  tkwm.geometry(block, sprintf("+%s+%s", pos[1] + 30, pos[2] + 30))
                                  tkwm.transient(block, location) # set transient
                                  tkbind(location, "<Destroy>",function(...) tkdestroy(block))
                                  tkbind(location, "<Unmap>",function(...) tkdestroy(block))
                                } else if(is.numeric(location)) {
                                  pos <- rep(location, 2)
                                  tkwm.geometry(block, sprintf("+%s+%s", pos[1] + 30, pos[2] + 30))
                                }
                              },
                              set_modal=function(value) {
                                "Set or release modal"
                                if(value) {
                                  tkwm.protocol(block, "WM_DELETE_WINDOW", function() {
                                    tkgrab.release(block)
                                    dispose_window()
                                  })
                                  #tkgrab(block)
                                  tkwait.window(block)
                                } else {
                                  tkgrab.release(block)
                                }
                              },
                              get_value = function(...) tclvalue(tktitle(block)),
                              set_value = function(value, ...) {
                                tktitle(block) <<- paste(value, collapse=" ")
                              },
                              set_focus = function(value) {
                                if(value)
                                  tkraise(block)
                              },
                              set_visible=function(value) {
                                tkwm.state(block, ifelse(value, "normal", "withdrawn"))
                              },
                              set_size = function(value) {
                                tkwm.minsize(block, value[1], value[2])
                              },
                              update_widget=function(...) {
                                tkwm.geometry(block, "")
                                invisible()
                              },
                              ## set an icon
                              set_icon=function(stock) {
                                icon <- getStockIconByName(stock)
                                if(icon != "")
                                  tcl("wm", "iconphoto", block, default=icon)
                              },
                              ## add methods
                              add_child=function(child, ...) {
                                                                
                                if(missing(child) || is.null(child))
                                  return()

                                ## whoa nelly, must check on type of child here -- not just in add method
                                if(is(child, "GMenuBar")) {
                                  add_menubar(child)
                                } else if(is(child, "GToolBar")) {
                                  add_toolbar(child)
                                } else if(is(child, "GStatusBar")) {
                                  add_statusbar(child)
                                } else {
                                  ## clear out old (only one child allowed)
                                  sapply(as.character(tkwinfo("children", content_area)), tkpack.forget)
                                  children <<- list()
                                  ## add. Child can be
                                  if(is(child, "GComponent")) {
                                    tkpack(child$block, expand=TRUE, fill="both")
                                    child_bookkeeping(child)
                                  } else {
                                    tkpack(child, expand=TRUE, fill="both")
                                  }
                                  update_widget()
                                }

                              },
                              remove_child=function(child) {
                                if(is(child, "GComponent")) {
                                  child$set_parent(NULL)
                                  tkpack.forget(child$block)
                                } else {
                                  tkpack.forget(child)
                                }
                              },
                              dispose_window = function() {
                                "close window"
                                tcl("after", 5, function() {
                                  tkdestroy(block)
                                })
                              },
                              add_menubar=function(child, ...) {
                                XXX("")
                              },
                              add_toolbar=function(child, ...) {
                                XXX("")
                              },
                              set_infobar=function(msg, ...) {
                                .Tcl("ttk::style configure InfoBar.TLabel -background red")
                                
                                infobar <- ttkframe(infobar_area, padding=c(0,0,0,0))
                                label <- ttklabel(infobar, text=paste(msg, collapse=" "), style="InfoBar.TLabel")
                                tkpack(label, side="left", expand=TRUE,  fill="x")
                                tkpack(infobar, expand=TRUE, fill="x", side="left")

                                remove_infobar <- function() {
                                  tkpack.forget(infobar)
                                  tkconfigure(infobar_area, height=1)
                                }
                                tkbind(label, "<Motion>", remove_infobar)
                                       
                                ## hide after 4 seconds of mouse click
                                timer <- GTimer$new(ms=4*1000,
                                                    FUN=function(...) {
                                                      remove_infobar()
                                                    },
                                                    one.shot=TRUE,
                                                    toolkit=NULL,
                                                    start=TRUE)
                              },
                              ## handlers
                              add_handler_changed=function(handler, action=NULL, ...) {
                                add_handler_destroy(handler, action, ...)
                              },
                              add_handler_destroy=function(handler, action=NULL, ...) {
                                "window manager delete event"
                                add_handler("<Destroy>", handler, action=action, ...)
                              },
                              add_handler_unrealize=function(handler, action, ...) {
                                "Intercept window manager delete event"
                                h <- list(obj=.self, action=action)
                                tkwm.protocol(block, "WM_DELETE_WINDOW",
                                              function(...) {
                                                val <- handler(h,...)
                                                ## FALSE -- keep, TRUE -- destroy
                                                if(is.null(val)  ||
                                                   (is.logical(val) && val)
                                                   )
                                                  tkdestroy(block) ## revers
                                              })
                              }
                              ))


                              
