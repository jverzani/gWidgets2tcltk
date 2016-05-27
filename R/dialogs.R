##' @include GWidget.R
##' @include GContainer.R
NULL



##' toolkit implementation for gmessage
##'
##' @inheritParams gWidgets2::gmessage
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gmessage guiWidgetsToolkittcltk
.gmessage.guiWidgetsToolkittcltk <- function(toolkit,
                                             msg,
                                             title = "message",
                                             icon = c("info","warning","error","question"),
                                             parent=NULL,
                                             ...
                                             ) {

  icon <- match.arg(icon)
  l <- list(icon=icon,
            message=gettext(msg[1]),
            title = title,
            type="ok")
  if(length(msg) > 1)
    l$detail <- gettext(msg[2])
            
  if(!is.null(parent))
    l$parent <- getWidget(parent)

  out <- do.call("tkmessageBox",l)
  ## logical
  return(as.character(out) == "ok")
}


##' toolkit implementation for gconfirm
##'
##' @inheritParams gWidgets2::gconfirm
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gconfirm guiWidgetsToolkittcltk
.gconfirm.guiWidgetsToolkittcltk <-  function(toolkit,
                                              msg,
                                              title = "Confirm",
                                              icon = c("info","warning","error","question"),
                                              parent=NULL,
                                              ...
                                              ) {
  icon <- match.arg(icon)
            
  l <- list(icon=icon,
            message=gettext(msg[1]),
            title = title,
            type="yesno")
  
  if(length(msg) > 1)
    l$detail <- gettext(msg[2])
  
  if(!is.null(parent))
    l$parent <- getWidget(parent)
  out <- do.call("tkmessageBox",l)
  val <- as.character(out) == "yes"
  
  return(val)
}




##' toolkit implmentation of ginput
##'
##' @inheritParams gWidgets2::ginput
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .ginput guiWidgetsToolkittcltk
.ginput.guiWidgetsToolkittcltk <- function(toolkit,
                                           msg,
                                           text="",
                                           title = "Input",
                                           icon = c("info","warning","error","question"),
                                           parent=NULL,                   
                                           ...
                                           ) {

  icon <- c(info="ok", warning="alert", error="error", question="help")[match.arg(icon)]

  
  dlg <- gwindow(title, parent=parent, visible=FALSE)
  g <- GGroup$new(cont=dlg, horizontal=TRUE)
  GImage$new(file=icon, dir="stock", cont=g, expand=FALSE)
  g1 <- GGroup$new(cont=g, horizontal=FALSE, expand=TRUE, fill="both")
  GLabel$new(text=msg[1], cont=g1)
  ed <- GEdit$new(text=text, cont=g1)
  bg <- GGroup$new(cont=g1, horizontal=TRUE)
  ok <- GButtonNoAction$new(text="ok", cont=bg, action=NULL, handler=function(h,...) {
    confirmed <<- TRUE
    tclvalue(flag) <- "destroy"
  })
  cancel <- GButtonNoAction$new(text="cancel", cont=bg, action=NULL, handler=function(h,...) {
    confirmed <<- FALSE
    tclvalue(flag) <- "destroy"
  })


  confirmed <- FALSE
  flag <- tclVar("")
  ## bind to destroy event
  tkwm.protocol(dlg$block, "WM_DELETE_WINDOW", function() {
    tclvalue(flag) <- "destroy"
  })

  tkfocus(ed$widget)
  dlg$set_visible(TRUE)
  
  ## make modal
  tkwait.variable(flag)
  out <- ""
  if(confirmed) 
    out <- ed$get_value()

  dlg$dispose_window()
  return(out)
                        
}


##' toolkit implementation
##'
##' @inheritParams gWidgets2::gbasicdialog
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gbasicdialog guiWidgetsToolkittcltk
.gbasicdialog.guiWidgetsToolkittcltk <- function(toolkit,
                                                 title = "Dialog",
                                                 parent=NULL,
                                                 do.buttons=TRUE,
                                                 handler = NULL,
                                                 action = NULL,
                                                 ...
                                                 ) {
            
  obj <- GBasicDialog$new(toolkit,
                          title=title, parent=parent, do.buttons=do.buttons,
                          handler=handler, action=action, 
                          ...)
  obj
}


## class for basic dialog
GBasicDialog <- setRefClass("GBasicDialog",
                    contains="GContainer",
                    fields=list(
                      handler="ANY",
                      action="ANY",
                      toplevel="ANY"
                      ),
                    methods=list(
                      initialize=function(toolkit=NULL,
                        title = "Dialog",
                        parent=NULL,
                        do.buttons=TRUE,
                        handler = NULL,
                        action = NULL,
                        ...) {

                        ## use a modal gwindow with visible method
                        w <- gwindow(title, parent=parent)
                        toplevel <<- w
                        g <- ggroup(horizontal=FALSE, cont=w)
                        content_area <- ggroup(horizontal=FALSE, cont=g, expand=TRUE, fill="both")

                        widget <<- content_area$widget
                        block <<- widget

                        if(do.buttons) {
                          button_group <- ggroup(cont=g, horizontal=TRUE)
                          f <- function(h, ...) {
                            toplevel$set_modal(FALSE)
                            if(!is.null(handler))
                              handler(list(obj=NULL, action=h$action))
                            toplevel$dispose_window()
                          }
                          ok_button <- gbutton("ok", cont=button_group, handler=f)
                          cancel_button <- gbutton("cancel", cont=button_group, handler=function(...) {
                            dispose_window()
                          })
                        }
                      },
                      add_child=function(child, ...) {
                        tkpack(child$block, side="top", expand=TRUE, fill="both")
                      },
                      dispose_window=function() {
                        toplevel$set_modal(FALSE)
                        toplevel$dispose_window()
                      },
                      set_visible=function(value) {
                        if(value) {
                          toplevel$set_visible(TRUE)
                          toplevel$set_modal(TRUE)
                        } 
                      }
                      ))


##' toolkit implementation of galert
##'
##' @param delay delay
##' @inheritParams gWidgets2::gaction
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .galert guiWidgetsToolkittcltk
.galert.guiWidgetsToolkittcltk <-  function(toolkit,
                                            msg,
                                            title = "message",
                                            delay = 3,
                                            parent=NULL,
                                            ...
                                            ) {

            ## insert an info bar here?
            if(is(parent, "GWindow")) {
              parent$set_infobar(msg)
            } else {
              ## make a transient dialog window centered on parent
              w <- GWindow$new(title, width=250, height=50, parent=parent, visible=FALSE)
              g <- GGroup$new(container=w)
              g1 <- GGroup$new(container=g, horizontal=FALSE, expand=TRUE)
              l <- GLabel$new(text=msg[1], container=g1)
              ## set bold
              if(length(msg) > 1)
              l1 <- GLabel$new(text=paste(msg[-1], collapse="\n"), container=g1)

              f <- function(...) w$dispose_window()
              GImage$new(file="dismiss", dir="stock", container=g, handler=f, anchor=c(1,1))

              w$set_visible(TRUE)
              timer <- GTimer$new(ms=delay*1000, FUN=f, one.shot=TRUE)
            }
          }
 
