##' @include GWidget.R
NULL



##' toolkit implementation for gmessage
##'
##' @inheritParams gWidgets2::ginput
##' @return NULL
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gmessage guiWidgetsToolkittcltk
##' @S3method .gmessage guiWidgetsToolkittcltk
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
    l$detail=gettext(msg[2])
            
  if(!is.null(parent))
    l$parent <- getWidget(parent)

  out <- do.call("tkmessageBox",l)
  ## logical
  return(as.character(out) == "ok")
}


##' toolkit implementation for gconfirm
##'
##' @inheritParams gWidgets2::ginput
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gconfirm guiWidgetsToolkittcltk
##' @S3method .gconfirm guiWidgetsToolkittcltk
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
    l$detail=gettext(msg[2])
  
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
##' @S3method .ginput guiWidgetsToolkittcltk
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
  ed <- GEdit$new(initial.msg=text, cont=g1)
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
##' @S3method .gbasicdialog guiWidgetsToolkittcltk
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
                      action="ANY"
                      ),
                    methods=list(
                      initialize=function(toolkit=NULL,
                        title = "Dialog",
                        parent=NULL,
                        do.buttons=TRUE,
                        handler = NULL,
                        action = NULL,
                        ...) {
                        
                        widget <<- gtkHBox() 
                        
                        ## parent
                        .parent <- parent
                        if(!is.null(.parent)) {
                          .parent <- getBlock(.parent)
                          if(!is(.parent,"GtkWindow"))
                            .parent <- .parent$GetWindow()
                          if(!is(.parent,"GtkWindow"))
                            .parent <- NULL          # give up
                        } else {
                          .parent <- gtkWindowNew(show=FALSE)
                        }
            

                        buttons <- c("ok", "cancel")

                        ## can override, though not included here
                        buttonMap <- function(name) {
                          if(name == "ok")
                            list("gtk-ok", GtkResponseType["ok"])
                          else if(name =="yes")
                            list("gtk-yes", GtkResponseType["ok"])
                          else if(name == "cancel")
                            list("gtk-cancel", GtkResponseType["cancel"])
                          else if(name == "close")
                            list("gtk-close", GtkResponseType["close"])
                          else if(name =="no")
                            list("gtk-no", GtkResponseType["cancel"])
                          else
                            list("gtk-yes", GtkResponseType["ok"])
                        }
                        
                        l <- list(title=title, parent=.parent, flags=c("modal"), show=FALSE)
                        for(i in buttons) {
                          m <- buttonMap(i)
                          l[[length(l) + 1]] <- m[[1]]
                          l[[length(l) + 1]] <- m[[2]]
                        }
                        
                        ## do buttons?
                        if(do.buttons) {
                          dlg <- do.call("gtkDialog", l)
                        } else {
                          dlg <- gtkDialogNew(show=FALSE)
                          ## hide separator and button box. Uses internals -- bad idea if widget changes
                          sapply(dlg$getChildren()[[1]]$getChildren(), gtkWidgetHide)
                        }
                        dlg$setTransientFor(.parent)
                        
                        dlg$SetTitle(title)
                        dlg$setDefaultResponse(GtkResponseType["ok"])
                        dlg$getVbox()$PackStart(widget)
                        dlg$grabFocus()
                        
                                
                        initFields(block=dlg,
                                   handler=handler,
                                   action=action
                                   )

                        callSuper(toolkit)
                      },
                      add_child=function(child, ...) {
                        widget$packStart(getBlock(child), expand=TRUE, fill=TRUE)
                        child_bookkeeping(child)
                      },
                      dispose=function() {
                        block$destroy()
                      },
                      set_visible=function(...) {
                        block$show()
                        response <- block$run()

                        h <- list(obj=.self, action=action)
                        if(response == GtkResponseType["cancel"] ||
                           response == GtkResponseType["close"] ||
                           response == GtkResponseType["delete-event"]) {
                          ## cancel action
                          ret <- FALSE
                        } else if(response == GtkResponseType["ok"]) {
                          if(!is.null(handler))
                            handler(h)
                          ret <- TRUE              # was widget, but TRUE now
                        } else if(response == GtkResponseType["delete-event"]) {
                          ## window manager close
                          ret <- FALSE
                        } else if(response == GtkResponseType["none"]) {
                          ## dispose() call
                          ret <- FALSE
                        } else {
                          ret <- FALSE
                        }
                        block$Destroy()
                        return(invisible(ret))
                      }
                      ))


##' toolkit implementation of galert
##'
##' @param delay delay
##' @inheritParams gWidgets2::gaction
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .galert guiWidgetsToolkittcltk
##' @S3method .galert guiWidgetsToolkittcltk
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
 
