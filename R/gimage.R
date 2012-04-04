##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gimage
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gimage guiWidgetsToolkittcltk
##' @S3method .gimage guiWidgetsToolkittcltk
.gimage.guiWidgetsToolkittcltk <-  function(toolkit,
                                         filename = "", dirname = "", stock.id=NULL, size = "",
                                         handler = NULL,action = NULL, container = NULL, ... ) {
  GImage$new( toolkit,
             filename=filename, dirname=dirname, stock.id=stock.id, size=size,
             handler = handler,action = action, container = container, ...)
}


## Main class for gimage
GImage <- setRefClass("GImage",
                      contains="GWidget",
                      fields=list(
                        "image_name"="ANY" # name (filename or stock)
                        ),
                      methods=list(
                        ## handler is click handler
                        initialize=function(toolkit=NULL,
                          filename = "", dirname = "", stock.id=NULL, size = "",
                          handler=NULL, action=NULL, container=NULL, ...) {
                          
                          widget <<- ttklabel(container$get_widget(), text="", compound="image")
                          initFields(block=widget)

                          ## what kind of file is it? stock or a file
                          if(!is.null(stock.id)) {
                            set_image(getStockIconByName(stock.id))
                          } else {
                            if(dirname != "")
                              filename <- sprintf("%s%s%s", dirname, .Platform$file.sep, filename)
                            set_value(filename)
                          }
                          ## Can we find the right size

                          
                          add_to_parent(container, .self, ...)
                          handler_id <<- add_handler_clicked(handler, action)
                          callSuper(toolkit)
                        },
                        get_value=function( ...) {
                          image_name
                        },
                        set_image=function(image_id) {
                          "configure label to show widget"
                          tkconfigure(widget, image=image_id)
                        },
                        set_value=function(value, ...) {
                          if(file.exists(value)) {
                            imageID <- sprintf("gWidgets::%s", digest(value))
                            x <- try(tcl("image","create","photo", imageID, file=value), silent=TRUE)
                             if(inherits(x,"try-error")) {
                               message(gettext("Only gif and pnm files are possible in gWidgets2tcltk\n"))
                               return()
                             } 
                            set_image(imageID)
                          } else {
                            ## assume stock
                            value <- getStockIconByName(value, toolkit=toolkit)
                            set_image(value)
                          }
                          image_name <<- value
                        },
                        add_handler_changed=function(handler, action=NULL, ...) {
                          add_handler_clicked(handler, action=action, ...)
                        }
                        ))

