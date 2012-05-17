##' @include GWidget.R
NULL

##' Toolkit label constructor
##'
##' @inheritParams gWidgets2::glabel
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .glabel guiWidgetsToolkittcltk
##' @S3method .glabel guiWidgetsToolkittcltk
.glabel.guiWidgetsToolkittcltk <- function(toolkit, text="", markup=FALSE, editable=FALSE,
                                           handler=NULL, action=NULL, container=NULL,
                                           ...) {
  GLabel$new(toolkit, text, markup, editable, handler, action, container, ...)
}

##' label class for tcltk
##'
##' @rdname gWidgets2tcltk-package
GLabel <- setRefClass("GLabel",
                            contains="GWidgetWithTclVariable",
                            fields=list(
                              markup="ANY",
                              editable="logical",
                              edit_widget = "ANY",
                              state="character"
                              ),
                            methods=list(

                              
                              initialize=function(toolkit=NULL, text, markup=FALSE, editable=FALSE, handler, action, container, ...) {

                                ## no markup
                                if(markup) {
                                  message("No markup is available for labels in tcltk. Use font<- instead. Stripping tags.")
                                  text <-  gsub("<[^>]*>","",text)
                                }

                                t_var <<- tclVar("")
                                widget <<- ttklabel(container$get_widget(), textvariable=t_var)
                                initFields(block=widget)
                                
                                add_to_parent(container, .self, ...)

                                set_value(text)

                                if(editable) {
                                  ## overwrite any handler
                                  handler <- function(h,...) {
                                    val <- ginput(msg=gettext("Change label value:"),
                                                  text=svalue(h$obj),
                                                  title="Change text for label",
                                                  icon="question",
                                                  parent=getTopLevel(h$obj))
                                    if(!is.na(val))
                                      set_value(val)
                                  }
                                  action <- NULL
                                }

                                add_handler_clicked(handler, action=action)

                                callSuper(toolkit)
                              }

                              
                              ))

