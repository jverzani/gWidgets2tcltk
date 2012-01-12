##' @include GWidget.R
NULL

## TODO: XXX drophandler -- doubles up!

##' Toolkit gedit constructor
##'
##' @param initial.msg initial.msg
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gedit guiWidgetsToolkittcltk
##' @S3method .gedit guiWidgetsToolkittcltk
.gedit.guiWidgetsToolkittcltk <-  function(toolkit,
                                           text = "", width = 25, coerce.with = NULL, initial.msg=initial.msg,
                    handler = NULL,action = NULL, container = NULL, ... ) {
  GEdit$new( toolkit, text = text, width = width, coerce.with = coerce.with, initial.msg=initial.msg,
                    handler = handler,action = action, container = container, ...)
}


##' The GEdit class adds some methods beyond the spec: \code{set_error}, \code{clear_error}, \code{validate_value}
GEdit <- setRefClass("GEdit",
                            contains="GWidgetWithTclVariable",
                            fields=list(
                              init_msg="character",
                              init_msg_flag="logical",
                              completion="ANY",
                              validator="ANY"
                              ),
                            methods=list(
                              initialize=function( toolkit=NULL,
                                text = "", width = 25, coerce.with = NULL,
                                initial.msg="",
                                handler = NULL, action = NULL, container = NULL, ...) {

                                t_var <<- tclVar(text)
                                print(list("entry", container$get_widget(), t_var))
                                widget <<- ttkentry(container$get_widget(),
                                                    textvariable=t_var)

                                initFields(block=widget,
                                           init_msg=initial.msg,
                                           init_msg_flag=FALSE,
                                           change_signal="<<Changed>>"
                                           )
                                set_coerce_with(coerce.with)
                                
                                ## initFields(block=widget,
                                ##            coerce_with=coerce.with,
                                ##            init_msg=initial.msg,
                                ##            init_msg_flag=FALSE,
                                ##            completion=NULL,
                                ##            validator=NULL,
                                ##            change_signal="activate"
                                ##            )

                                ## overwrite?
                                if(nchar(text) > 0) {
                                  set_value(text)
                                } else if(nchar(initial.msg) > 0) {
                                  set_init_txt(initial.msg)
                                  tkbind(wiget, "<FocusIn>", clear_init_txt)
                                }

                                
                                add_to_parent(container, .self, ...)

                                
                                handler_id <<- add_handler_changed(handler, action)

                                callSuper(toolkit)
                              },
                              set_value=function(value,  drop=TRUE, ...) {
                                clear_init_txt()
                                callSuper(value)
                              },
                              get_value=function(...) {
                                if(!init_msg_flag)
                                  callSuper(...)
                                else
                                  ""
                              },
                              ## initial text until widge has focus
                              set_init_txt=function(msg) {
                                "set initial text, gray out"
                                tkconfigure(widget, foreground="gray")
                                set_value(msg)
                                init_msg_flag <<- TRUE
                              },
                              clear_init_txt=function() {
                                "clear out init text, set back to black"
                                 tkconfigure(widget, foreground="black")
                                if(init_msg_flag)
                                  widget$setText("")
                                init_msg_flag <<- FALSE
                              },
                              ## type ahead
                              get_items=function(i, j, ..., drop=TRUE) {
                                "i for index"
                              },
                              set_items=function(value, i, j, ...) {
                              },
                              get_visible = function() {
                                ## visibility is whether password character is being used
                                as.character(tkcget(e, "-show")) != "*"
                              },

                              set_visible = function(value) {
                                tkconfigure(widget, show=ifelse(value, "", "*"))
                              },

                              get_editable=function() {
                                "Can we actually edit widget?"
                                as.character(tkcget(widget, "-state")) == "normal"
                              },
                              set_editable = function(value, j) {
                                tkconfigure(widget, state=ifelse(value, "readonly", "normal"))
                              },
                              ## Handler: changed -> clicked
                              ## add_handler_changed = function(handler, action=NULL, ...) {
                              ##   if(missing(handler) || is.null(handler))
                              ##     return()
                              ##   f <- function(h, widget, event, ...) {
                              ##     keyval <- event$GetKeyval()
                              ##     if(keyval == GDK_Return) {
                              ##       handler(h, widget, event, ...)
                              ##       return(TRUE)
                              ##     } else {
                              ##       return(FALSE)
                              ##     }
                              ##   }
                              ##   add_handler("activate", f, action=action, ...)
                              ## },
                             

                              ## Extra methods
                              set_validator = function(FUN) {
                                "Set a function to do the validation"
                                validator <<- FUN
                              },
                              validate_input = function() {
                                "Return logical indicating if input is valid"
                                if(is.null(validator))
                                  TRUE
                                else 
                                  validator(get_value())
                              },
                              set_error = function(msg) {
                                "Add error state and message to widget"
                              },
                              clear_error = function() {
                                "Clear error message"
                              }
                              ))

