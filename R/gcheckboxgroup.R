##' @include GWidget.R
NULL

##' Toolkit  constructor
##'
##' @inheritParams gWidgets2::gcheckboxgroup
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gcheckboxgroup guiWidgetsToolkittcltk
##' @S3method .gcheckboxgroup guiWidgetsToolkittcltk
.gcheckboxgroup.guiWidgetsToolkittcltk <-  function(toolkit=NULL,
                                                    items, checked = FALSE, horizontal = FALSE,
                                                    use.table=FALSE, handler = NULL,
                                                    action = NULL, container = NULL, ... ) {
  if(use.table)
    GCheckboxGroupTable$new(toolkit, items, checked = checked,
                            handler = handler,action = action,  container = container, ...)
  else
    GCheckboxGroup$new(toolkit,
                       items, checked = checked, horizontal = horizontal,
                       handler = handler, action = action, container = container, ...)
}


GCheckboxGroup <- setRefClass("GCheckboxGroup",
                      contains="GWidgetWithItems",
                      fields=list(
                        items="character" # label names
                        ),
                      methods=list(
                        initialize=function(toolkit, items, checked, horizontal,
                          handler, action, container, ...) {
                          widgets <<- list()
                          
                          widget <<- ttkframe(container$get_widget(), padding=c(2,2,2,2))

                          initFields(block=widget,
                                     horizontal=horizontal,
                                     change_signal="command"
                                     )
                          
                          set_items(items)
                          set_value(as.logical(checked))
                          
                          add_to_parent(container, .self, ...)
                          
                          handler_id <<- add_handler_changed(handler, action)
                          
                          callSuper(toolkit)
                        },
                        get_value=function(drop=TRUE, ...) {
                          ## return names
                          ind <- sapply(seq_along(items), function(i) {
                            val <- tclvalue(widgets[[i]]$var)
                            as.logical(as.numeric(val))
                          })
                          items[ind]
                        },
                        set_value=function(value, drop=TRUE, ...) {
                          ## value is name or logical
                          if(is.logical(value)) {
                            value <- rep(value, length=get_length()) # recycle
                            sapply(seq_along(items), function(i) {
                              text_var <- widgets[[i]]$var
                              tclvalue(text_var) <- value[i]
                            })
                          } else {
                            set_index(match(value, items))
                          }
                        },
                        get_index = function(...) {
                          match(get_value(), items)
                        },
                        set_index = function(value, ...) {
                          ## set by index
                          val <- rep(FALSE, get_length())
                          val[value] <- TRUE
                          set_value(val)
                        },
                        get_items = function(i, ...) {
                          items[i]
                        },
                        set_items = function(value, i, ...) {
                          ## make widgets
                          if(missing(i)) {
                            ## remove old
                            sapply(widgets, function(i) tkpack.forget(i$button))
                            tclServiceMode(FALSE)
                            ## new values
                            values <- unique(value) # make unique
                            items <<- values
                            widgets <<- lapply(items, function(i) {
                              text_var <- tclVar(0) # 0 unchecked
                              btn <- ttkcheckbutton(widget, text=as.character(i),
                                                    variable=text_var)
                              ## configure call back
                              tkconfigure(btn, command=function() {
                                notify_observers(signal=change_signal)
                              })
                              if(horizontal)
                                tkpack(btn, padx=2, side="left", anchor="w")
                              else
                                tkpack(btn, pady=2, side="top", anchor="w")
                              list(button=btn, var=text_var)
                            })
                            tclServiceMode(TRUE)
                          } else {
                            ## update label values for i
                            if(min(i) < 1 || max(i) > get_length()) {
                              
                            }

                            
                            items[i] <<- as.character(value)
                            sapply(i, function(ind) {
                              btn <- widgets[[i]]$button
                              tkconfigure(btn, text=items[i])
                            })
                          }
                          invisible()
                        },
                        get_length=function() base::length(items),
                        set_icons=function(value, i, compound) {
                          "Set icon, compound a value in 'text', 'image','center','top','bottom','left','right','none'"
                          if(missing(i))
                            i <- seq_along(items)
                          if(missing(compound))
                            compound <- "left"
                          sapply(i, function(j) {
                            tkconfigure(widgets[[j]], image=value[j], compound=match.arg(compound))
                          })
                        }
                        ))

