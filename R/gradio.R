##' @include GWidget.R
##' @include gmenu.R
NULL

##' Toolkit  constructor
##'
##' @inheritParams gWidgets2::gradio
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gradio guiWidgetsToolkittcltk
.gradio.guiWidgetsToolkittcltk <-  function(toolkit,
                                            items,selected=1, horizontal=FALSE, handler=NULL,
                                            action=NULL, container=NULL, ...
                                            ) {
  GRadio$new(toolkit, items, selected, horizontal,
               handler, action, container, ...)
}


## radio button class
GRadio <- setRefClass("GRadio",
                      contains="GWidgetWithItems",
                      fields=list(
                        items="character", # label names
                        menu_proxies="ANY"
                        ),
                      methods=list(
                        initialize=function(toolkit, items, selected, horizontal,
                          handler, action, container, parent, ...) {
                          widgets <<- list()

                          ## might have parent argument if a menuitem
                          if(!missing(parent))
                            container <- parent
                          
                          widget <<- ttkframe(container$get_widget(), padding=c(2,2,2,2))

                          initFields(block=widget,
                                     horizontal=horizontal,
                                     state_var=tclVar(items[selected]), # store label name
                                     change_signal="command",
                                     menu_proxies=GMenuProxy$new()                                     
                                     )
                          
                          set_items(items)
                          set_index(selected)

                          if(missing(parent) || is(parent, "GToolBar"))
                            add_to_parent(container, .self, ...)
                          
                          handler_id <<- add_handler_changed(handler, action)
                          
                          callSuper(toolkit)
                        },
                        get_value=function(drop=TRUE, ...) {
                          as.character(tclvalue(state_var))
                        },
                        set_value=function(value, drop=TRUE, ...) {
                          if(!value %in% items)
                            return()
                          a <- state_var
                          tclvalue(a) <- value
                          menu_proxies$set_value(value)
                        },
                        get_index = function(...) {
                          match(tclvalue(state_var), items)
                        },
                        set_index = function(value, ...) {
                          set_value(items[value])
                        },
                        set_enabled=function(value) {
                          menu_proxies$set_enabled(value)
                          callSuper(value)
                        },
                        get_items = function(i, ...) {
                          items[i]
                        },
                        set_items = function(value, i, ...) {
                          ## make widgets
                          if(missing(i)) {
                            ## remove old
                            sapply(widgets, tkpack.forget)
                            ## new values
                            values <- unique(value) # make unique
                            items <<- as.character(values)
                            tclServiceMode(FALSE)
                            widgets <<- lapply(items, function(i) {
                              btn <- ttkradiobutton(widget, variable=state_var,
                                                    text=i, value=i)
                              ## configure call back
                              tkconfigure(btn, command=function() {
                                if(as.character(tclvalue(state_var)) == tclvalue(tkcget(btn, "-text")))
                                  notify_observers(signal=change_signal)
                              })
                              if(horizontal)
                                tkpack(btn, padx=2, side="left", anchor="w")
                              else
                                tkpack(btn, pady=2, side="top", anchor="w")
                              btn
                            })
                            tclServiceMode(TRUE)                            
                            set_index(1)
                          } else {
                            ## update label values for i
                            items[i] <<- as.character(value)
                            sapply(i, function(ind) {
                              btn <- widgets[[i]]
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
                        },
                        add_menu_item_proxy=function(mb, index) {
                          menu_proxies$add_proxy(mb, index)
                        }
                        ))


