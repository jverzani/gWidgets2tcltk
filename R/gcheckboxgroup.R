##' @include GWidget.R
##' @include gtable.R
NULL

##' Toolkit  constructor
##'
##' @inheritParams gWidgets2::gcheckboxgroup
##' @export
##' @usage \method{.gcheckboxgroup}{guiWidgetsToolkittcltk}(toolkit=NULL, items, checked = FALSE,horizontal = FALSE,
##'                 use.table = FALSE, handler = NULL, action = NULL, container = NULL, ...)
##' @rdname gWidgets2tcltk-undocumented
##' @method .gcheckboxgroup guiWidgetsToolkittcltk
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
                            items <<- as.character(values)
                            
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
                              ## XXX what goes here?
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
                          compound <- match.arg(compound,
                                                c('text', 'image','center','top','bottom','left','right','none'))
                          f <- function(widget, image) tkconfigure(widget, image=image, compound=compound)
                          mapply(f, widgets, value)
                        }
                        ))



GCheckboxGroupTable <- setRefClass("GCheckboxGroupTable",
                                   contains="BaseTableClass",
                                   fields=list(
                                     ..selected="logical",
                                     on_off_icons="ANY"
                                     ),
                                   methods=list(
                                     initialize=function(toolkit,
                                       items, checked = FALSE,
                                       handler = NULL,
                                       action = NULL, container = NULL, ...,
                                       icon.col, tooltip.col # don't pass along
                                       ) {

                                       items <- data.frame(items, stringsAsFactors=FALSE)

                                       initFields(n=ncol(items),
                                                  change_signal="<Button-1>")

                                       init_widget(container$get_widget(), ...)
                                       tkconfigure(widget, columns=seq_len(n))
                                       
                                       set_selectmode("none")
                                       set_column_headings(names(items))
                                       ## add in icons
                                       configure_icon_column(50)

                                       tkimage.create("photo", "::image::off", file=system.file("images", "checkbutton-off.gif", package="gWidgets2tcltk"))
                                       tkimage.create("photo", "::image::on",  file=system.file("images", "checkbutton-on.gif",  package="gWidgets2tcltk"))

                                       icons <- paste("::image::", c("on","off"), sep="")
                                       class(icons) <- "StockIcon"
                                       on_off_icons <<- icons
                                       
                                       set_DF(items)
                                       set_icons()
                                       
                                       toggle_handler <- function(W, x, y) {
                                         row <- as.character(tcl(W, "identify", "row", x, y))
                                         ind <- match(row, child_ids)
                                         if(length(ind)) {
                                           toggle_state(ind)
                                           invoke_change_handler()
                                         }

                                       }
                                       tkbind(widget, "<Button-1>", toggle_handler)

                                       ## use header to toggle
                                       tcl(widget, "heading", "#0", command=function() {
                                         ind <- seq_len(get_length())
                                         toggle_state(ind)
                                       })

                                       
                                       ## need to turn on selection to get Return key

                                       set_index(checked)
                                       add_to_parent(container, .self, ...)

                                       handler_id <<- add_handler_changed(handler, action)


                                       
                                       callSuper(...)
                                     },
                                     add_handler_changed=function(handler, action=NULL, ...) {
                                       if(is_handler(handler)) {

                                         o <- gWidgets2:::observer(.self, handler, action)
                                         invisible(add_observer(o, change_signal))
                                       }

                                     },
                                     set_DF=function(items) {
                                       items <- data.frame(items, stringsAsFactors=FALSE)
                                       ..selected <<- rep(TRUE, nrow(items))

                                       callSuper(items)
                                     },
                                     set_icons=function() {
                                       "Set icons by selected"
                                       icons <- on_off_icons[2 - as.numeric(..selected)]
                                       class(icons) <- "StockIcon"
                                       callSuper(icons)
                                     },
                                     toggle_state=function(ind) {
                                       "Toggle state after click"
                                       ..selected[ind] <<- !..selected[ind]
                                       sapply(ind, function(i) {
                                         tcl(widget, "item", child_ids[i], image=on_off_icons[2 - as.numeric(..selected[i])])
                                       })
                                      
                                     },
                                     ### gWidgets inteface
                                     get_value=function(...) {
                                       get_items()[get_index()]
                                     },
                                     get_index=function(...) {
                                       which(..selected)
                                     },
                                     set_value=function(value, ...) {
                                       set_index(match(value, get_items()))
                                     },
                                     set_index=function(value, ...) {
                                       vals <- rep(FALSE, get_length())
                                       vals[value] <- TRUE
                                       ..selected <<- vals
                                       set_icons()
                                       invoke_change_handler()
                                     },
                                     get_items=function(i, ...) {
                                       get_data()[i,1]
                                     },
                                     set_items=function(value, i, ...) {
                                       if(!missing(i)) {
                                         message("Can only reset all items")
                                         return()
                                       }
                                       set_DF(value)
                                       cur_width = as.numeric(tkwinfo("width", widget))
                                       tcl(widget, "column", ncol(value),  width=cur_width- 50, stretch=TRUE)
                                       set_index(integer(0))
                                     },
                                     get_length=function(...) {
                                       nrow(DF)
                                     }
                                     ))
