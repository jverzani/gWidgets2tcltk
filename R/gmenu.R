##' @include GWidget.R
NULL

## Needs a means to remove objects when deleted from menu, or atleast check if still there?
## Need a means to append to list

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gmenu
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gmenu guiWidgetsToolkittcltk
##' @S3method .gmenu guiWidgetsToolkittcltk
.gmenu.guiWidgetsToolkittcltk <-  function(toolkit,
                                           menu.list=list(),
                                           popup=FALSE,
                                           container = NULL,
                                           ... ) {
  if(popup)
    GMenuPopup$new(toolkit, menu.list=menu.list, container=container, ...)
  else
    GMenu$new(toolkit, menu.list=menu.list, container = container, ...)
}



##' Toplevel menu bar
##'
##' The \code{GMenuBar} class provides functionality for a top-level
##' menubar. In \pkg{tlctk} the menubars can show either
##' \code{gaction} items (proxied as buttons), \code{gradio} items or
##' \code{gcheckbox} items. In the latter two cases, one uses the
##' \code{parent} argument -- not the \code{container} argument -- to
##' specify the parent container. Such items can also be shared with
##' toolbars.
##' @param ... passed to constructor
##' @examples
##' \dontrun{
##' w <- gwindow("having fun?")
##' sb <- gstatusbar("Your message here...", cont=w)
##' g <- ggroup(cont=w, horizontal=FALSE)
##' f <- function(h,...) message(h$obj$get_value())
##' 
##' l <- list(file=gaction("file", handler=function(h,...) print("file"),
##'                key.accel="<Control-x><Control-s>", parent=w),
##'           ok=gaction("ok", icon="ok", handler=function(h,...) print("ok")),
##'           radio=list(
##'             rb=gradio(state.name[1:3], parent=w, handler=function(h,...)
##'                print(h$obj$get_value()))
##'             )
##'           sep=gseparator(vertical=TRUE),
##'           ,cb=gcheckbox("really", parent=w, handler=function(h,...) print(h$obj$get_value()))
##'           )
##' mlist <- list(File=l)
##' mb <- gmenu(mlist, cont=w)
##' }
GMenuBar <- setRefClass("GMenuBar",
                     contains="GWidget",
                     fields=list(
                       menu_list="list"
                       ),
                     methods=list(
                       
                       ## add items
                       add_menu_items=function(sub_menu, items, index) {

                         sapply(seq_along(items), function(i) {
                           item <- items[[i]]
                           new_index <- c(index, i)
                           ## do dispatch based on class
                           if(is(item, "list")) {
                             ## get name by looking up and matching
                             add_submenu(sub_menu, item, nm=names(Filter(function(x) identical(x, item), items)), index=new_index)
                           } else if(is(item, "GAction")) {
                             add_gaction_menuitem(sub_menu, item, index=new_index)
                           } else if(is(item, "GSeparator")) {
                             add_gseparator_menuitem(sub_menu, item, index=new_index)
                           } else if(is(item, "GRadio")) {
                             add_radio_menuitem(sub_menu, item, index=new_index)
                           } else if(is(item, "GCheckbox")) {
                             add_checkbutton_menuitem(sub_menu, item, index=new_index)
                           }
                         })
                       },
                       add_submenu=function(parent_menu, items, nm, index) {
                         sub_menu <- tkmenu(parent_menu, tearoff=FALSE)
                         tkadd(parent_menu, "cascade", label=nm, menu=sub_menu)
                         add_menu_items(sub_menu, items, index=index)
                       },
                       add_gaction_menuitem=function(sub_menu, item, index) {
                         ## how to enable/disable, link up to action?
                         out <- tkadd(sub_menu, "command",
                                      label=item$get_value(),
                                      command=function() item$invoke_change_handler(),
                                      image=getStockIconByName(item$get_icon()),
                                      compound="left"
                                      )
                         item$add_menu_item_proxy(.self, index)
                       },
                       add_gseparator_menuitem=function(sub_menu, item, index) {
                         tkadd(sub_menu, "separator")
                       },
                       add_radio_menuitem=function(sub_menu, item, index) {
                         sapply(item$get_items(), function(i) {
                           tkadd(sub_menu, "radiobutton",
                                 label=i,
                                 variable=item$state_var,
                                 command=function() item$invoke_change_handler()
                                 )
                         })
                         item$add_menu_item_proxy(.self, index)
                       },
                       add_checkbutton_menuitem=function(sub_menu, item, index) {
                         tkadd(sub_menu, "checkbutton",
                               label=item$get_items(),
                               variable=item$t_var,
                               command=function() item$invoke_change_handler()
                               )
                         item$add_menu_item_proxy(.self, index)
                       },
                       clear_menubar=function() {
                         "Clear out menu items"
                         
                       },
                       ##
                       get_value=function( ...) {
                         menu_list
                       },
                       set_value=function(value, ...) {
                         menu_list <<- value
                         add_menu_items(widget, value, index=integer(0))
                       },
                       append_value=function(items) {
                         "Append to menu list"
                         add_menu_items(widget, items, index=length(menu_list))
                         menu_list <<- gWidgets2:::merge.list(menu_list, items)
                       },
                       ## This gives us a way to lookup and configure
                       ## menu items. We need to specify the items by
                       ## their numeric index using a heirarchy. This
                       ## is all done behind the scenes when the menu
                       ## bar is created.
                       configure_menu_item=function(index, opts) {
                         "Configure menu item. Index numeric index, opts from Tk menu page. Eg. list(label='new label')"
                         index <- index -1L # 0-based
                         pop <- function(x) x[length(x)]
                         get_menu_item <- function(id, x) {
                           id <- pop(as.character(tcl(id, "entryconfigure", x, "menu"=NULL)))
                           id
                         }
                         get_menu_id <- function(id, x) {
                           id <- get_menu_item(id, x[1])
                           if(length(x) > 1)
                             id <- get_menu_id(id, x[-1])
                           id
                         }

                         id <- widget
                         if(length(index) > 1) 
                           id <- get_menu_id(id, index[-length(index)])
                         f <- function(...) tcl(id, "entryconfigure", pop(index), ...)
                         do.call(f, opts)
                       }
                       ))




GMenu <- setRefClass("GMenu",
                     contains="GMenuBar",
                     methods=list(
                         initialize=function(toolkit=NULL,
                         menu.list=list(),
                         container=NULL, ...) {

                           
                           ## is container a window?
                           if(!is(container, "GWindow")) {
                             message(gettext("gtoolbar requires a gwindow instance as a parent container"))
                              return()
                           }
                           
                           widget <<- tkmenu(container$get_block(), tearoff=FALSE)
                           tkconfigure(container$block, menu=widget)                           

                           
                           initFields(block=widget,
                                      menu_list=list()
                                      )

                           set_value(menu.list)

                           
                           callSuper(toolkit)
                       }
                       ))

## Popup class
GMenuPopup <- setRefClass("GMenuPopup",
                            contains="GMenuBar",
                            methods=list(
                              initialize=function(toolkit=NULL,
                                menu.list=list(),
                                container=NULL,
                                ...) {

                                widget <<- tkmenu(getWidget(container), tearoff=FALSE)
                                menu_list <<- menu.list
                                add_menu_items(widget, menu.list, index=integer(0))
                                callSuper(toolkit)
                              }
                              ))

## meny proxy handler
GMenuProxy <- setRefClass("GMenuProxy",
                          fields=list(
                            proxies="ANY" # a List
                            ),
                          method=list(
                            initialize=function() {
                              proxies <<- List$new()
                            },
                            add_proxy=function(menu, id) {
                              proxies$push(list(menu=menu, id=id))
                            },
                            set_value=function(value) {
                              proxies$each(function(i, key, item) {
                                item$menu$configure_menu_item(item$id, list(label=value))
                              })
                            },
                            set_enabled=function(value) {
                              proxies$each(function(i, key, item) {
                                item$menu$configure_menu_item(item$id, list(state=ifelse(value, "normal", "disabled")))
                              })
                            },
                            set_icon=function(value) {
                              proxies$each(function(i, key, item) {
                                item$menu$configure_menu_item(item$id, list(image=getStockIconByName(value, compound="left")))
                              })
                            }
                            ))
                          

                          
                          
                            
