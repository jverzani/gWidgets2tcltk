##' @include GWidget.R
NULL

## Toolkit constructor
##
## @inheritParams gWidgets2::gtoolbar
## @param ... passed to constructor
## @export
## @rdname gWidgets2tcltk-undocumented
## @method .gtoolbar guiWidgetsToolkittcltk
.gtoolbar.guiWidgetsToolkittcltk <-  function(toolkit,
                                              toolbar.list=list(),
                                              style = c("both","icons","text","both-horiz"),
                                              container = NULL,
                                              ... ) {
  GToolBar$new(toolkit,
               toolbar.list=toolbar.list, style=style,
               container=container ,...)
}


##' \code{GToolBar} is the base class for toolbars
##'
##' The toolbar is a container, so can have other widgets added to it
##' as though it were a box container. Buttons should be added as
##' action items, so that they are rendered in the proper style. Check
##' buttons should be given the argument \code{use.togglebutton}. Use
##' \code{addSpring} to right align items.
##' @param ... passed to constructor
GToolBar <- setRefClass("GToolBar",
                        contains="GBoxContainer",
                        fields=list(
                          toolbar_list="list",
                          style="character"
                          ),
                        methods=list(
                          initialize=function(toolkit=NULL,
                            toolbar.list=list(),
                            style = c("both", "icons", "text", "both-horiz"),
                            container = NULL,
                            ...) {

                            ## A toolbar maps a list to buttons
                            ## but here we make it a container

                            ## is container a window?
                            if(!is(container, "GWindow")) {
                              message(gettext("gtoolbar requires a gwindow instance as a parent container"))
                              return()
                            }

                            widget <<- ggroup(horizontal=TRUE, spacing=0, expand=TRUE, fill="x",
                                              container=container$toolbar_area)

                            initFields(block=widget,
                                       toolbar_list=list(),
                                       style=style,
                                       horizontal=TRUE
                                       )
                            set_spacing(0)
                            

                            add_toolbar_items(toolbar.list)

                            callSuper(toolkit)
                          },
                          get_widget=function() {
                            widget$widget
                          },
                          add_toolbar_items=function(items) {
                            "Map a toolbar list, a named list of gaction items or gsepartor items"
                            sapply(items, function(item) {
                              ## do dispatch based on class
                              if(is(item, "GAction"))
                                add_gaction_toolitem(item)
                              else if(is(item, "GSeparator"))
                                add_gseparator_toolitem()
                            })
                            widget$show()
                            toolbar_list <<- gWidgets2:::merge.list(toolbar_list, items)
                          },
                          add_gaction_toolitem=function(obj) {
                            "Helper to add a gaction item"
                            btn <- gbutton(action=obj, container=widget, expand=FALSE)
                            style_map <- list("both"="center", "icons"="image", "text"="text", "both-horiz"="left")
                            tkconfigure(btn$widget, compound=style_map[style], style="Toolbutton")
                          },
                          add_gseparator_toolitem=function() {
                            "Helper to add a separator"
                            gseparator(horizontal=FALSE, container=widget)
                          },
                          clear_toolbar=function() {
                            "Clear toolbar items"
                            widget$remove_children()
                          },
                          get_value=function( ...) {
                            toolbar_list
                          },
                          set_value=function(value, ...) {
                            "Clear toolbar, add anew"
                            clear_toolbar()
                            add_toolbar_items(value)
                          }
                          ))

