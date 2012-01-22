##' @include tcltk-misc.R
NULL

##' DND class for gWidgets2tcltk
##'
##' Drag and drop in \pkg{tcltk} is not supported by any underlying
##' toolkit functionality. As such, in \pkg{gWidgets2tcltk}  we roll
##' our own. The result is a little limited: a) you can't drop values
##' from other applications b) drop targets aren't allowed to be picky
##' about what they receive (not by mime-type anyways).
##'
##' To implement drag and drop, we bind to the toplevel window the
##' events: button1, motion and button1 release. The binding occurs
##' not to to widget, but to the toplevel window containing the
##' widget. This has some advantages, most imporantly it is unlikely
##' the binding will be overwritten by usual gWidgets2tcltk
##' programs. (Recall tkbind will only allow one binding per widget
##' per signal).
##'
##' The gWidgets2 way of doing drag and drop is implemented here. You
##' add a drop source with a handler that returns the value of what
##' you want to pass via the dnd process. Then you specify a widget as
##' a drop target and give a handler. This handler receives the data
##' through the \code{dropdata} component of the "\code{h}"
##' argument. Here, the value is generated when the drop occurs, not
##' when the drag initiates. Not sure this makes any difference, but
##' it might.
##' @rdname gWidgets2tcltk-package
DND <- setRefClass("DND",
                   fields=list(
                     drag_start_position="ANY",
                     drag_start_widget = "ANY",
                     last_widget="ANY",
                     dragging="ANY",
                     drag_sources="list",
                     drop_targets="list",
                     toplevels="list",
                     drag_cursor="character",
                     default_cursor="character"
                     ),
                   methods=list(
                     initialize=function( ...) {

                       initFields(
                                  drag_sources=list(),
                                  drop_targets=list(),
                                  toplevels=list(),
                                  drag_start_position=NULL,
                                  drag_start_widget=NULL,
                                  last_widget=NULL,
                                  dragging=FALSE,
                                  drag_cursor="circle",
                                  default_cursor=""
                         )

                       
                       callSuper(...)
                     },
                     add_bindings=function(obj) {
                       toplevel <- obj$get_toplevel_tk_id()
                       if(!is.null(toplevels[[toplevel]]))
                         return()

                       ## okay, not bound do it.
                       toplevels[[toplevel]] <<- TRUE
                       
                       tkbind(toplevel, "<ButtonPress-1>", function(W, x, y, X, Y) {
                         pos <- as.numeric(c(x,y))
                         cur_widget <- as.character(tkwinfo("containing", X, Y))
                         watch_drag(cur_widget, pos)
                       })

                       tkbind(toplevel, "<Motion>", function(W, x, y, X, Y) {
                         pos <- as.numeric(c(x,y))
                         cur_widget <- as.character(tkwinfo("containing", X, Y))
                         if(length(cur_widget) == 0 ||
                            !is_watching())
                           return()

                         if(is_dragging()) {
                           if(changed_widget(cur_widget)) {
                             ## how to highlight drop areas?
                             ## for some reason configuring cursor isn't working
                             if(is_droppable(last_widget))
                               tkconfigure(last_widget, state="normal")
                             if(is_droppable(cur_widget))
                               tkconfigure(cur_widget, state="active")
                             last_widget <<- cur_widget
                           }
                         } else {
                           ## are we dragging
                           if(is_draggable() && has_moved(pos)) {
                             ## set up drag
                             tkconfigure(W, cursor=drag_cursor)
                             set_dragging(cur_widget)
                           }
                         }
                       })

                       tkbind(toplevel, "<ButtonRelease-1>", function(W, x, y, X, Y) {
                         tkconfigure(W, cursor=default_cursor)
                         
                         pos <- as.numeric(c(x,y))
                         cur_widget <- as.character(tkwinfo("containing", X, Y))
                         if(length(cur_widget) && is_dragging() && is_droppable(cur_widget) && (cur_widget != drag_start_widget)) {
                           drag_widget <- get_drag_source(drag_start_widget)
                           drop_widget <- get_drop_target(cur_widget)

                           drop_data <- drag_widget$invoke_handler("<<DragRequest>>")
                           if(length(drop_data) == 1)
                             drop_data <- drop_data[[1]] # take first component
                           drop_widget$invoke_handler("<<DropEvent>>", extra_args=list(dropdata=drop_data))

                           
                         }
                         ## configure cursors
                         ## if(length(cur_widget))
                         ##   tkconfigure(cur_widget, cursor=default_cursor)
                         ## tkconfigure(last_widget, cursor=default_cursor)

                         clear_drags()
                       })
                       
                     },
                     add_drag_source=function(obj) {
                       drag_sources[[obj$get_tk_id()]] <<- obj
                       add_bindings(obj)
                     },
                     add_drop_target=function(obj) {
                       drop_targets[[obj$get_tk_id()]] <<- obj
                       add_bindings(obj)
                     },
                     get_drag_source=function(ID) {
                       drag_sources[[ID]]
                     },
                     get_drop_target=function(ID) {
                       drop_targets[[ID]]
                     },
                     is_draggable=function(ID) {
                       if(missing(ID))
                         ID <- drag_start_widget
                       ID %in% names(drag_sources)
                     },
                     set_dragging=function(ID) {
                       dragging <<- TRUE
                       tkconfigure(ID, cursor="circle")
                       last_widget <<- ID
                     },
                     has_moved=function(pos) {
                       is_watching() &&
                       max(abs(pos - drag_start_position)) > 5
                     },
                     is_droppable=function(ID) {
                       ID %in% names(drop_targets)
                     },
                     watch_drag=function(id, pos) {
                       drag_start_position <<- pos
                       drag_start_widget <<- id
                     },
                     is_watching = function() {
                       "Are we watching for a drag?"
                       !is.null(drag_start_position)
                     },
                     is_dragging=function() dragging,
                     changed_widget = function(widget) {
                       ## check for out of bounds!
                       last_widget != widget
                     },
                     clear_drags=function() {
                       drag_start_position <<- NULL
                       drag_start_widget <<- NULL
                       last_widget <<- NULL
                       dragging <<- FALSE
                     }

                     ))

## Again use memoization to implement Singleton Pattern
## We only want one application widget drag and drop.
get_dnd <- memoise(function() DND$new())
## This is a unique script global
..dnd.. <- get_dnd()
