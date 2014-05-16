##' @include GComponent.R
NULL

##' GContainer is the base class for container objects. The main
##' method is \code{add_child}, but there is also book-keepingn code
##' to keep track of the child components of the container
##' @rdname gWidgets2tcltk-package
GContainer <- setRefClass("GContainer",
                          contains="GComponentObservable",
                          fields=list(
                            children="list",
                            ..enabled="logical"
                            ),
                          methods=list(
                            get_widget = function() {
                              "What widget do we use for the parent of the children"
                              widget
                            },
                            add=function(...) {
                              "add is just add_child"
                              add_child(...)
                            },
                            add_child = function(child, expand, fill, anchor, ...) {
                              "Add child to parent, do internal book keeping"
                            },
                            child_bookkeeping=function(child) {
                              "Update parent property of child and children property of parent container"
                              if(is(child, "GComponent"))
                                child$set_parent(.self)
                              children <<- c(children, child)
                            },
                            set_enabled=function(value) {
                              ..enabled <<- as.logical(value)
                              tclServiceMode(FALSE)
                              sapply(children, function(i) i$set_enabled(value))
                              tclServiceMode(TRUE)
                            },
                            get_enabled=function(value) {
                              if(is(..enabled, "uninitializedField"))
                                ..enabled <<- TRUE
                              if(length(..enabled) == 0)
                                TRUE
                              else
                                ..enabled
                            }
                          ))

                              



## base class for box containers. 
GBoxContainer <- setRefClass("GBoxContainer",
                             contains="GContainer",
                                            
                             fields=list(
                               horizontal="logical",
                               spacing="numeric"
                               ),
                             methods=list(
                               set_spacing=function(value) {
                                 "Spacing is padx and pady"
                                 if(length(value) == 1)
                                   spacing <<- rep(value, 2)
                                 else
                                   spacing <<- value[1:2]
                               },
                               ## Main add method
                               add_child = function(child, expand, fill, anchor, ...) {
                                 "Add child to box container. Child can be tk or GComponent. We use expand=TRUE, fill=TRUE as a default for containers, and expand=FALSE, fill=FALSE, as the default for widgets. These will usually need tweeking. The properties default_expand and default_fill allow for this."

                                 toolkit_child <- getBlock(child)

                                 side <- ifelse(horizontal, "left", "top")
                                 expand <- getWithDefault(expand, FALSE)

                                 fill_default <- getWithDefault(child$default_fill, ifelse(horizontal, "y", "x"))
                                 fill <- getWithDefault(fill, fill_default)
                                 if(is.logical(fill))
                                   fill <- ifelse(fill, "both", "none")

                                 anchor <- xyToAnchor(getWithDefault(anchor, c(-1,0)))

                                 padx <- spacing[1]
                                 pady <- spacing[2]

                                 tkpack(toolkit_child,
                                        side=side,
                                        expand=expand,
                                        fill=fill,
                                        anchor=anchor,
                                        padx=padx, pady=pady)
                                 ## anchor likes this better than
                                 ## being in the tkpack call. Go figure
                                 ## we wrap in try, as we can't query if widget
                                 ## supports an anchor argument...
                                 try(tkconfigure(toolkit_child, anchor=anchor), silent=TRUE)

                                 child_bookkeeping(child)
                               },
                               ## Remove a child from list. Can be added back in, if not garbage collected
                               remove_child = function(child) {
                                 "remove child from box container"
                                 children <<- Filter(function(x) !identical(x, child), children) # remove from list
                                 child$set_parent(NULL) # adjust child widget property
                                 tkpack.forget(getBlock(child))
                               },
                               remove_children=function() {
                                 tclServiceMode(TRUE)
                                 sapply(children, function(i) {
                                   child$set_parent(NULL)
                                   tkpack.forge(getBlock(child))
                                 })
                                 children <<- list()
                               },
                               add_spring=function() {
                                 blank_label <- ttklabel(get_widget(), text=" ")
                                 l <- list(
                                           expand=TRUE,
                                           fill=ifelse(horizontal, "x","y"),
                                           side=ifelse(horizontal, "left", "top")
                                           )
                                 l_tkpack <- function(...) tkpack(blank_label, ...)
                                 do.call(l_tkpack, l)
                               },
                               add_space=function(value) {
                                 l <- list(ttklabel(get_widget(), text=""),
                                           side=ifelse(horizontal, "left", "top")
                                           )
                                 if(horizontal)
                                   l$padx <- as.integer(value)
                                 else
                                   l$pady <- as.integer(value)

                                 do.call(tkpack, l)
                               },
                               ## [ for returning children
                               get_items = function(i, j, ..., drop=TRUE) {
                                 "Return children"
                                 out <- children[i]
                                 if(drop && length(out) == 1)
                                   out[[1]]
                                 else
                                   out
                               },
                               
                               ## svalue (borderwidth, spacing)
                               get_value=function(...) {
                                 spacing
                               },
                               set_value=function(value, ...) {
                                 "We can't really adjust spacing between children after they have been positioned."
                                 set_spacing(value)
                                 warning(gettext("In gWidgetstcltk, setting spacing value only effects children added after spacing is set"))
                               },
                               set_borderwidth=function(value, ...) {
                                 "Set borderwidth argument of parent frame"
                                 tkconfigure(widget, borderwidth=value[1])
                               }
                               
                        ))

