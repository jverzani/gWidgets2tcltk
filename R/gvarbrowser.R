##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gvarbrowser
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gvarbrowser guiWidgetsToolkittcltk
##' @S3method .gvarbrowser guiWidgetsToolkittcltk
.gvarbrowser.guiWidgetsToolkittcltk <-  function(toolkit,
                                                 handler = NULL,action = "summary", container = NULL, ... ) {
  GVarBrowser$new(toolkit,
                  handler = handler,action = action, container = container, ...)
}

## S3 method. Could be in gWidgets2...
description <- function(x) UseMethod("description")
description.default <- function(x) sprintf("%s object", class(x)[1])
description.numeric <- function(x) sprintf("Numeric object of length %s", length(x))
description.integer <- function(x) sprintf("Integer of length %s", length(x))
description.character <- function(x) sprintf("Character of length %s", length(x))
description.logical <- function(x) sprintf("Logical of length %s", length(x))

description.data.frame <- function(x) sprintf("Data frame %s by %s", nrow(x), ncol(x))
description.matrix <- function(x) sprintf("Matrix %s by %s", nrow(x), ncol(x))
description.list <- function(x) sprintf("List with %s component%s", length(x), ifelse(length(x) == 1, "", "s"))

description.function <- function(x) sprintf("Function")
description.lm <- function(x) sprintf("Model object")


## icon
tk_icon <- function(x) UseMethod("tk_icon")
tk_icon.default <- function(x) "Put image here"


## Class for variable browser.
GVarBrowser <- setRefClass("GVarBrowser",
                            contains="GWidget",
                          fields=list(
                             "ws_model"="ANY",
                             "icon_classes"="list",
                             "timer"= "ANY"
                             ),
                            methods=list(
                              initialize=function(toolkit=NULL,
                                handler=NULL, action=NULL, container=NULL, ..., fill=NULL) {

                                ws_model <<- gWidgets2:::WSWatcherModel$new()
                                o = gWidgets2:::Observer$new(function(self) {self$update_view()}, obj=.self)
                                ws_model$add_observer(o)


                                init_widget(container$get_widget())
                                set_selection_mode("extended")

                                
                                
                                ## set up drag source
#                                add_drop_source(function(h,...) {
#                                  path <- h$drag_data
#                                  paste(path, collapse="$")
#                                })

                                
                                icon_classes <<- getWithDefault(getOption("gwidgets2:gvarbrowser_classes"),
                                                                gWidgets2:::gvarbrowser_default_classes)
                                
                                add_context_menu()


                                
                                add_to_parent(container, .self, ..., fill=fill)

                                handler_id <<- add_handler_changed(handler, action)

                                ## Try our own timer. Need to check in update view the size and slow down if too large
                                timer <<- gtimer(1000, function(...) {.self$ws_model$update_state()})
                                
                           populate_view() # initial

                                
                                callSuper(toolkit)
                              },
                              init_widget=function(parent, ...) {
                                block <<- ttkframe(parent)
                                
                                widget <<- ttktreeview(block,
                                                      show="tree headings",
                                                      displaycolumns="#all",
                                                      columns=1
                                                      )
                                
                                xscr <- ttkscrollbar(block, orient="horizontal",
                                                     command=function(...) tkxview(widget,...))
                                yscr <- ttkscrollbar(block, orient="vertical",
                                                     command=function(...) tkyview(widget,...))
                                
                                tkconfigure(widget,
                                            xscrollcommand=function(...) tkset(xscr,...),
                                            yscrollcommand=function(...) tkset(yscr,...))
                                
                                tkgrid(widget, row=0, column=0, sticky="news")
                                tkgrid(yscr, row=0, column=1, sticky="ns")
                                tkgrid(xscr, row=1, column=0, sticky="ew")
                                tkgrid.columnconfigure(block, 0, weight=1)
                                tkgrid.rowconfigure(block, 0, weight=1)

                                
                                ## we have columns var, component and description
                                tcl(widget, "column", "#0", width=200L, anchor="w", stretch=TRUE)
                                tcl(widget, "column", 1, stretch=TRUE)
                                f <- function(col, value) tcl(widget, "heading", col, text=value)
                                mapply(f, c("#0", 1), gettext(c("Variable", "Description")))
                                
                                
                              },
                              start_timer=function() {
                                timer$start_timer()
                              },
                              stop_timer=function() timer$stop_timer(),
                              adjust_timer=function(ms) {
                                "Adjust interval to size of workspace"
                                if(missing(ms)) {
                                  n <- length(ls(envir=.GlobalEnv))
                                  ms <- 1000 * floor(log(5 + n, 5))
                                }
                                timer$set_interval(ms)
                              },
                              set_icon_classes=function(icon_classes) {
                                icon_classes <<- icon_classes
                                populate_view()
                              },
                              get_children=function(node) {
                                "Return all children of the node"
                                as.character(tcl(widget, "children", node))
                              },
                              sort_node=function(node, decreasing=FALSE) {
                                "Sort children of node"
                                kids <- get_children(node)
                                vals <- sapply(kids, function(i) tclvalue(tcl(widget, "item", i, "-text")))
                                idx <- order(vals)
                                if(!decreasing)
                                  idx <- rev(idx)
                                sapply(kids[idx], function(i) tcl(widget, "move", i, node, 0)) # move to top
                              },
                              set_selection_mode=function(mode=c("none", "browse", "extended")) {
                                "Helper: Set the selection mode"
                                tkconfigure(widget, selectmode=match.arg(mode))
                              },
                              
                              clear_items=function() {
                                "Clear out tree"
                                 kids <- get_children("")
                                sapply(kids, function(i) tcl(widget, "delete", i))
                              },
                              
                              add_item=function(varname, x, node) {
                                "Add an item to tree at node"
                                descr <- description(x)
                                id <- tcl(widget, "insert", node, "end", text=varname, values=sprintf("{%s}",descr))
                                if(is.recursive(x) && !is.null(names(x)))
                                  sapply(names(x), function(comp) .self$add_component(comp, x, id))
                              },
                              add_component=function(varname, x, node) {
                                "Add components to an item"
                                y <- x[[varname]]
                                descr <- description(y)
                                id <- tcl(widget, "insert", node, "end", text=varname, values=sprintf("{%s}",descr))
                              },
                             populate_view=function() {
                                "Populate view based on icon_classes"
                                clear_items()
                                for(i in names(icon_classes)) {
                                  ## add header to root
                                  node <- ""
                                  node <- tcl(widget, "insert", node, "end", text=i, values="")
                                  
                                  items <- ws_model$get_by_class(icon_classes[[i]])
                                  if(length(items))
                                    mapply(.self$add_item, names(items), items, list(node))
                                }
                              },
                              update_view=function() {
                                ## names, need to put into icon classes
                                stop_timer()
                                on.exit({adjust_timer(); start_timer()})
                                
                                update_section <- function(node, nm) {
                                  ## update a section given by `nm`
                                  objs <- ws_model$get_by_class(icon_classes[[nm, exact=TRUE]])
                                  nms <- names(objs)
                                  
                                  kids <- get_children(node)
                                  for(id in kids) {
                                    ## from tree
                                    key <- as.character(tcl(widget, "item", id, "-text"))
                                    val <- tclvalue(tcl(widget, "item", id, "-values"))
                                    
                                    ## from object
                                    desc <- description(objs[[key]])
                                    
                                    old_tag <- paste(key, val, sep="") 
                                    new_tag <- paste(key, "{", desc, "}", sep="")
                                    
                                    if(!key %in% names(objs)) {
                                      tcl(widget, "delete", id)
                                      
                                    } else {
                                      if(new_tag !=  old_tag) {
                                        
                                        x <- get(key, .GlobalEnv)
                                        descr <- description(x)
                                        tcl(widget, "item", id, values=sprintf("{%s}", descr))
                                        
                                      }
                                      nms <- setdiff(nms, key)        #update nms
                                    }
                                  }
                                  
                                  if(length(nms)) {
                                    ## add nms
                                    for(nm in nms) {
                                      descr <- description(objs[[nm, exact=TRUE]])
                                      add_item(nm, objs[[nm, exact=TRUE]], node)
                                    }
                                    sort_node(node)
                                  }
                                }
                                
                                ## Now apply to each
                                child_nodes <- get_children("")
                                topics <- sapply(child_nodes, function(id) tclvalue(tcl(widget, "item", id, "-text")))
                                mapply(update_section, child_nodes, topics)
                              },
                              get_object_name=function(node, collapse="$") {
                                "Helper. March back tree to get object name, past with '$' if requested"
                                path <- tclvalue(tcl(widget, "item", node, "-text"))
                                
                                node <- tclvalue(tcl(widget, "parent", node))
                                while(node != "") {
                                  path <- c(tclvalue(tcl(widget, "item", node, "-text")), path)
                                  node <- tclvalue(tcl(widget, "parent", node))
                                }
                                path <- path[-1] # drop initial
                                if(is.null(collapse))
                                  path
                                else
                                  paste(path, collapse=collapse)
                              },
                              get_value=function(drop=TRUE, ...) {
                                "Get selected values as names. A value may be 'name' or 'lst$name1$name2'"
                                sel <- as.character(tcl(widget,"selection"))
                                if(is.null(sel))
                                  return(integer(0))

                                if(is.null(drop)) drop <- TRUE
                                lst <- lapply(sel, .self$get_object_name, collapse=ifelse(drop, "$", NULL))
                                lst <- Filter(function(i) !(length(i) == 0 || i == ""), lst)
                                if(length(lst) == 1)
                                  lst[[1]]
                                else
                                  lst
                              },

                              set_value=function(value, ...) {
                                "Select and open value given."
                              },
                              ## context menu popup
                              add_context_menu=function() {
                                return()
                                ## XXX update
                                ## make context sensitive menu. Requires identifying value of selected
                               
                              },
                              add_handler_changed=function(handler, action=NULL,...) {
                                add_handler("<Double-ButtonPress-1>", handler, action=NULL, ...)
                              },
                              add_handler_selection_changed=function(handler, action=NULL, ...) {
                                add_handler("<<TreeviewSelect>>", handler, action, ...)
                              }
                              ))
