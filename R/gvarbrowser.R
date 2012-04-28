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



## icon
tk_icon <- function(x) UseMethod("tk_icon")
tk_icon.default <- function(x) "Put image here"


## Class for variable browser.
## supports filtering too, see set_filter_name
GVarBrowser <- setRefClass("GVarBrowser",
                            contains="GWidget",
                          fields=list(
                            "ws_model"="ANY",
                            "filter_classes"="list",
                            "other_label"="character",
                            "filter_name"="character",
                            "timer"= "ANY",
                            "use_timer"="logical"
                            ),
                            methods=list(
                              initialize=function(toolkit=NULL,
                                handler=NULL, action=NULL, container=NULL, ..., fill="both") {

                                ws_model <<- gWidgets2:::WSWatcherModel$new()
                                o = gWidgets2:::Observer$new(function(self) {self$update_view()}, obj=.self)
                                ws_model$add_observer(o)

                                init_widget(container$get_widget())
                                set_selection_mode("extended")

                                
                                
                                ## set up drag-n-drop source
                                add_drop_source(function(h,...) {
                                  svalue(h$obj)
                                })

                                initFields(filter_name="",
                                           other_label=gettext("Other"),
                                           filter_classes=gWidgets2:::gvarbrowser_default_classes,
                                           use_timer=TRUE
                                           )
                                
                                add_context_menu()

                                add_to_parent(container, .self, ..., fill=fill)

                                handler_id <<- add_handler_changed(handler, action)

                                ## Try our own timer. Need to check in update view the size and slow down if too large
                                timer <<- gtimer(1000, function(...) {.self$ws_model$update_state()})
                                ## clean up after death
                                tkbind(widget, "<Destroy>", function() {
                                  message("stopping timer...")
                                  timer$stop_timer()
                                })
                                
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
                                use_timer <<- TRUE
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
                              ##
                              set_filter_name=function(value) {
                                filter_name <<- as.character(value)[1]
                                populate_view()
                              },
                              set_filter_classes=function(filter_classes) {
                                filter_classes <<- filter_classes
                                populate_view()
                              },
                              ##
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
                              get_object_name=function(node) {
                                "Helper. March back tree to get object named"
                                path <- tclvalue(tcl(widget, "item", node, "-text"))
                                
                                node <- tclvalue(tcl(widget, "parent", node))
                                while(node != "") {
                                  path <- c(tclvalue(tcl(widget, "item", node, "-text")), path)
                                  node <- tclvalue(tcl(widget, "parent", node))
                                }
                                if(nchar(filter_name) == 0)
                                  path <- path[-1] # drop initial
                                return(path)
                              },
                              ## tree methods
                              clear_items=function() {
                                "Clear out tree"
                                 kids <- get_children("")
                                sapply(kids, function(i) tcl(widget, "delete", i))
                              },
                              set_selection_mode=function(mode=c("none", "browse", "extended")) {
                                "Helper: Set the selection mode"
                                tkconfigure(widget, selectmode=match.arg(mode))
                              },
                              ## add item, component, populate, update
                              add_item=function(varname, x, node) {
                                "Add an item to tree at node"
                                descr <- short_summary(x)
                                id <- tcl(widget, "insert", node, "end", text=varname, values=sprintf("{%s}",descr))
                                if(is.recursive(x) && !is.null(names(x)))
                                  sapply(names(x), function(comp) .self$add_component(comp, x, id))
                              },
                              add_component=function(varname, x, node) {
                                "Add components to an item"
                                y <- x[[varname]]
                                descr <- short_summary(y)
                                id <- tcl(widget, "insert", node, "end", text=varname, values=sprintf("{%s}",descr))
                              },
                              update_section = function(node, objs) {
                                "update a section given with objs"
                                  ## an ugly thing: removed, changed and added here

                                nms <- names(objs)
                                  
                                kids <- get_children(node)
                                ## Loop over each node of the tree
                                for(id in kids) {
                                  key <- as.character(tcl(widget, "item", id, "-text"))
                                  val <- tclvalue(tcl(widget, "item", id, "-values"))
                                  
                                  if(!key %in% names(objs)) {
                                    ## Delete if not there
                                    tcl(widget, "delete", id)
                                  } else {
                                    ## compare objects, do we update?
                                    new_val <- objs[[key]]
                                    desc <- short_summary(new_val)
                                    
                                    old_tag <- paste(key, val, sep="") 
                                    new_tag <- paste(key, "{", desc, "}", sep="")

                                    if(new_tag !=  old_tag) {
                                      descr <- short_summary(new_val)
                                      tcl(widget, "item", id, values=sprintf("{%s}", descr))
                                    }
                                    nms <- setdiff(nms, key)        #update nms
                                  }
                                }
                                ## what did we miss, not in tree so we add
                                if(length(nms)) {
                                  ## add nms
                                  for(nm in nms) {
                                    item <- objs[[nm]] #mget(nm, list2env(objs))[[1]]
                                    descr <- short_summary(item)
                                    add_item(nm, item, node)
                                  }
                                  sort_node(node)
                                }
                              },
                             populate_view=function() {
                                "Populate view based on filter_classes of filter_name"
                                clear_items()

                                ## do we do filter_classes or filter_name
                                if(nchar(filter_name) > 0) {
                                  f <- function(x) {force(filter_name); grepl(filter_name, x)}
                                  items <- ws_model$filter_names(f)
                                  node <- ""
                                  if(length(items))
                                    mapply(.self$add_item, names(items), items, list(node))
                                  
                                } else {
                                  for(i in names(filter_classes)) {
                                    ## add header to root
                                    node <- ""
                                    node <- tcl(widget, "insert", node, "end", text=i, values="")
                                    
                                    items <- ws_model$get_by_class(filter_classes[[i]])
                                    if(length(items))
                                      mapply(.self$add_item, names(items), items, list(node))
                                  }

                                  all_classes <- unlist(filter_classes)
                                  f <- function(x) !Reduce("||", sapply(all_classes, is, object=x))
                                  items <- ws_model$get_by_function(f)

                                  node <- tcl(widget, "insert", "", "end", text=other_label, values="")
                                  if(length(items))
                                    mapply(.self$add_item, names(items), items, list(node))
                                  
                                  
                                }
                              },
                              update_view=function() {
                                ## names, need to put into icon classes
                                stop_timer()
                                on.exit({
                                  if(use_timer) {
                                    adjust_timer(); start_timer()
                                  }
                                })
                                
                               
                                ## now work
                                ## filter_name or icon classes
                                if(nchar(filter_name)) {
                                  objs <- ws_model$filter_names(function(x) {
                                    force(filter_name)
                                    grepl(filter_name, x)
                                  })
                                  update_section("", objs)
                                  
                                } else {                                
                                  ## Now apply to each
                                  child_nodes <- get_children("")
                                  n <- length(child_nodes)
                                  other_node <- child_nodes[n]; child_nodes <- child_nodes[-n]
                                  
                                  topics <- sapply(child_nodes, function(id) tclvalue(tcl(widget, "item", id, "-text")))
                                  topics <- lapply(topics, function(nm) ws_model$get_by_class(filter_classes[[nm, exact=TRUE]]))
                                  mapply(update_section, child_nodes, topics)


                                  all_classes <- unlist(filter_classes)
                                  f <- function(x) !Reduce("||", sapply(all_classes, is, object=x))
                                  items <- ws_model$get_by_function(f)
                                  update_section(other_node, items)
                                }
                              },
                              ## gWidgets2 methods
                              get_value=function(drop=TRUE, ...) {
                                "Get selected values as names. For recursive object return with '$'. If drop=FALSE, return objects"
                                sel <- as.character(tcl(widget,"selection"))
                                if(is.null(sel))
                                  return(character(0))

                                objs <- lapply(sel, .self$get_object_name)
                                ## quote if needed for "$"
                                nms <- lapply(objs, function(x) {
                                  sapply(x, function(i) ifelse(grepl("\\s", i),
                                                               sprintf("'%s'", i),
                                                               i))
                                })
                                nms <- sapply(nms, paste, collapse="$", USE.NAMES=FALSE)
                                names(objs) <- nms

                                ## trim out headers, they are ""
                                nms <- Filter(nchar, nms)
                                objs <- objs[nms]

                                ## Okay, what to return, names or objects?
                                
                                if(is.null(drop) || drop) {
                                  return(nms)
                                } else {
                                  out <- lapply(objs, gWidgets2:::get_object_from_string)
                                  if(length(out) == 1) out[[1]] else out
                                }
                              },
                              set_value=function(value, ...) {
                                "Select and open value given."
                                ## no method to set the selected value. Could be useful to open categories
                              },
                              update_widget=function(...) {
                                "Update tree, by updating model and having it notify this view"
                                ws_model$update_state()
                              },
                              ## context menu popup
                              add_context_menu=function() {
                                return()
                                ## XXX update
                                ## make context sensitive menu. Requires identifying value of selected
                               
                              },
                              ## add handlers
                              add_handler_changed=function(handler, action=NULL,...) {
                                add_handler("<Double-ButtonPress-1>", handler, action=NULL, ...)
                              },
                              add_handler_selection_changed=function(handler, action=NULL, ...) {
                                add_handler("<<TreeviewSelect>>", handler, action, ...)
                              }
                              ))
