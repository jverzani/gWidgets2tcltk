##' @include GTable.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gtree
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gtree guiWidgetsToolkittcltk
##' @S3method .gtree guiWidgetsToolkittcltk
.gtree.guiWidgetsToolkittcltk <-  function(toolkit,
                                           offspring = NULL, offspring.data = NULL,
                                           chosen.col = 1, offspring.col=2, icon.col=NULL, tooltip.col=NULL,
                                           multiple = FALSE,
                                           handler = NULL,action = NULL, container = NULL, ... ) {

  
  GTree$new(toolkit,
            offspring=offspring, offspring.data=offspring.data,
            chosen.col=chosen.col, offspring.col=offspring.col, icon.col=icon.col, tooltip.col=tooltip.col,
            multiple=multiple,
            handler = handler,action = action, container = container, ...)
}


GTree <- setRefClass("GTree",
                     contains="GWidget",
                     fields=list(
                       chosen_col="ANY",
                       offspring_col="ANY",
                       icon_col="ANY",
                       tooltip_col="ANY",
                       offspring_data="ANY",
                       offspring="function",
                       no_columns="numeric",
                       multiple="logical"
                       ),
                     methods=list(
                       initialize=function(toolkit=NULL,
                         offspring = NULL, offspring.data = NULL,
                         chosen.col = 1, offspring.col=2, icon.col=NULL, tooltip.col=NULL,
                         multiple = FALSE,
                         handler=NULL, action=NULL, container=NULL, ...) {


                         init_widget(container$get_widget())
                         set_selection_mode(c("browse", "extended")[1 + as.logical(multiple)])

                         
                         
                         initFields(offspring=offspring,
                                    offspring_data=offspring.data,
                                    chosen_col=chosen.col,
                                    offspring_col=offspring.col,
                                    icon_col = icon.col,
                                    tooltip_col=tooltip.col,
                                    change_signal="<TreeviewExpand>"
                                    )


                         items <- get_offspring_icons_tooltips(c())$items
                         print(items)
                         no_columns <<- ncol(items)
                         configure_columns(items)
                         set_names(items)

                         add_offspring("", c())
                         add_tree_bindings()

                         
                         add_to_parent(container, .self, ...)
                         
                         handler_id <<- add_handler_changed(handler, action)
                         
                         callSuper(toolkit)
                       },
                       init_widget=function(parent, ...) {
                         
                         block <<- ttkframe(parent)
                         xscr <- ttkscrollbar(block, orient="horizontal",
                                              command=function(...) tkxview(widget,...))
                         yscr <- ttkscrollbar(block, orient="vertical",
                                              command=function(...) tkyview(widget,...))
                         
                         ## child needs to configure columns, displaycolumns, show
                         widget <<- ttktreeview(block,
                                                show="tree headings",
                                                displaycolumns="#all",
                                                xscrollcommand=function(...) tkset(xscr,...),
                                                yscrollcommand=function(...) tkset(yscr,...)
                                                )
                         
                         tkgrid(widget, row=0, column=0, sticky="news")
                         tkgrid(yscr, row=0, column=1, sticky="ns")
                         tkgrid(xscr, row=1, column=0, sticky="ew")
                         tkgrid.columnconfigure(block, 0, weight=1)
                         tkgrid.rowconfigure(block, 0, weight=1)
                         
                         tcl("autoscroll::autoscroll", xscr)
                         tcl("autoscroll::autoscroll", yscr)
                         
                       },
                       set_selection_mode=function(mode=c("none", "browse", "extended")) {
                         "Helper: Set the selection mode"
                         tkconfigure(widget, selectmode=match.arg(mode))
                       },
                       configure_columns=function(items) {
                         "Make columns, already got rid of non-shown columns"
                         no_cols <- ncol(items)
                         if(no_cols == 1)
                           tkconfigure(widget, columns=1)
                         else
                           tkconfigure(widget, columns=seq_len(no_cols))
                         configure_column_widths(data=items)
                        },
                       configure_column_widths=function(widths, data) {
                         "straight from GTable, subclass?"
                         if(!missing(data)) {
                           m <- gWidgets2tcltk:::gwidgets2_tcltk_format_to_char(data)
                           chars <- apply(m, 2, function(x) max(nchar(x)))
                           widths <- ceiling(1.4 * widthOfChar * pmax(4, chars))
                         }
                         if(length(widths) != no_columns) {
                           message(sprintf("Widths are not the correct length. Expecting %s, got %s", n, length(widths)))
                           return()
                         }
                         f <- function(col, width) tcl(widget, "column", col, width=width, stretch=FALSE)
                         mapply(f, seq_along(widths), widths)
                         
                         tcl(widget, "column", ncol(m), stretch=TRUE)
                         
                         ## do icon column, unlike gtable, here we want to strecth
                         tcl(widget, "column", "#0", width=50L, anchor="w", stretch=TRUE)                         
                       },
                       add_tree_bindings=function() {
                         ## Main configuration respond to open event, close event by populating 
                         tkbind(widget, "<<TreeviewOpen>>",function(W, x,y) {
                           ## selection
                           sel <- as.character(tcl(W,"selection"))
                           
                           ## check if  children, if not return
                           children <- as.character(tcl(W,"children",sel))
                           if(length(children) == 0)
                             return()

                           ## clear out any children
                           lapply(children, function(i) tcl(W,"delete",i))

                           ## add new
                           add_offspring(parent_node=sel, get_tree_path(W))
                         })
                         

                       },
                       ## tree methods
                       get_tree_path=function(tr) {
                         ## return path from selection
                         sel <- as.character(tcl(tr,"selection"))
                         
                         if(is.null(sel))
                           return(c())

                         path <- tclvalue(tcl(tr,"item",sel,"-text"))
                         tparent <- tclvalue(tcl(tr,"parent",sel))
                         while(tparent != "") {
                           cur_item <- tclvalue(tcl(tr,"item", tparent,"-text"))
                           path <- c(cur_item, path)
                           tparent <- tclvalue(tcl(tr,"parent", tparent))
                         }
                         
                         return(path)
                       },
                       get_offspring_icons_tooltips=function(path) {
                         "Return list with items, has_offspring icons, tooltip (possible NULL)"
                         
                         items <- offspring(path, offspring_data)

                         ## chosen_col
                         if(is.null(chosen_col))
                           chosen_col <<- 1
                         chosen_vals <- items[,chosen_col]
                         
                         ## offspring_column
                         if(!is.null(offspring_col)) 
                           has_offspring <- items[ ,offspring_col]
                         else
                           has_offspring <- rep(FALSE, nrow(items))
                         ## icons
                         if(!is.null(icon_col))
                            icons <- items[ , icon_col]
                         else
                           icons <- rep("", nrow(items))
                         ## tooltips
                         if(!is.null(tooltip_col))
                           tooltips <- items[, tooltip_col]
                         else
                           tooltips <- rep("", nrow(items))
                         ind <- unlist(list(chosen_col, offspring_col, icon_col, tooltip_col))
                         if(!is.null(ind))
                           items <- items[, -ind, drop=FALSE]
                         
                         return(list(items=items, chosen_vals=chosen_vals, has_offspring=has_offspring, icons=icons, tooltips=tooltips))
                       },
                       add_offspring=function(parent_node, path) {
                         ## add in children
                         lst <- get_offspring_icons_tooltips(path)
                         
                         ## fix icons - allow for stock or file or "" or null or NA
                         ## are icons "", NA, filename or stockname?
                         icons <- sapply(lst$icons, getStockIconByName)
                         items <- gwidgets2_tcltk_format_to_char(lst$items)
                         
                         for(i in seq_len(nrow(items))) 
                           configure_row(parent_node, lst$chosen_vals[i], items[i,], lst$has_offspring[i], icons[i])
                       },
                       configure_row=function(node, text, values, has_offspring, icon) {
                         id <- tcl(widget, "insert", node, "end", text=text, values=values)
                         tcl(widget, "item", id, image=icon)
                         if(has_offspring) {
                           print("add offspring")
                           tcl(widget,"insert", id, "end", text="")
                         }
                         as.character(id)
                       },
                       ## main gWidgets methods
                       get_value=function(i, drop=TRUE,...) {
                         "Return path (by chosen col)"
                         get_tree_path(widget)[i, drop=drop]
                       },
                       set_value=function(value, ...) {
                         "open path, set via match"
                         value <- unlist(value)
                         clear_selection()
                         item_id <- ""  # root node
                         get_children <- function(node) {
                           kid_ids <- as.character(tcl(widget, "children", node))
                           kid_labels <- sapply(kid_ids, function(id) as.character(tcl(widget, "item", id, "-text")))
                           return(list(ids=kid_ids, labels=kid_labels))
                         }
                           
                         for(i in value) {
                           lst <- get_children(item_id)
                           if(length(lst$ids)) {
                             index <- match(i, lst$labels)
                             if(is.na(index)) {
                               warning(gettext("No node matching values"))
                               return()
                             }
                             item_id <- lst$ids[index]
                           } else {
                             warning(gettext("No node matching index"))
                             return()
                           }
                         }
                         tcl(widget, "see", item_id)
                         tcl(widget, "selection", "set", item_id)
                         
                         
                       },
                       get_index = function(...) {
                         "get path index as integer vector"
                          ## return index from selection (see get__tree_path)
                         sel <- as.character(tcl(tr,"selection"))
                         if(is.null(sel))
                           return(integer(0))

                         path <- as.numeric(tcl(tr,"index",sel))
                         tparent <- tclvalue(tcl(tr,"parent",sel))
                         while(tparent != "") {
                           cur_index <- as.numeric(tcl(tr,"index", tparent))
                           path <- c(cur_index, path)
                           tparent <- tclvalue(tcl(tr,"parent", tparent))
                         }
                         path <- path + 1L # 1-based
                         return(path)
                       },
                       set_index = function(value,...) {
                         "open to specifed index, if possible"
                         ## value may be a list
                         value <- unlist(value)
                         clear_selection()
                         item_id <- ""  # root node
                         get_children <- function(node) as.character(tcl(widget, "children", node))
                         for(i in value) {
                           kids <- get_children(item_id)
                           if(length(kids)) {
                             item_id <- kids[i]
                           } else {
                             warning(gettext("No node matching index"))
                             return()
                           }
                         }
                         tcl(widget, "see", item_id)
                         tcl(widget, "selection", "set", item_id)
                       },
                       get_items = function(i, j, ..., drop=TRUE) {
                         "Get items in the selected row"
                       },
                       set_items = function(value, i, j, ...) {
                         stop(gettext("One sets items at construction through the x argument of offspring function"))
                       },
                       get_names=function() {
                         sapply(seq_len(no_columns), function(i) as.character(tcl(widget, "heading", i, "-text")))
                       },
                       set_names=function(items) {
                         nms <- names(items)
                         f <- function(col, value) tcl(widget, "heading", col, text=value)
                         mapply(f, seq_along(nms), nms)
                       },
                       update_widget=function(...) {
                         "Update base of widget, reopen selected paths if possible"
                         ## block_observers()
                         ## cur_sel <- get_index()
                         ## widget$collapseAll()

                         ## ## clear base
                         ## model <- widget$getModel()$getModel()
                         ## n <- model$IterNChildren(NULL)
                         ## if(n >= 1) {
                         ##   for(i in 0:(n-1)) {
                         ##     child_iter <- model$IterChildren(NULL)
                         ##     if(child_iter$retval)
                         ##       model$Remove(child_iter$iter)
                         ##   }
                         ## }
                         ## ## repopulate
                         ## items <- offspring(c(), offspring_data)
                         ## add_child_items(items, NULL)
                         ## set_index(cur_sel)
                         ## unblock_observers()                         
                       },
                       ## Some extra methods
                       clear_selection=function() {

                       }
                       ))

