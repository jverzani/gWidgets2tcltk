##' @include GContainer.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::glayout
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .glayout guiWidgetsToolkittcltk
##' @S3method .glayout guiWidgetsToolkittcltk
.glayout.guiWidgetsToolkittcltk <-  function(toolkit,
                                             homogeneous = FALSE, spacing = 10,
                                             container = NULL, ... ) {
  GLayout$new(toolkit=toolkit, homogeneous=homogeneous, spacing=spacing, container = container, ...)
}


## layout class
GLayout <- setRefClass("GLayout",
                       contains="GContainer",
                       fields=list(
                         child_positions="list",
                         homogeneous="logical",
                         spacing="numeric",
                         adjust="ANY"
                         ),
                       methods=list(
                         initialize=function(toolkit=NULL,
                           homogeneous = FALSE, spacing = 10,
                           container = NULL, ... 
                           ) {
                          
                           widget <<- ttkframe(container$get_widget())


                           initFields(block=widget,
                                      homogeneous=homogeneous,
                                      spacing=rep(spacing, length=2), # can put asymmetric spacing
                                      adjust="center",
                                      children=list())
                                      
                           add_to_parent(container, .self, ...)

                           callSuper(toolkit)
                         },
                         add_child=function(child, expand=FALSE, fill=FALSE, anchor=c(-1,0), ...) {
                           ## we don't add child, but do the bookkeeping here
                           children <<- c(children, child)
                         },
                         remove_child=function(child) {
                           ## remove from GUI
                           tkgrid.forget(child$get_block())
                           ## remove from children list
                           children <<- Filter(function(i) !identical(i, child), children)
                         },
                         get_dim=function(...) {
                           "current size of table"
                           d <- rev(as.numeric(tcl("grid","size", widget)))
                           setNames(d, c("nrow", "ncol"))
                           d
                         },
                         get_items = function(i, j, ..., drop=TRUE) {


                           ## make matrix, then extract
                           d <- get_dim()
                           m <- matrix(nrow=d[1], ncol=d[2])
                           for(index in seq_along(child_positions)) {
                             item <- child_positions[[index]] 
                             for(ii in item$x)
                               for(jj in item$y) {
                                 m[ii,jj] <- index
                               }
                           }
                           widgets <- sapply(as.vector(m), function(ii) {
                             if(is.na(ii))
                               NA
                             else
                               child_positions[[ii]]$child
                           })
                           widgets <- matrix(widgets, ncol=d[2])
                           out <- widgets[i,j, drop=drop]
                           if(length(out) == 1)
                             out <- out[[1]]
                           out
                         },
                         set_items = function(value, i, j, expand=FALSE, fill=FALSE, anchor=NULL) {
                           "Main method to add children"

                           if(missing(j)) {
                             cat(gettext("glayout: [ needs to have a column specified."))
                             return()
                           }

                           if(missing(i))
                             i <- get_dim()[1] + 1
                           
                           if(is.character(value)) {
                             value <- GLabel$new(value, container=.self, toolkit=toolkit)
                           }

                           expand <- getWithDefault(expand, getWithDefault(child$default_expand, FALSE))
                           fill <- getWithDefault(fill, getWithDefault(child$default_fill, FALSE))
                           
                           ## widgets
                           child <- getBlock(value)

                           ## need means to adjust via sticky
                           ## fill value takes precedence over anchor here
                           anchor <- getWithDefault(anchor, c(-1,0))

                           sticky <- "w"                # like others
                           if(anchor[1] == -1)
                             sticky = "w"
                           else if(anchor[1] == 1)
                             sticky = "e"
                           else if(anchor[2] == -1)
                             sticky = "s"

                           
                           fill <- getWithDefault(fill, "none")
                           if(is.logical(fill)) fill <- ifelse(fill, "both", "none")
                           fill <- c("none"="", "x"="ew", "y"="ns", both="news")[fill]

                           if(fill != "")
                             sticky <- fill

                           tkgrid(child,
                                  row = min(i) - 1,
                                  rowspan = 1 + max(i) - min(i),
                                  column = min(j) - 1,
                                  columnspan = 1 + max(j) - min(j),
                                  sticky = sticky,
                                  padx=spacing[1], pady=spacing[2]
                                  )

                           expand <- getWithDefault(expand, default=homogeneous)
                           weight <- as.numeric(expand)
                           lapply( (min(i):max(i)), function(row) {
                             set_row_weight(row, weight)
                           })
                           lapply( (min(j):max(j)), function(col) {
                             set_column_weight(col, weight)
                           })

                           
                           ## Internal bookkeeping, add to lists
                           if(is(value, "GComponent"))
                             value$set_parent(.self)
                           children <<- c(children, value)
                           ## store for [ method
                           l <- child_positions
                           l[[as.character(length(l) + 1)]] <- list(x=i, y=j, child=value)
                           child_positions <<- l
                         },
                         set_row_weight=function(i, weight) {
                           "adjust weight for row i"
                           f <- function(row, weight) tkgrid.rowconfigure(widget, row-1, weight=weight)
                           mapply(f, i, weight)
                         },
                         set_column_weight=function(j, weight) {
                           f <- function(col, weight) tkgrid.columnconfigure(widget, col-1, weight=weight)
                           mapply(f, j, weight)
                         }
                         ))

