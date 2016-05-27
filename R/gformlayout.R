##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gformlayout
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gformlayout guiWidgetsToolkittcltk
.gformlayout.guiWidgetsToolkittcltk <-  function(toolkit,
                                             align="left",
                                             spacing=5,
                                             container = NULL, ... ) {
  GFormLayout$new(toolkit,
             align,
             spacing,
             container=container ,...)
}


## a form layout -- really just a table
GFormLayout <- setRefClass("GFormLayout",
                           contains="GContainer",
                           fields=list(
                             align="character",
                             spacing="numeric",
                             lyt="ANY"
                             ),
                           methods=list(
                             initialize=function(toolkit=NULL,
                               align="left", spacing=5,
                               container=NULL,
                               ...) {
                               
                               initFields(align=align,
                                          spacing=rep(spacing, length=2)
                                          )
                               widget <<- ttkframe(container$get_widget())
                               block <<- widget
                               
                               add_to_parent(container, .self)
                               callSuper(toolkit, ...)
                             },
                             finalize=function() {
                               ## some cases one needs to call finalize to write table (gWidgetsWWW2)
                             },
                             add_child=function(child, label="", ...) {
                               add_row(label, child, ...)
                             },
                             add_row=function(label, child, ...) {
                               "Add a row at end"
                               row <- no_rows()
                               
                               label_widget <- ttklabel(widget, text=label)
                               child_widget <- getBlock(child)
                               ## implement alignment and 
                               ifelse(align == "left", "", "") # fill style

                               tkgrid(label_widget,
                                      row=row, rowspan=1,
                                      column=0, columnspan=1,
                                      sticky=ifelse(align == "left", "w", "e"),
                                      padx=spacing[1], pady=spacing[2])

                               tkgrid(child_widget,
                                      row=row, rowspan=1,
                                      column=1, columnspan=1,
                                      sticky="we",
                                      padx=spacing[1], pady=spacing[2])

                               ## bookkeeping
                               if(is(child, "GComponent"))
                                 child$set_parent(.self)

                               nms <- names(children)
                               children <<- c(children, child)
                               names(children) <<- c(nms, label)
                             },
                             get_value=function(...) sapply(children, svalue, simplify=FALSE),
                             set_value=function(value, ...) {
                               "value a named list matching children"
                               value <- as.list(value)
                               nms <- Filter(function(i) !is.na(match(i, names(children))), names(value))
                               sapply(nms, function(nm) {
                                 obj <- children[[nm]]
                                 svalue(obj) <- value[[nm]]
                               })
                             },
                             get_dim=function() {
                               d <- rev(as.numeric(tcl("grid","size", widget)))
                               setNames(d, c("nrow", "ncol"))
                               d
                             },
                             no_rows=function() get_dim()[1],
                             ## hacky way to set label
                             set_label_font=function(row, value) {
                               "Row is row number, value is font spec"
                               l = tcl("grid", "slaves", widget, row=row-1, column=0)
                               set_font_ttk(value, l)
                             }
                             ))
                             
