##' @include GWidget.R
NULL


##' toolkit implementation of gtext
##'
##' @inheritParams gWidgets2::gtext
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gtext guiWidgetsToolkittcltk
##' @S3method .gtext guiWidgetsToolkittcltk
.gtext.guiWidgetsToolkittcltk <-  function(toolkit,
                    text = NULL, width = NULL, height = 300, font.attr = NULL,
                    wrap = TRUE,
                    handler = NULL, action = NULL, container = NULL,... ) {
  
  GText$new(toolkit,
            text = text, width = width, height = height,
            font.attr = font.attr, wrap = wrap,
            handler = handler, action = action, container = container, ...
            )

}


GText <- setRefClass("GText",
                     contains="GWidget",
                     fields=list(
                       buffer="ANY",
                       tag_table="ANY",
                       xscr="ANY",
                       yscr="ANY"
                       ),
                     methods=list(
                       initialize=function(toolkit=NULL,
                         text = NULL, width = NULL, height = 300,
                         font.attr = NULL, wrap = TRUE,
                         handler=NULL, action=NULL, container=NULL, ...) {

                         ## arguments
                         if(wrap) wrap <- "word" else wrap <- "none"

                         init_widget(container)
                         tkconfigure(widget, wrap=wrap, undo=TRUE, setgrid=FALSE)

                         initFields(change_signal="<KeyRelease>")
                         
                         ## set point
                         tkmark.set(widget,"insert","0.0")

                         ## font.attr sets text properties for entire buffer
                         if(!is.null(font.attr)) {
                           set_font(font.attr)
                         }

                         set_value(text)
                         
                         add_to_parent(container, .self, ...)
                         
                         handler_id <<- add_handler_changed(handler, action)
                         
                         callSuper(toolkit)
                       },
                       init_widget=function(container) {
                         ## set up block and widget with scrollbars
                         block <<- ttkframe(container$get_widget())
                         widget <<- tktext(block)

                         xscr <<- ttkscrollbar(block, orient="horizontal",
                                              command=function(...)tkxview(widget,...))
                         yscr <<- ttkscrollbar(block, 
                                              command=function(...)tkyview(widget,...))
                         tkconfigure(widget,
                                     xscrollcommand=function(...)tkset(xscr,...),
                                     yscrollcommand=function(...)tkset(yscr,...)
                                     )

                         ## pack into a grid
                         ## see tkFAQ 10.1 -- makes for automatic resizing
                         tkgrid(widget,row=0,column=0, sticky="news")
                         tkgrid(yscr,row=0,column=1, sticky="ns")
                         tkgrid(xscr, row=1, column=0, sticky="ew")
                         tkgrid.columnconfigure(block, 0, weight=1)
                         tkgrid.rowconfigure(block, 0, weight=1)
                         
                         if(!is_aqua()) {
                           tcl("autoscroll::autoscroll", xscr)
                           tcl("autoscroll::autoscroll", yscr)
                           tkgrid.propagate(block, FALSE)                        
                         }
                       },
                       has_selection=function() {
                         as.character(tclvalue(tktag.ranges(widget, "sel"))) != ""
                       },
                       get_value=function(drop=FALSE, ...) {
                         "Return text, or selected text if drop=TRUE. If index=TRUE, return index of text"
                         ## if drop=FALSE or NULL grab all text
                         ## if drop=TRUE, get selected text only
                         if(is.null(drop) || drop == FALSE) {
                           val <-  tclvalue(tkget(widget, "0.0", "end"))
                           ## strip off last "\n"'s
                           val <- gsub("\n*$","",val)
                         } else {
                           range <- as.numeric(tktag.ranges(widget, "sel"))
                           ## range is numeric(0) if none
                           if(length(range) > 0)
                             val <- tclvalue(tkget(getWidget(obj),"sel.first","sel.last"))
                           else
                             val <- ""
                         }
                         ## val = unlist(strsplit(val,"\n"))
                         return(val)
                       },
                       set_value=function(value, ...) {
                         "Replace all text, pasted together with newline"
                         value <- paste(value, collapse="\n")

                         tkdelete(widget,"0.0", "end") # clear old
                         tkinsert(widget, "end", value)
                         tksee(widget, "0.0")
                       },
                       get_index = function(...) {
                         "Return the index of the selected text"
                         ## get the selected text from gtext,
                         ## return the index instead of text.
                         if(has_selection())
                           ## row.column: row 1-based, column 0-based
                           val <- as.character(tktag.ranges(widget, "sel"))
                         else
                           val <- c(0,0)
                         return(as.numeric(val))
                       },
                       set_index = function(value,...) {
                         "set selection by range of rows"
                         start <- min(value)
                         end <- max(value)
                         ### XXX
                       },
                       get_items = function(i, j, ..., drop=TRUE) {
                         stop("Not defined")
                       },
                       set_items = function(value, i, j, ...) {
                         stop("Not defined")
                       },
                       insert_text=function(value,
                         where=c("end", "beginning", "at.cursor"),
                         font.attr=NULL,
                         do.newline=TRUE, ...) {

                         where <- match.arg(where)

                         where <- c(end="end", at.cursor="insert", beginning="0.0")[where]
                         
                         value <- paste(value,collapse="\n")
                         if(do.newline) {
                           message("insert new line")
                           value <- paste(value,"\n",sep="")
                         }
                         ## Handle markup here
                         if(!is.null(font.attr) && length(font.attr) > 0) {
                           ## bit of a hack to set font
                           fname <- paste(as.character(date()),rnorm(1), sep="") ## some random string
                           
                           fontList <- map_font_to_spec(font.attr, TRUE)
                           do.call("tkfont.create", merge_list(fname, fontList))                           
                           
                           tkmark.set(widget, "left","insert")
                           tkmark.gravity(widget,"left","left")
                           tkmark.set(widget, "right","insert")
                           tkmark.gravity(widget,"right","right")
                           tkinsert(widget, where, value)
                           tktag.add(widget, fname, "left","right")
                           tktag.configure(widget, fname, font=fname)
                           if("color" %in% names(font.attr))
                             tktag.configure(widget, fname, foreground=font.attr$color)
                         } else {
                           ## no markup
                           tkinsert(widget, where, value)
                         }
                         
                         ## does this place the cursor? TK FAQ 10.6
                         tksee(widget, "insert")

                       },
                       set_font=function(value) {
                         if(has_selection()) {
                           selected <- as.character(tktag.ranges(widget, "sel"))
                           fname <- paste(as.character(date()),rnorm(1), sep="") ## some random string

                           ## make font, tag in buffer, configure tag
                           fontList <- map_font_to_spec(value, TRUE)
                           do.call("tkfont.create", merge_list(fname, fontList))
                           tktag.add(widget, fname, selected[1], selected[2])
                           tktag.configure(widget, fname, font=fname)
                           if("color" %in% names(value))
                             tktag.configure(widget, fname, foreground=value$color)
                         } else {
                           ## clear out old tags -- we are resetting
                           tagNames <- as.character(tktag.names(widget))
                           sapply(tagNames, function(i) tktag.delete(widget, i))
                           ## set selection to entire buffer
                           tcl(widget, "tag", "add", "sel", "0.0", "end")
                           set_font(value)
                           tcl(widget, "tag", "remove", "sel", "0.0", "end")  ## clear selection
                         }
                       },
                       add_handler_changed=function(handler, action=NULL, ...) {
                         add_handler_keystroke(handler, action=action, ...)
                       },
                       add_handler_selection_changed=function(handler, action=NULL, ...) {
                         add_handler("<<Selection>>", handler, action)
                       },
                       undo=function() {
                         tcl(widget, "undo")
                       },
                       redo=function() {
                         tcl(widget, "redo")
                       },
                       can_undo=function() {
                         ## XXX no clue
                       },
                       can_redo=function() {
                         ## XXX no clue
                       }
                       ))


  
