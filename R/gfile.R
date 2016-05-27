##' @include GWidget.R
NULL

##' Toolkit implementation
##'
##' @inheritParams gWidgets2::gfile
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gfile guiWidgetsToolkittcltk
.gfile.guiWidgetsToolkittcltk <- function(toolkit,
                                          text = "",
                                          type = c("open","save","selectdir"),
                                          initial.filename = NULL,
                                          initial.dir = getwd(),
                                          filter = list(),
                                          multi=FALSE,
                                          ...) {
  ## make dialog, return character class object (character(0) if no selectino)
              
  args = list(...)

  if(is.null(initial.dir))
    initia.ldir <- getOption("gWidgetstcltk::gfile_initialdir")
  ## may still be NULL that is okay
  options("gWidgetstcltk::gfile_initialdir"=initial.dir) # store
            
  type <- match.arg(type)

  ## different things depending on type
  if(type == "open") {
    ## the filter is a named list of patters, possibly more than one
    ## fb <- gfilebrowse(cont=g, filter=list(
    ##                         "R"=list(patterns=c(".R", ".Rdata")),
    ##                         "images"=list(patterns=c(".gif", ".jpg", ".jpeg", ".png")),
    ##                         "data"=list(patterns=c(".csv", ".txt", ".dcf", ".fwf")),
    ##                         "All files"=list(patterns="*")
    ##                         ))
    ## The mimetypes components are ignored in gWidgets2tcltk
    
    l <- list(title=text,  multiple=multi)

    if(is.character(filter)) {
      the_filter <- sapply(names(filter), function(nm) {
        list(patterns=paste(".", filter[nm], sep=""))
      }, simplify=FALSE)
      the_filter[['All files']]$patterns = "*"
    } else {
      the_filter <- Filter(function(i) !is.null(i$patterns), filter)
    }

    if(length(the_filter)) {
      l$filetypes <- paste(unlist(
                                  Map(function(nm, patt) sprintf("{{%s} {%s}}", nm, patt),
                                      names(the_filter), lapply(the_filter, function(i) i$pattern))
                                  ),
                           sep=" ", collapse=" ")
     l$filetypes <- gsub("[{][*][}]", "*", l$filetypes)
    }

    if(!is.null(initial.filename))
      l$initialfile=initial.filename
    if(!is.null(initial.dir))
      l$initialdir=initial.dir
    
    val <- do.call("tkgetOpenFile", l)
    
    if(multi) {
      val <- as.character(val) # empty = character(0)
      if(length(val) == 0)
        val <- NA
    } else {
      val <- tclvalue(val)    # empty=""
      if(val == "")
        val <- character(0)
    }
    
    ## save initialdir information
    if(!is.na(val[1]) && nchar(val[1]) > 0)
      options("gWidgetstcltk::gfile_initialdir"=dirname(val[1]))
  } else if(type == "save") {
    
    l <- list(title=text)
    l$initialfile <- initial.filename
    val <- do.call(tkgetSaveFile, l)
    val <- tclvalue(val)
    
  } else if(type == "selectdir") {
    
    val <- tkchooseDirectory()
    val <- tclvalue(val)
    
  }
  
  
  if(length(val) == 0) {
    return(character(0))
  } else if (length(val) > 1 || nchar(val) > 0) {
    ## how to return filename?
    return(val)
  } else {
    ## cancel
    return(character(0))
  }
  
  
}


##' Toolkit constructor
##'
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gfilebrowse guiWidgetsToolkittcltk
.gfilebrowse.guiWidgetsToolkittcltk <-  function(toolkit,
                                                 text = "",
                                                 type = c("open","save","selectdir"),
                                                 initial.filename = NULL,
                                                 initial.dir = getwd(),
                                                 filter = list(),
                                                 quote=TRUE,
                                                 handler=NULL,
                                                 action=NULL,
                                                 container = NULL,
                                                 ... ) {
  GFileBrowse$new(toolkit,
            text=text, type=type, initial.filename=initial.filename, initial.dir = initial.dir,
            filter=filter, quote=quote, handler=handler, action=action, container=container, ...)
}


## XXX
GFileBrowse <- setRefClass("GFileBrowse",
                           contains="GWidget",
                           fields=list(
                             t_var="ANY",
                             text="ANY",
                             type="ANY",
                             initial.filename="ANY",
                             initial.dir = "ANY",
                             filter="ANY",
                             quote="ANY"
                             ),
                           methods=list(
                              initialize=function(
                                toolkit=NULL,
                                text = "",
                                type = c("open", "save", "selectdir"),
                                initial.filename = NULL,
                                initial.dir = getwd(),
                                filter = list(),
                                quote=TRUE,
                                handler=NULL,
                                action=NULL,
                                container = NULL,
                                ... ) {


                                block <<- ttkframe(container$get_widget())
                                t_var <<- tclVar("")
                                widget <<- ttkentry(block, textvariable=t_var)
                                btn <- ttkbutton(block, text="file", image=getStockIconByName("file"), command=.self$popup_dialog)

                                tkpack(widget, side="left", expand=TRUE, fill="both")
                                tkpack(btn, side="left")

                                initFields(change_signal="<<Changed>>",
                                           text=text,
                                           type=type,
                                           initial.filename=initial.filename,
                                           initial.dir = initial.dir,
                                           filter=filter,
                                           quote=quote)

                                
                                add_to_parent(container, .self, ...)
                                handler_id <<- add_handler_changed(handler, action)
                                callSuper(toolkit)
                              },
                             popup_dialog=function() {
                               ret <- gfile(text=text, type=type,
                                            initial.filename=initial.filename, initial.dir = initial.dir,
                                            filter=filter, toolkit=toolkit)
                               if(length(ret))
                                 set_value(ret)
                             },
                             get_value=function( ...) {
                               x <- as.character(tclvalue(t_var))
                               Encoding(x) <- "UTF-8"
                               x
                             },
                             set_value=function(value, ...) {
                               ## should we check file.exists?
                               tclvalue(t_var) <<- value
                               invisible(notify_observers(signal=change_signal))
                             }
                             ))

