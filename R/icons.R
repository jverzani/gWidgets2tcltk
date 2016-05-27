##' @include misc.R
NULL


TcltkStockIcons <- setRefClass("TcltkStockIcons",
                               fields=list(
                                 l="list"
                                 ),
                               methods=list(
                                 initialize=function(...) {
                                   initFields(l=list())
                                   callSuper(...)
                                 },
                                 load_gWidgets_icons=function() {
                                   dir <- system.file("images", package = "gWidgets")
                                   x <- list.files(dir, pattern="gif$", full.names=TRUE)
                                   nms <- basename(x)
                                   nms <- gsub("\\.gif$","",nms)
                                   add_icons(nms, x)
                                 },
                                 add_icons=function(nms, x) {
                                   for(i in seq_along(nms)) {
                                     nm <- nms[i]; f <- x[i]
                                     
                                     iconName <- paste("::stockicon::",nm, sep="")
                                     out <- try(tcl("image","create","photo",
                                                    iconName,
                                                    file=f), silent=TRUE)
                                     if(!inherits(out,"try-error")) {
                                       class(f) <- c("gWidgetstcltkIcon", class(f))
                                       l[[nm]] <<- f
                                     }
                                   }
                                 },
                                 has_icon=function(stockname) {

                                   stockname <- as.character(stockname)
                                   
                                   out <- is.null(stockname) || length(stockname) == 0 ||
                                          nchar(stockname) == 0 ||
                                          stockname == "" || is.null(l[[stockname, exact=TRUE]])
                                   !out
                                 },
                                 find_icon=function(stockname) {
                                   "REturn tcltk icon name"
                                   if(has_icon(stockname)) {
                                     val <- paste("::stockicon::", stockname, sep="")
                                     class(val) <- c("StockIcon", class(val))
                                   } else {
                                     val <- ""
                                   }
                                   return(val)
                                 },
                                 find_icon_file=function(stockname) {
                                   "Return icon file:"
                                   if(has_icon(stockname)) 
                                     l[[stockname, exact=TRUE]]
                                   else
                                     NULL
                                 },
                                 show=function(...) cat("icon object\n")
                                 ))


## package global to hold icons
tcltkStockIcons <- TcltkStockIcons$new()

findIcon <- function(stockname) tcltkStockIcons$find_icon(stockname)


##' add stock icons
##'
##' @export
##' @inheritParams gWidgets2::addStockIcons
##' @rdname gWidgets2tcltk-undocumented
##' @method .addStockIcons guiWidgetsToolkittcltk
.addStockIcons.guiWidgetsToolkittcltk <- function(toolkit, iconNames, iconFiles,... ) {
  tcltkStockIcons$add_icons(iconNames, iconFiles)
}

##' Returns list of stock ids
##'
##' @inheritParams gWidgets2::getStockIcons
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .getStockIcons guiWidgetsToolkittcltk
.getStockIcons.guiWidgetsToolkittcltk <- function(toolkit, ...) {
  tcltkStockIcons$l
}

##' return stock id
##'
##' @param name name of icon
##' @export
##' @usage \method{.getStockIconByName}{guiWidgetsToolkittcltk}(toolkit,name, ...)
##' @rdname gWidgets2tcltk-undocumented
##' @method .getStockIconByName guiWidgetsToolkittcltk
.getStockIconByName.guiWidgetsToolkittcltk <- function(toolkit, name, ...) {
  
  tcltkStockIcons$find_icon(name)
}


##################################################

##' return stock id from object
##'
##' @param obj obj to get icon from
##' @export
##' @usage  \method{.stockIconFromObject}{guiWidgetsToolkittcltk}(toolkit,obj, ...)
##' @rdname gWidgets2tcltk-undocumented
##' @method .stockIconFromObject guiWidgetsToolkittcltk
.stockIconFromObject.guiWidgetsToolkittcltk <- function(toolkit, obj, ...) {
  icon_for_object <- function(x) UseMethod("icon_for_object")
  icon_for_object.default <- function(x) "symbol_dot"
  icon_for_object.numeric <- function(x) "numeric"
  icon_for_object.numeric <- function(x) "numeric"
  icon_for_object.factor <- function(x) "factor"
  icon_for_object.character <- function(x) "character"
  icon_for_object.function <- function(x) "function"
  icon_for_object.data.frame <- function(x) "dataframe"
  
  icon_for_object(obj)
}
