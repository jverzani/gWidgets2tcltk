## miscellaneous functions
##' @import gWidgets2
##' @include gWidgets2tcltk-package.R
##' @include List.R
NULL

##' toolkit class for tlctk
##'
##' @name guiWidgetsToolkittcltk-class
##' @export
setClass("guiWidgetsToolkittcltk",
         contains="guiWidgetsToolkit")


using_Mac <- function() as.character(tcl("tk", "windowingsystem")) == "aqua"
