### tk2tip.R - Tooltips for Tk widgets
### Copyright (c), Philippe Grosjean (phgrosjean@sciviews.org)
### Licensed under LGPL 3 or above
###
### Changes:
### - 2007-01-01: first version (for tcltk2_1.0-0)
###
### To do:
### - add and check catch instructions here

## JV: Rather than load in tcltk2 dependency, we borrow Philippe's work here
tk2tip <- function (widget, msg) {
    ##	if (!is.tk()) stop("Package Tk is required but not loaded")
    if (is.null(msg)) msg <- ""
    
    ## Hack for negative numbers
    if(grepl("^-", msg))
        msg <- sprintf("(%s)", substr(msg, 2, nchar(msg)))
    
    res <- tclRequire("tooltip")
	if (inherits(res, "tclObj")) {
            res <- tcl("tooltip::tooltip", widget, msg)
            ## Store tip text in the object (use NULL instead of "" for no tip)
            if (msg == "") msg <- NULL
            widget$env$tip <- msg
	} else stop("cannot find tcl package 'tooltip'")
    return(invisible(res))
}

tk2killtip <- function ()
{
  ##if (!is.tk()) stop("Package Tk is required but not loaded")
	return(invisible(tcl("tooltip::hide")))
}

## Get tip method
tip <- function (x, ...)
	UseMethod("tip")

tip.tk2widget <- function (x, ...)
	return(x$env$tip)

## Chenge tip method
`tip<-` <- function (x, value)
	UseMethod("tip<-")

`tip<-.tk2widget` <- function (x, value)
{
	tk2tip(x, value)
	return(x)
}
