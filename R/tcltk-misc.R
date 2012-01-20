##' @include misc.R
NULL

## Basic functions for tcltk objects

## return tk object
getWidget <- function(obj) {
  if(is(obj, "GComponent"))
    obj$get_widget()
  else
    obj
}

getBlock <- function(obj) {
  if(is(obj, "GComponent"))
    obj$get_block()
  else
    obj
}

## some package icons, fonts, colors




## CONSTANTS
widthOfChar <- ceiling(as.numeric(tclvalue(tcl("font","measure","TkTextFont","0"))))
heightOfChar <-  as.numeric(as.character(tcl("font","metrics","TkTextFont"))[6])
xyToAnchor = function(anchor) {
  m = rbind(
    c("nw","n","ne"),
    c("w","center","e"),
    c("sw","s","se")
    )
  anchor = m[2 - anchor[2],2 + anchor[1]]
  return(anchor)
}

is_aqua <- function() {
  as.character(tcl("tk","windowingsystem")) == "aqua"
}
