##' @include icons.R
NULL

.onLoad <- function(libname,pkgname,...) {
  ## methods isn't loaded yet, so we try calling through :::
  oldClasses <- c("tkwin", "tclVar", "tclObj")
  methods:::setClass("tcltkObject")
  lapply(oldClasses, function(i) {
    methods:::setOldClass(i)
    methods:::setIs(i,"tcltkObject")
  })


  
}
         


.onAttach <- function(...) {
  ## version check
  if(as.numeric(.Tcl("info tclversion")) < 8.5) {
    packageStartupMessage("\n\n *** gWidgetstcltk needs tcl/tk version 8.5 or newer ***\n\n")
  }
  
  ## some configuration
  .Tcl("option add *tearOff 0")         # disable tearoff menus



  ## read in tklibs (from tcltk2 pacakge)
  addTclPath(system.file("tklibs", package="gWidgets2tcltk"))
  tclRequire("tooltip")
  tclRequire("autoscroll")

  ## Icons
  tcltkStockIcons$load_gWidgets_icons()
  ## images from http://ryanfait.com/resources/custom-checkboxes-and-radio-buttons/. Thanks
  tkimage.create("photo", "::image::off", file=system.file("images", "checkbutton-off.gif", package="gWidgetstcltk"))
  tkimage.create("photo", "::image::on",  file=system.file("images", "checkbutton-on.gif",  package="gWidgetstcltk"))
}
