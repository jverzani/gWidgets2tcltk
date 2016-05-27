##' @include ggroup.R
NULL

##' gframe constructor
##'
##' @inheritParams gWidgets2::gframe
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gframe guiWidgetsToolkittcltk
.gframe.guiWidgetsToolkittcltk <- function(toolkit, text, markup, pos, horizontal=TRUE, spacing=5, container=NULL, ...) {
  GFrame$new(toolkit, text, markup, pos, horizontal, spacing, container, ...)
}

## base class for gframe
GFrame <- setRefClass("GFrame",
                      contains="GBoxContainer",
                      fields=list(
                        markup="logical"
                        ),
                      methods=list(
                        initialize=function(toolkit=NULL, text="", markup=FALSE, pos=3, horizontal=TRUE, spacing=5, container=NULL, ..., use.scrollwindow=FALSE) {

                          initFields(horizontal=horizontal)
                          set_spacing(spacing)
                          
                          if(markup) {
                            message(gettext("HTML markup not supported for title. \n"))
                            text <- strip_markup(text)    # strip off HTML
                          }
                           ## where to put
                          labAnchor <- c("nw", "n", "ne", "ne")[as.integer(1 + 3*pos)]

                          widget <<- tkwidget(container$get_widget(),
                                             "ttk::labelframe",
                                             text=text,
                                             labelanchor=labAnchor)
                          block <<- widget
                          
                          add_to_parent(container, .self, ...)
                          
                          callSuper(toolkit, horizontal=horizontal, ...)
                        },
                        strip_markup=function(string) {
                          gsub("<[^>]*>","", string)  
                        },
                        get_names=function(...) {
                          as.character(tkcget(widget, "-text"))
                        },
                        set_names=function(value, ...) {
                          tkconfigure(widget, text=strip_markup(value))
                        }
                        ))
                        
