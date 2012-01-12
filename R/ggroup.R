##' @include GContainer.R
NULL

##' toolkit constructor for ggroup
##'
##' @inheritParams gWidgets2::ggroup
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .ggroup guiWidgetsToolkittcltk
##' @S3method .ggroup guiWidgetsToolkittcltk
.ggroup.guiWidgetsToolkittcltk <- function(toolkit, horizontal=TRUE, spacing=5, use.scrollwindow=FALSE, container=NULL, ...) {
  GGroup$new(toolkit, horizontal, spacing=spacing, use.scrollwindow=use.scrollwindow, container, ...)
}

## TODO XXX raise on drag motion

## base class for box containers. 
GGroup <- setRefClass("GGroup",
                      contains="GBoxContainer",
                      fields=list(
                        use_scrollwindow="logical"
                        ),
                      methods=list(
                        ## main intialize method
                        initialize=function(toolkit=NULL,
                          horizontal=TRUE, spacing=5,
                          use.scrollwindow=FALSE,
                          container=NULL, ...) {

                          if(use.scrollwindow) {
                            init_scrollwindow(container)
                          } else {
                            widget <<- ttkframe(container$get_widget())
                            block <<- widget
                          }
                          initFields(horizontal=horizontal,
                                     use_scrollwindow=use.scrollwindow)
                          set_spacing(spacing)

                          add_to_parent(container, .self, ...)
                          
                          callSuper(toolkit)
                        },
                        init_scrollwindow=function(container) {
                          XXX("Work to do on use.scrollwindow")
                        }
                        ))
