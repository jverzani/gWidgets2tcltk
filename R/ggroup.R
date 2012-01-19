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

                          initFields(horizontal=horizontal,
                                     use_scrollwindow=use.scrollwindow)

                          if(use.scrollwindow) {
                            init_scrollwindow(container)
                            tkconfigure(block, width=400, height=400)
                            tkpack.propagate(block, "FALSE")
                          } else {
                            widget <<- ttkframe(getWidget(container))
                            block <<- widget
                          }
                          set_spacing(spacing)

                          add_to_parent(container, .self, ...)
                          
                          callSuper(toolkit)
                        },
                        init_scrollwindow=function(container) {
                          block <<- ttkframe(container$get_widget())
                          cnv <- tkcanvas(block)

                          ## pack cnv with scrollbars
                           xscr <- ttkscrollbar(block, orient="horizontal",
                                   command=function(...)tkxview(cnv,...))
                           yscr <- ttkscrollbar(block, 
                                     command=function(...)tkyview(cnv,...))

                          if(horizontal) 
                            tkconfigure(cnv,
                                        xscrollcommand = function(...) tkset(xscr,...))
                          else
                            tkconfigure(cnv,
                                        yscrollcommand = function(...) tkset(yscr,...))
                          ##
                           ## see tkFAQ 10.1 -- makes for automatic resizing
                          tkgrid(cnv,row=0,column=0, sticky="news")
                          if(horizontal)
                            tkgrid(xscr,row=1,column=0, sticky="ew")
                          else
                            tkgrid(yscr,row=0,column=1, sticky="ns")

                          
                          tkgrid.columnconfigure(cnv, 0, weight=1)
                          tkgrid.rowconfigure(cnv, 0, weight=1)

                          ##
                          widget <<- ttkframe(cnv)
                          widgetID <- tcl(cnv,"create","window",0,0,anchor="nw",window=widget)

                          tkbind(block, "<Map>", function() {
                            if(horizontal) {
                              width <- tkwinfo("width", block)
                              tkconfigure(cnv, width=width)
                            } else {
                              height <- tkwinfo("height", block)
                              tkconfigure(cnv, height=height)
                            }
                          })

                          ## coordinate change of heights
                          tkbind(block, "<Configure>", function(W) {
                            width <- as.numeric(tkwinfo("width", W))
                            height <- as.numeric(tkwinfo("height", W))
                            
                            cnvwidth <- as.numeric(tkwinfo("width", cnv))
                            cnvheight <- as.numeric(tkwinfo("height", cnv))

                            widgetwidth <- as.numeric(tkwinfo("width", widget))
                            widgetheight <- as.numeric(tkwinfo("height", widget))
                            
                            if(horizontal)
                              scroll_size <- as.numeric(tkwinfo("height", xscr))
                            else
                              scroll_size <- as.numeric(tkwinfo("width", yscr))

                            if(horizontal) {
                              tkconfigure(cnv, height=height - scroll_size, width=width)
                              tkitemconfigure(cnv, widgetID, height=height - scroll_size - 2)
                            } else {
                              tkconfigure(cnv, height=height, width=width - scroll_size)
                              tkitemconfigure(cnv, widgetID, width=width - scroll_size - 2)
                            }
                          })

                          
                          tkbind(widget,"<Configure>",function() {
                            bbox <- tcl(cnv,"bbox","all")
                            tcl(cnv,"config",scrollregion=bbox)
                          })
                          
                          
                        }
                        ))
