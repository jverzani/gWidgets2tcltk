##' @include GContainer.R
NULL

##' toolkit constructor for ggroup
##'
##' @inheritParams gWidgets2::ggroup
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .ggroup guiWidgetsToolkittcltk
.ggroup.guiWidgetsToolkittcltk <- function(toolkit, horizontal=TRUE, spacing=5, use.scrollwindow=FALSE, container=NULL, ...) {
  GGroup$new(toolkit, horizontal, spacing=spacing, use.scrollwindow=use.scrollwindow, container, ...)
}

## TODO XXX raise on drag motion

## base class for box containers. 
GGroup <- setRefClass("GGroup",
                      contains="GBoxContainer",
                      fields=list(
                        use_scrollwindow="character"
                        ),
                      methods=list(
                        ## main intialize method
                        initialize=function(toolkit=NULL,
                          horizontal=TRUE, spacing=5,
                          use.scrollwindow=FALSE,
                          container=NULL, ...) {

                          initFields(horizontal=horizontal,
                                     use_scrollwindow=as.character(use.scrollwindow))

                          
                          if(use_scrollwindow != FALSE) {
                            init_scrollwindow(container)
                            tkconfigure(block, width=400, height=400)
                            tkgrid.propagate(block, "FALSE")
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

                          do_x <- horizontal #|| use_scrollwindow %in% c("TRUE", "x")
                          do_y <- !horizontal# || use_scrollwindow %in% c("TRUE", "y")

                          tkconfigure(cnv,
                                      xscrollcommand = function(...) tkset(xscr,...))
                          tkconfigure(cnv,
                                      yscrollcommand = function(...) tkset(yscr,...))
                          ##
                           ## see tkFAQ 10.1 -- makes for automatic resizing
                          tkgrid(cnv,row=0,column=0, sticky="news")
                          if(do_x)
                            tkgrid(xscr,row=1,column=0, sticky="ew")
                          if(do_y)
                            tkgrid(yscr,row=0,column=1, sticky="ns")

                          
                          tkgrid.columnconfigure(cnv, 0, weight=1)
                          tkgrid.rowconfigure(cnv, 0, weight=1)

                          ##
                          widget <<- ttkframe(cnv)
                          widgetID <- tcl(cnv,"create","window",0,0,anchor="nw",window=widget)

                          ## initial mapping
                          tkbind(block, "<Map>", function() {
                            width <- tkwinfo("width", block)
                            height <- tkwinfo("height", block)
                            if(do_x) tkconfigure(cnv, width=width)
                            if(do_y) tkconfigure(cnv, height=height)
                          })

                          ## coordinate change of heights bewteen block and canvas
                          tkbind(block, "<Configure>", function(W) {
                            width <- as.numeric(tkwinfo("width", W))
                            height <- as.numeric(tkwinfo("height", W))
                            
                            if(do_x) {
                              scroll_size <- as.numeric(tkwinfo("height", xscr))
                              tkconfigure(cnv, height=height - scroll_size, width=width)
                              tkitemconfigure(cnv, widgetID, height=height - scroll_size - 2)
                            }
                            if(do_y) {
                              scroll_size <- as.numeric(tkwinfo("width", yscr))                              
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
