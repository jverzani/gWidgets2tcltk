##' @include GComponent.R
NULL

##' GWidget is the Base class for widget objects
##' @rdname gWidgets2tcltk-package
GWidget <- setRefClass("GWidget",
                       contains="GComponentObservable",
                       fields=list(
                         coerce_with="FunctionOrNULL"
                         ), 
                       methods=list(
                         initialize=function(..., coerce.with=NULL) {
                           if(is.null(coerce_with) && !is.null(coerce.with))
                             set_coerce_with(coerce.with)
                           callSuper(...)
                         },
                         set_coerce_with=function(coerce.with) {
                           if(is.character(coerce.with))
                             coerce.with <- get(coerce.with)
                           coerce_with <<- coerce.with
                         },
                         coerce_value=function(value) {
                           if(is.function(coerce_with))
                             coerce_with(value)
                           else
                             value
                         }
                       ))

                                   
##' GWidgetWithItems is Base class for selection widgets based on a set of items. The key
##' here is the handlers apply to each item, but the handler is
##' assigned to the class member.
##' @rdname gWidgets2tcltk-package
GWidgetWithItems <- setRefClass("GWidgetWithItems",
                                contains="GWidget",
                                fields=list(
                                  widgets="list",
                                  horizontal="logical",
                                  state_var="ANY" #tclVar
                                  ),
                                methods=list(
                                  connect_to_toolkit_signal=function(signal,f, ...) {
                                    ## override, done when adding items
                                  },
                                  set_enabled=function(value) {
                                    ## apply to each widget
                                    sapply(widgets, tkconfigure, state=ifelse(value, "normal", "disabled"))
                                  },
                                  get_enabled=function(...) {
                                    ## check first
                                    as.character(tkcget(widgets[[1]], "-state")) == "normal"
                                  }
                                ))


##
GWidgetWithTclVariable <- setRefClass("GWidgetWithTclVariable",
                                      contains="GWidget",
                                      fields=list(
                                        t_var="ANY"
                                        ),
                                      methods=list(
                                        ## initialize in subclass
                                        get_value=function(...) {
                                          coerce_value(tclvalue(t_var))
                                        },
                                        set_value=function(value, ...) {
                                          a <- t_var
                                          tclvalue(a) <- value
                                        }
                                        ))
                                        


