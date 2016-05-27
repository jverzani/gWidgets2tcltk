##' @include GWidget.R
NULL

##' S3 method for gtimer
##'
##' @inheritParams gWidgets2::gtimer
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gtimer guiWidgetsToolkittcltk
.gtimer.guiWidgetsToolkittcltk <- function(toolkit, ms, FUN, data=NULL, one.shot=FALSE, start=TRUE)
  GTimer$new(toolkit, ms, FUN, data=data, one.shot=one.shot, start=start)

##' Timer class for gWidgets.
##'
##' The main reference methods \code{GTimer} are \code{start_timer} and \code{stop_timer}
##' @rdname gWidgets2tcltk-package
GTimer <- setRefClass("GTimer",
                      fields=list(
                        "again"="logical",
                        interval="integer",
                        data="ANY",
                        FUN="ANY",
                        FUN_wrapper="ANY",
                        ID = "ANY"
                        ),
                      methods=list(
                        initialize=function(toolkit=guiToolkit(), ms, FUN=function(...) {},
                          data=NULL,
                          one.shot=FALSE, start=TRUE) {

                          
                          f <- function() {
                            FUN(data)
                            if(again) {
                              start_timer()
                            } else {
                              stop_timer()
                            }
                          }

                          initFields(interval=as.integer(ms),
                                     again=!one.shot,
                                     data=data,
                                     FUN=FUN,
                                     FUN_wrapper=f
                                     )

                          ID <<- ""
                          
                          if(start) 
                            start_timer()
                          
                          callSuper()
                        },
                        ## Main interface for gtimer:
                        set_interval=function(ms) {
                          "Set the interval. Need to stop and start active timer to implement."
                          interval <<- as.integer(ms)
                        },
                        start_timer = function() {
                          "Start the timer"
                          again <<- TRUE
                          ID <<- tcl("after", interval, FUN_wrapper)
                        },
                        stop_timer = function() {
                          "stop the timer"
                          again <<- FALSE
                          tcl("after", "cancel", ID)
                        }
                        ))
