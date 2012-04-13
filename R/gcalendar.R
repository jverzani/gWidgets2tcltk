##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gcalendar
##' @export
##' @rdname gWidgets2tcltk-undocumented
##' @method .gcalendar guiWidgetsToolkittcltk
##' @S3method .gcalendar guiWidgetsToolkittcltk
.gcalendar.guiWidgetsToolkittcltk <-  function(toolkit,
                                               text="",
                                               format="%Y-%m-%d",
                                               handler = NULL,action = NULL, container = NULL, ... ) {
  GCalendar$new(toolkit,
                 text=text,
                format=format,
                handler = handler,action = action, container = container, ...)
}
  


## Calendar
GCalendar <- setRefClass("GCalendar",
                         contains="GWidget",
                         fields=list(
                           "format"="character",
                           date_var="ANY"
                           ),
                         methods=list(
                           initialize=function(toolkit=NULL,
                             text="",
                             format="%Y-%m-%d",
                             handler, action, container, ...) {

                             block <<- ttkframe(container$get_widget())
                             date_var <<- tclVar("")
                             widget <<- ttkentry(block, textvariable=date_var)
                             id <- getStockIconByName("date")
                             button <- ttkbutton(block, text="date",
                                                 image=id, compound="image",
                                                 command=.self$popup_date
                                                 )

                             
                             tkpack(widget, expand=TRUE, fill="x", side="left")
                             tkpack(button, side="left")

                             
                             initFields(format=format,
                                        change_signal="<<Changed>>"
                                        )
                             set_value(text)

                             add_to_parent(container, .self, ...)
                             handler_id <<- add_handler_changed(handler, action)
                             callSuper(toolkit)
                           },
                           popup_date=function() {
                             cur_date <- get_value()
                             if(is.na(cur_date))
                               cur_date <- Sys.Date()
                             makeCalendar(date_var, widget, cur_date, format)
                           },
                           get_value=function(drop=TRUE, ...) {
                             val <- as.character(tclvalue(date_var))

                             cur_date <- try(as.Date(val, format="%Y-%m-%d"))
                             if(is.na(cur_date)) 
                               cur_date <- as.Date(NA)
                             if(missing(drop) || is.null(drop) || drop)
                               format(cur_date, format=format)
                             else
                               cur_date
                           },
                           set_value=function(value, ...) {
                             if(value == "") return()
                             d <- as.Date(value, format=format)
                             tclvalue(date_var) <<- format(d)
                             invoke_change_handler()
                           }
                           ))

## helper
makeCalendar <- function(date_var, widget, date, date_format="%Y-%m-%d") {

  if(missing(date))
    date <- Sys.Date()

  date <- as.Date(date)
  
  year <- as.POSIXlt(date)$year + 1900
  month <- as.POSIXlt(date)$mon + 1

  ##

  
  toplevel <- tktoplevel()
  tktitle(toplevel) <-  gettext("Select a day")

  ## make transient to widget
  xpos <- as.numeric(tkwinfo("rootx", widget))
  ypos <- as.numeric(tkwinfo("rooty", widget))
  tkwm.geometry(toplevel,paste("+",xpos+30,"+",ypos+30,sep="")) # shift

  tkwm.transient(toplevel, widget) # set transient
  tkbind(widget, "<Destroy>",function(...) tkdestroy(toplevel))
  tkbind(widget, "<Unmap>",function(...) tkdestroy(toplevel))

  ##
  f <- ttkframe(toplevel, padding=c(3,3,12,12))
  tkpack(f, expand=TRUE, fill="both", side="top")
  cframe <- ttkframe(f)
  calframe <- ttkframe(f)
  tkpack(cframe, fill="x", side="top")
  tkpack(calframe, expand=TRUE, anchor="n")


  year <- year; month <- month          # function local

  
  ##' from chron with slight change to arguments
  day.of.week <- function (year, month, day) {
    ix <- year + trunc((month - 14)/12)
    jx <- (trunc((13 * (month + 10 - (month + 10)%/%13 * 12) - 
                  1)/5) + day + 77 + (5 * (ix - (ix%/%100) * 100))%/%4 + 
           ix%/%400 - (ix%/%100) * 2)
    jx%%7
  }
  
  
  ## is this a valid date
  validDate <- function(year, month, day) 
    !is.na(as.Date(sprintf("%s-%s-%s", year, month, day), format="%Y-%m-%d"))
  
  ## how many days in a month
  days.in.month <- function(year, month) {
    for(i in c(31, 30, 29, 28)) {
      if(validDate(year, month, i))
        return(i)
    }
  }
  ## 0-based week of month
  week.of.month <- function(year, month, day) {
    first.day <- day.of.week(year, month, 1)
    (first.day + day - 1) %/% 7
  }
  
  makeMonth <- function(w, year, month) {
    ## add headers
    days <- c("S","M","T","W","Th","F","S")
    lapply(1:7, function(i) {
      l <- ttklabel(w, text=days[i])           # color
      tkgrid(l, row=0, column=i-1, sticky="")
    })
    ## add days
    lapply(1:days.in.month(year, month),  function(day) {
      l <- ttklabel(w, text=day)

      ## bind to each day
      ## might be more efficient to bind to toplevel and intercept
      tkbind(l, "<Button-1>", function(W) {
        day <- sprintf("%s-%s-%s", year, month, tclvalue(tkcget(W,"-text")))
        
        date <- as.Date(day, "%Y-%m-%d")
        date <- format(date, format=date_format)
        
        tclvalue(date_var) <- date
        tkdestroy(toplevel)
      })

      
      tkgrid(l, row=1 + week.of.month(year, month, day),
             column=day.of.week(year, month, day),
             sticky="e")
    })
  }

  ## controls
  prevy <- ttklabel(cframe, text="<<")
  prevm <- ttklabel(cframe, text="<")
  nextm <- ttklabel(cframe, text=">")
  nexty <- ttklabel(cframe, text=">>")
  curmo <- ttklabel(cframe)

  tkpack(prevy, side="left", anchor="w", padx=2)  
  tkpack(prevm, side="left", anchor="w")
  tkpack(curmo, side="left", anchor="center", expand=TRUE, padx=2)
  tkpack(nextm, side="left", anchor="e")
  tkpack(nexty, side="left", anchor="e", padx=2)

  
  setMonth <- function() {
    tclServiceMode(TRUE)
    sapply(as.character(tkwinfo("children", calframe)), tkpack.forget)
#    calframe <<- ttkframe(f); tkpack(calframe)
    makeMonth(calframe, year, month)
    tkconfigure(curmo, text=sprintf("%s %s", month.abb[month], year))
    tclServiceMode(TRUE)
  }
  setMonth()                              # initial calendar
  ## lock up size

  tkbind(prevy, "<Button-1>", function() {
    year <<- year - 1
    setMonth()
  })
  
  tkbind(prevm, "<Button-1>", function() {
    if(month > 1) {
      month <<- month - 1
    } else {
      month <<- 12; year <<- year - 1
    }
    setMonth()
  })
  
  tkbind(nextm, "<Button-1>", function() {
    if(month < 12) {
      month <<- month + 1
  } else {
    month <<- 1; year <<- year + 1
  }
    setMonth()
  })
  
  tkbind(nexty, "<Button-1>", function() {
    year <<- year + 1
    setMonth()
  })

}

